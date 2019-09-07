(ns forman.core
  (:gen-class)
  (:require
   [clojure.java.shell :refer [sh]]
   [clojure.string :as s]
   [clojure.tools.reader.edn :as edn]))

(defn keywordize-maps-array [coll]
  (map (fn [item] (into {} (map (fn [[k v]] [(keyword k) v]) item))) coll))

(defn parse-section [bookmark-name]
  (if (some? bookmark-name)
    (if-let [matched (re-matches #"^[^\[]*\[([^\]]+)\].*$" bookmark-name)]
      (second matched))))

(defn parse-bookmarks-array [bookmarks-str]
  (if (empty? bookmarks-str)
    []
    (as-> bookmarks-str x
      (s/split x #"bookmarks=")
      (second x)
      (s/replace x #",time=" "\",time ")
      (s/replace x #"name=" "name \"")
      (str "[" x "]")
      (edn/read-string x)
      (keywordize-maps-array x)
      (map #(update % :section (fn [entry] (parse-section (:name entry)))) x)
      (sort-by :time x)
      (sort-by :section x))))

(defn ensure-absolute-path [path playlist-file-path]
  (if (s/starts-with? path "file:/")
    (java.net.URLDecoder/decode path)
    (str (.getParent (.getAbsoluteFile (java.io.File. playlist-file-path))) "/" path)))

(defn playlist-entry->video-info [playlist-file-path]
  (fn [[extinf bookmarks-str file-str]]
    {:file  (str "'" (ensure-absolute-path file-str playlist-file-path) "'")
     :bookmarks (parse-bookmarks-array bookmarks-str)
     :duration (Integer/parseInt (subs (re-find #":\d+" extinf) 1))}))

(defn parse-int [number-string]
  (cond
    (= number-string "s") :absolute-start
    (= number-string "e") :absolute-end
    :else (try (Integer/parseInt number-string)
               (catch Exception e nil))))

(defn parse-relative-range [range-str]
  (let [matched (re-matches #"^(\d+|s)?\-?(\d+|e)?$" range-str)
        deltaRangeMatch (parse-int (first matched))
        rangeStart (parse-int (second matched))
        rangeEnd (parse-int (nth matched 2))]
    (if (some? matched)
      (if (and (some? rangeStart) (some? rangeEnd))
        {:start (if (= rangeStart :absolute-start) :absolute-start (* -1 rangeStart))
         :end rangeEnd}
        (if (some? deltaRangeMatch)
          {:start (min 0 deltaRangeMatch)
           :end (max 0 deltaRangeMatch)})))))

(defn parse-range-timestamps [input-str time]
  (some-> input-str
          (s/split #"\|")
          (first)
          (s/trim)
          (not-empty)
          (parse-relative-range)
          (update :start #(if (= % :absolute-start) :absolute-start (+ time %)))
          (update :end #(if (= % :absolute-end) :absolute-end (+ time %)))))

(defn collect-range-timestamps [{name :name time :time :as bookmark-entry}]
  (merge bookmark-entry (parse-range-timestamps name time)))

(defn join-timestamps [previous next]
  {:start (:time previous)
   :end (:time next)
   :name (str (:name previous) "+" (:name next))})

(defn is-ranged-timestamp [timestamp]
  (some? (:start timestamp)))

(defn group-non-range-timestamps []
  (comp (partition-by #(not (is-ranged-timestamp %)))
        (mapcat #(partition-all 2 %))
        (mapcat (fn [ts-pair]
                  (if (is-ranged-timestamp (first ts-pair))
                    ts-pair
                    [(join-timestamps (first ts-pair) (second ts-pair))])))))

(defn ignored-bookmark? [bookmark]
  (let [bookmark-str (:name bookmark)]
    (not (or (= "i" bookmark-str) (= "ignored" bookmark-str)))))

(defn prepare-timestamps [bookmarks-array video-duration]
  (eduction
   (comp
    (filter ignored-bookmark?)
    (map collect-range-timestamps)
    (group-non-range-timestamps)
    (map (fn [ts] (update ts :start #(if (= % :absolute-start) 0 %))))
    (map (fn [ts]  (update ts :end #(if (= % :absolute-end) video-duration %)))))
   bookmarks-array))

(defn video-info-bookmarks->timestamps [video-info]
  (assoc video-info
         :timestamps (prepare-timestamps (:bookmarks video-info) (:duration video-info))))

(defn video-info->ffmpeg-script
  ([{:keys [file timestamps]}]
   (mapcat (fn [{:keys [name start end]}]
             [(str "#" name)
              (str (str "file " file))
              (str "inpoint " start)
              (str "outpoint " end)]) timestamps)))

(defn print-result-lines
  ([])
  ([line] (if (some? line) (println line)))
  ([line1 line2] (print-result-lines line1) (print-result-lines line2)))

(defn group-to-playlistentry []
  (comp (partition-by #(s/starts-with? % "#EXTINF"))
        (partition-all 2)
        (map flatten)
        (map #(if (= 2 (count %)) (list (first %) "" (second %)) %))))

#_(defn group-by-sections []
    (comp (map #(update % :timestamps (partial group-by :section)))
          (map #(map (fn [section] (assoc % :timestamps (get-in % [:timestamps section]) :section section)) (keys (:timestamps %))))
          (map (partial sort-by :section))
          (map (partial group-by :section))))

(defn read-playlist [file-path apply-result-fn]
  (transduce
   (comp (drop 1)
         (group-to-playlistentry)
         (map (playlist-entry->video-info file-path))
         (map video-info-bookmarks->timestamps)
         (mapcat video-info->ffmpeg-script))
   apply-result-fn
   (s/split-lines (slurp file-path))))

(defn prepare-ffmpeg-command [input-file-part output-file-path]
  (let [first-part "-auto_convert 1 -f concat -safe 0 -i "
        second-part " -y -c copy -fflags +genpts -flags global_header -movflags faststart -avoid_negative_ts make_zero -loglevel panic "
        args (str first-part input-file-part second-part output-file-path)]
    (str "ffmpeg " args)))

(defn execute-ffmpeg-concat [input-file-path output-file-path]
  (let [input-lines (read-playlist input-file-path conj)
        file-content (s/join \newline input-lines)
        file-content-echo (str "<(echo '" file-content "')")
        command (prepare-ffmpeg-command file-content-echo output-file-path)]
    #_(println args)
    (println (:err (sh "bash" "-c" command)))
    (shutdown-agents)))

(defn -main [& args]
  (if (= (count args) 2)
    (execute-ffmpeg-concat (first args) (second args))
    (let [ffmpeg-command (prepare-ffmpeg-command "inputfile" "result.mp4")]
      (println (str "#" ffmpeg-command))
      (.println *err*  ffmpeg-command)
      (read-playlist (first args) print-result-lines))))
