(ns forman.core
  (:gen-class)
  (:require
   [clojure.java.shell :refer [sh]]
   [clojure.string :as s]
   [clojure.tools.reader.edn :as edn]))

(defn printxf
  ([] (printxf identity))
  ([format-fn]
   (map (fn [item] (println (format-fn item)) item))))

(defn keywordize-maps-array [coll]
  (map (fn [item] (into {} (map (fn [[k v]] [(keyword k) v]) item))) coll))

(defn parse-section [bookmark-name]
  (or (if (some? bookmark-name)
        (if-let [matched (re-matches #"^[^\[]*\[([^\]]+)\].*$" bookmark-name)]
          (second matched)))
      ""))

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
      (map #(assoc % :section (parse-section (:name %))) x)
      (sort-by :time x)
      (sort-by :section x))))

(defn ensure-absolute-path [path playlist-file-path]
  (if (s/starts-with? path "file:/")
    (java.net.URLDecoder/decode path)
    (str (.getParent (.getAbsoluteFile (java.io.File. playlist-file-path))) "/" path)))

(defn playlist-entry->video-info [playlist-file-path escape-char]
  (fn [[extinf bookmarks-str file-str]]
    {:file  (str escape-char (ensure-absolute-path file-str playlist-file-path) escape-char)
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
   :section (:section previous)
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

(defn video-info-coll->concat-script [video-info-coll]
  (mapcat
   (fn [{:keys [file timestamps]}]
     (mapcat (fn [{:keys [name start end]}]
               [(str "#" name " ")
                (str (str "file " file))
                (str "inpoint " start)
                (str "outpoint " end)]) timestamps)) video-info-coll))

(defn group-to-playlistentry []
  (comp (partition-by #(s/starts-with? % "#EXTINF"))
        (partition-all 2)
        (map flatten)
        (map #(if (= 2 (count %)) (list (first %) "" (second %)) %))))

(defn group-by-sections []
  (comp
   (map #(update % :timestamps (partial group-by :section)))
   (map #(map (fn [section] (assoc % :timestamps (get-in % [:timestamps section]) :section section)) (keys (:timestamps %))))
   (map (partial sort-by :section))
   (map (partial group-by :section))))

(defn parse-playlist-into-sections [file-path escape-char]
  (transduce
   (comp (drop 1)
         (group-to-playlistentry)
         (map (playlist-entry->video-info file-path escape-char))
         (map video-info-bookmarks->timestamps)
         (map #(dissoc % :bookmarks))
         (group-by-sections))
   (partial merge-with concat)
   (s/split-lines (slurp file-path))))

(defn prepare-ffmpeg-command [input-file-part output-file-path]
  (let [first-part "-auto_convert 1 -f concat -safe 0 -i "
        second-part " -y -c copy -fflags +genpts -flags global_header -movflags faststart -avoid_negative_ts make_zero -loglevel error "
        args (str first-part input-file-part second-part output-file-path)]
    (str "ffmpeg " args)))

(defn prepare-output-path [section-name output-path]
  (if (empty? section-name) output-path
      (if-let [dot-idx (s/last-index-of output-path ".")]
        (str (subs output-path 0 dot-idx) "_" section-name (subs output-path dot-idx))
        (str output-path "_" section-name))))

(defn execute-ffmpeg-concat [output-path concat-script-lines section-name]
  (let [file-content (s/join \newline concat-script-lines)
        output-file-path (prepare-output-path section-name output-path)
        file-content-echo (str "<(echo '" file-content "')")
        command (prepare-ffmpeg-command file-content-echo output-file-path)]
    (println "creating" output-file-path "...")
    (println (sh "bash" "-c" command))))

(defn print-script [concat-script-lines section-name]
  (let [lines-str (s/join \newline concat-script-lines)]
    (if (not-empty section-name)
      (println "#### SECTION:" section-name))
    (println lines-str)))

(defn execute-files-cut [playlist-path execute-fn escape-char]
  (let [sections (parse-playlist-into-sections playlist-path escape-char)]
    (dorun (map (fn [[section-name video-info-coll]]
                  (execute-fn (video-info-coll->concat-script video-info-coll) section-name)) sections))))

(defn create-videos [playlist-path output-path]
  (execute-files-cut playlist-path
                     (partial execute-ffmpeg-concat output-path)
                     "'\"'\"'")
  (shutdown-agents))

(defn write-script-to-stdout [playlist-path]
  (let [ffmpeg-command (prepare-ffmpeg-command "inputfile" "result.mp4")]
    (println (str "#" ffmpeg-command))
    (.println *err*  ffmpeg-command)
    (execute-files-cut playlist-path print-script "'")))

(defn -main [& args]
  (if-let [playlist-path (first args)]
    (if (= (count args) 2)
      (create-videos playlist-path (second args))
      ;; else
      (write-script-to-stdout playlist-path))
    (println "usage: forman path-to-vlc-playlist [output-video-path]")))
