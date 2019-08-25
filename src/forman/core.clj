(ns forman.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.tools.reader.edn :as edn]))

(defn lines-reducible
  [^java.io.BufferedReader rdr]
  (reify clojure.lang.IReduceInit
    (reduce [this f init]
      (try
        (loop [state init]
          (if (reduced? state)
            @state
            (if-let [line (.readLine rdr)]
              (recur (f state line))
              state)))
        (finally
          (.close rdr))))))

(defn keywordize [bookmarks-array]
  (map (fn [entry] (into {} (map (fn [[k v]] [(keyword k) v]) entry))) bookmarks-array))

(defn process-video-entry [[_ bookmarks-str file-str]]
  (let [[_ bookmarks-split-str] (clojure.string/split bookmarks-str (re-pattern "bookmarks="))
        bookmarks-replaced-time-str (clojure.string/replace bookmarks-split-str #",time=" "\",time ")
        bookmarks-replaced-name-str (clojure.string/replace bookmarks-replaced-time-str #"name=" "name \"")
        bookmarks-array-edn (str "[" bookmarks-replaced-name-str "]")
        bookmarks-array (keywordize (edn/read-string bookmarks-array-edn))
        bookmarks-array-sorted (sort-by :time bookmarks-array)]
    {:file (str "'" (java.net.URLDecoder/decode file-str) "'")
     :bookmarks bookmarks-array-sorted}))

;;; before-after || after || -before
(defn enhance-timestamps [{name :name time :time :as bookmark-entry}]
  (conj bookmark-entry
        (if-let [timestamps-str (first (clojure.string/split name #"\|"))]
          (if-let [matched (re-matches #"^(\d+)?\-?(\d+)?$" timestamps-str)]
            (let [[match1 match2 match3] matched]
              (if
               (and (some? match2) (some? match3))
                {:start (+ time (* -1 (Integer/parseInt match2)))
                 :end (+ time (Integer/parseInt match3))}
;;;else match only first number
                {:start (+ time (min 0 (Integer/parseInt match1)))
                 :end (+ time (max 0 (Integer/parseInt match1)))}))))))
(defn join-timestamps [previous next]
  {:start (:time previous)
   :end (:time next)
   :name (str (:name previous) "+" (:name next))})

(defn find-orphaned-timestamps [enhanced-timestamps]
  (second (reduce (fn [[previous-ts result] ts]
                    (if (some? (:start ts))
                      [nil (conj result ts)]
                      (if (some? previous-ts)
                        [nil (conj result (join-timestamps previous-ts ts))]
                        [ts result])))  [nil []] enhanced-timestamps)))

(defn make-file-timestamps [bookmarks-info]
  (let [enhanced-timestamps (map enhance-timestamps (:bookmarks bookmarks-info))
        missing-timestamps (find-orphaned-timestamps enhanced-timestamps)]
    (assoc bookmarks-info :timestamps missing-timestamps)))

(defn print-ffmpeg-one-file-format [{:keys [file timestamps]}]
  (doall (map (fn [{:keys [name start end]}]
                (println "#" name)
                (println (str "file " file))
                (println "inpoint " start)
                (println "outpoint" end)) timestamps)))

(defn read-playlist [path]
  (println "#ffmpeg -auto_convert 1 -f concat -safe 0 -i filescut -y -c copy -fflags +genpts -avoid_negative_ts make_zero result.mp4")
  (.println *err*  "ffmpeg -auto_convert 1 -f concat -safe 0 -i filescut -y -c copy -fflags +genpts -avoid_negative_ts make_zero result.mp4")
  (into []
        (comp (drop 1)
              (partition-all 3)
              (map process-video-entry)
              (map make-file-timestamps)
              (map print-ffmpeg-one-file-format))
        (lines-reducible (io/reader path))))

(defn -main [& args]
  (read-playlist (first args)))
