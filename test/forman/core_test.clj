(ns forman.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as s]
            [forman.core :refer [parse-range-timestamps parse-relative-range group-non-range-timestamps execute-files-cut print-script parse-section]]))

(defn check-parsed-range [parsed start end]
  (and (= (:start parsed) start) (= (:end parsed) end)))

(deftest parse-section-test
  (testing "should parse"
    (is (= (parse-section "asdasd[something]") "something"))
    (is (= (parse-section "second [me]") "me"))
    (is (= (parse-section "asdasd[me and you] blablabla") "me and you"))
    (is (= (parse-section "asdasd[only me] [other]") "only me"))
    (is (= (parse-section "1-2|[only me] [other]") "only me")))
  (testing "should not parse"
    (is (= "" (parse-section "asdasdasd")))
    (is (= "" (parse-section "")))
    (is (= "" (parse-section "1-2 [asdasd")))
    (is (= "" (parse-section "|as")))))

(deftest parse-range-timestamps-test
  (testing "should parse range with start end"
    (is (check-parsed-range (parse-range-timestamps "0-2" 100) 100 102))
    (is (check-parsed-range (parse-range-timestamps "0-2|aaa" 100) 100 102))
    (is (check-parsed-range (parse-range-timestamps "2|aaa" 100) 100 102))
    (is (check-parsed-range (parse-range-timestamps "0" 100) 100 100))
    (is (check-parsed-range (parse-range-timestamps "20|aaa" 100) 100 120))
    (is (check-parsed-range (parse-range-timestamps "20" 100) 100 120))
    (is (check-parsed-range (parse-range-timestamps "-12|aaa" 100) 88 100))
    (is (check-parsed-range (parse-range-timestamps "-12" 100) 88 100))
    (is (check-parsed-range (parse-range-timestamps "-12|" 100) 88 100))
    (is (check-parsed-range (parse-range-timestamps "15-20|aaa" 100) 85 120))
    (is (check-parsed-range (parse-range-timestamps "15-20   |aaa" 100) 85 120))
    (is (check-parsed-range (parse-range-timestamps "15-20" 100) 85 120))
    (is (check-parsed-range (parse-range-timestamps " 15-20 " 100) 85 120))
    (is (check-parsed-range (parse-range-timestamps "15-0|aaa" 100) 85 100))
    (is (check-parsed-range (parse-range-timestamps " 15-0 |aaa" 100) 85 100))
    (is (check-parsed-range (parse-range-timestamps "15-0|aaa" 100.12) 85.12 100.12))
    (is (check-parsed-range (parse-range-timestamps "0-0| sdfsd aaa" 100) 100 100)))
  (testing "should not parse range"
    (is (nil? (parse-range-timestamps "blablabl" 100)))
    (is (nil? (parse-range-timestamps "20-a|a" 100)))
    (is (nil? (parse-range-timestamps "20-" 100)))
    (is (nil? (parse-range-timestamps "a-" 100)))
    (is (nil? (parse-range-timestamps "a-20" 100)))
    (is (nil? (parse-range-timestamps "a-20| sd" 100)))
    (is (nil? (parse-range-timestamps "some title of the bookmark" 100)))
    (is (nil? (parse-range-timestamps "| blablabl" 100)))
    (is (nil? (parse-range-timestamps "" 100)))
    (is (nil? (parse-range-timestamps "|12-10| asdasd"  100)))))

(deftest parse-relative-range-test
  (testing "should parse with valid input"
    (is (= (parse-relative-range "10-20") {:start -10 :end 20}))
    (is (= (parse-relative-range "0-0") {:start 0 :end 0}))
    (is (= (parse-relative-range "-0") {:start 0 :end 0}))
    (is (= (parse-relative-range "2-1") {:start -2 :end 1}))
    (is (= (parse-relative-range "2-e") {:start -2 :end :absolute-end}))
    (is (= (parse-relative-range "s-1") {:start :absolute-start :end 1}))
    (is (= (parse-relative-range "-20") {:start -20 :end 0}))
    (is (= (parse-relative-range "20") {:start 0 :end 20})))
  (testing "should not parse with invalid input"
    (is (nil? (parse-relative-range "blabla")))
    (is (nil? (parse-relative-range "1a2")))
    (is (nil? (parse-relative-range "")))
    (is (nil? (parse-relative-range "a d")))
    (is (nil? (parse-relative-range "-d")))
    (is (nil? (parse-relative-range "-a1")))
    (is (nil? (parse-relative-range "10-")))))

(defn mock-timestamps [ts-array]
  (map-indexed (fn [idx, mock]
                 (if (= mock :n)
                   {:name (str "non ranged timestamp " idx)
                    :time (+ 50 (* 100 idx))}
                   {:name (str "ranged timestamp " idx)
                    :start (+ 50 (* 10 idx))
                    :end (+ 60 (* 10 idx))})) ts-array))

(defn test-grouping-timestamps [mock-def]
  (let [prepared-mocks (mock-timestamps mock-def)
        grouped-timestamps (into [] (eduction (group-non-range-timestamps) prepared-mocks))
        expected-count (reduce + (map #(if (= :r %) 1 1/2) mock-def))]
    (is (= expected-count (count grouped-timestamps)))
    (loop [mock-def mock-def
           actual-ts grouped-timestamps]
      (if-let [mock (first mock-def)]
        (let [actual (first actual-ts)]
          (if (= :r mock)
            (is (= 10 (- (:end actual) (:start actual))))
           ;; is non range timestamp
            (is (= 100 (- (:end actual) (:start actual)))))
          (recur (if (= :r mock) (rest mock-def) (rest (rest mock-def)))
                 (rest actual-ts)))))))

(deftest group-non-range-timestamps-test
  (testing "should group"
    (test-grouping-timestamps [])
    (test-grouping-timestamps [:r])
    (test-grouping-timestamps [:r :n :n])
    (test-grouping-timestamps [:r :n :n :r])
    (test-grouping-timestamps [:r :n :n :r :r :r])
    (test-grouping-timestamps [:n :n])
    (test-grouping-timestamps [:n :n :r :r])
    (test-grouping-timestamps [:n :n :n :n :n :n :r])
    (test-grouping-timestamps [:r :n :n :n :n])))

(defn valid-output-line? [output-line]
  (some #(re-matches % output-line) [#"#.+"
                                     #"file .+"
                                     #"^inpoint \d+(\.\d+)?$"
                                     #"^outpoint \d+(\.\d+)?$"]))

(defn is-valid-output [actual-file-content]
  (let [tmp-file (java.io.File/createTempFile "forman-test" ".m3u")
        tmp-file-path (.getAbsolutePath tmp-file)]
    (spit tmp-file-path actual-file-content)
    (let [actual-output (clojure.string/split-lines (with-out-str (execute-files-cut tmp-file-path print-script)))]
      (is (> (count actual-output) 1))
      (is (every? #(not (s/includes? % "nil")) actual-output))
      (is (every? valid-output-line? actual-output)))
    (.delete tmp-file)))

(deftest read-playlist-test
  (testing "should read input"
    (is-valid-output "#EXTM3U
#EXTINF:1062,GOPR1134.MP4
#EXTVLCOPT:bookmarks={name=zaciatok,time=121.371},{name=Untitled,time=164.384},{name=15-10|gol,time=212.446},{name=akcia,time=243.984},{name=po gole na 0:2,time=438.671},{name=Untitled,time=458.692},{name=koniec paru akci,time=490.458},{name=Untitled,time=499.247},{name=koniec akcie,time=514.247},{name=Untitled,time=530.263},{name=koniec akcii,time=560.513},{name=Untitled,time=568.065},{name=konec,time=589.567},{name=Untitled,time=592.842},{name=konec po mojom gole na 1:2,time=632.842},{name=Untitled,time=657.904},{name=Untitled,time=664.404},{name=Untitled,time=698.444},{name=po yogiho gole,time=708.455}
file:///Volumes/NO%20NAME/DCIM/100GOPRO/GOPR1134.MP4
")
    (is-valid-output "#EXTM3U
#EXTINF:894,GP041134.MP4
#EXTVLCOPT:bookmarks={name=10-1|bojan akcia,time=83.829},{name=7-1|lubo strela,time=103.846},{name=12-2|dano lubo strela,time=121.859},{name=15-1|akcia,time=161.893},{name=6-2|marcel akcia,time=205.942},{name=15-1|akcia,time=241.975},{name=20-2|lubo strela,time=316.056},{name=12-1|lubo ygi,time=366.603},{name=10-1|lukas gol,time=377.120},{name=5-2|marcel strela,time=404.152},{name=6-1|marcela akcia,time=442.185},{name=10-1|lukas gol,time=469.468},{name=12-2|bojan gol,time=568.813},{name=20-2|ja gol,time=630.371},{name=Untitled,time=685.933},{name=Untitled,time=716.964},{name=6-1|lubo strela,time=725.472},{name=20-1|boso akcia,time=762.998},{name=20-1|ja gol,time=813.545}
file:///Volumes/NO%20NAME/DCIM/100GOPRO/GP041134.MP4
#EXTINF:1534,result.mp4
#EXTVLCOPT:bookmarks={name=Untitled,time=430.000},{name=Untitled,time=434.000}
result.mp4
#EXTINF:1534,result3.mp4
result3.mp4
#EXTINF:1534,result.mp4
result.mp4
")
    (is-valid-output "#EXTM3U
#EXTINF:1534,result.mp4
#EXTVLCOPT:bookmarks={name=Untitled,time=430.000},{name=Untitled,time=434.000},{name=0-e|Untitled,time=1202.396},{name=s-0|Untitled,time=17.775}
result.mp4
#EXTINF:1534,result3.mp4
result3.mp4
#EXTINF:1534,result.mp4
result.mp4
")
    (is-valid-output "#EXTM3U
#EXTINF:271,test.mp4
#EXTVLCOPT:bookmarks={name=Untitled,time=8.258},{name=Untitled,time=34.282},{name=Untitled[me],time=61.560},{name=Untitled[me],time=79.327},{name=first [me],time=6.256},{name=second [me],time=29.778}
test.mp4
#EXTINF:4988,output.MP4
#EXTVLCOPT:bookmarks={name=start [me],time=136.386},{name=end [me],time=355.605},{name=5-10|Untitled,time=779.028}
file:///Users/tomaskacur/Movies/output.MP4")))
