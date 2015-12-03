(ns dc2015.core
  (:require [reaver :refer [parse extract-from text attr]]
            [clojure.data.json :as json]
            [incanter.processing :as ip]))

(use '(incanter core stats charts datasets excel))

;;
;; Utility function to parse html table
;;

(def parse-slurp (comp parse slurp))

(defn- read-hdrs [tbl data]
  (->> (extract-from data tbl [:x] "tr th" text)
       first vals first))

(defn- read-data [url]
  (let [d    (parse-slurp url)
        hdrs (read-hdrs "table" d)]
    (->> (extract-from d "table tr" [:x] "td" text)
         (map :x)
         (remove nil?)
         (map #(zipmap hdrs %)))))

;;
;; District name to code mapping
;;

(def district-map
  (let [d (slurp "http://www.census2011.gov.hk/en/constituency-area.html")
        cf (comp keyword clojure.string/trim #(clojure.string/replace % "&nbsp;" ""))]
    (->> (re-find #"selectoptions = (\{[\s\S]*\})" d)
         last
         (#(clojure.string/replace % #"//.*" ""))
         (#(json/read-str % :key-fn cf))
         (filter (fn [[_ {:keys [key]}]] (= 1 (count key))))
         (mapcat (fn [[k {:keys [values]}]]
                   (map #(hash-map :district (clojure.string/upper-case %) :region k) (vals values))))
         to-dataset)))

;;
;; Join the nomination details with election results
;;

(def result
  (let [data    (parse-slurp "http://www.elections.gov.hk/dc2015/eng/results_hk.html")
        hdrs    (read-hdrs "table.contents2" data)
        results (->> (extract-from data "table.contents2 tr" [:x :y] "td[rowspan]" text "td" text)
                     (remove #(every? nil? (vals %)))
                     (reduce (fn [[last res] {:keys [x y]}]
                               (if (nil? x)
                                 [last (conj res (concat last y))]
                                 [x    (conj res y)]))
                             [nil []])
                     last
                     (map #(zipmap hdrs %))
                     to-dataset
                     (add-derived-column "Votes" ["No. of Votes Received"]
                                         #(let [x (-> (re-find #"[0-9,]*" %)
                                                      (clojure.string/replace "," ""))]
                                            (read-string (if (empty? x) "0" x))))
                     (add-derived-column "Win" ["No. of Votes Received"]
                                         #(if (re-find #"\*" %) 1 0))
                     (add-derived-column "Contested" ["No. of Votes Received"]
                                         #(if (re-find #"Uncontest" %) 0 1)))
        noms    (->> "http://www.elections.gov.hk/dc2015/pdf/2015_DCE_Valid_Nominations_C.html"
                     read-data
                     to-dataset)
        noms2   (->> (extract-from (parse-slurp "http://www.elections.gov.hk/dc2015/chi/nominat2.html")
                                   "table tr td"
                                   [:x] "a" (attr :href))
                     (map :x)
                     (remove nil?)
                     (apply concat)
                     (filter #(and (string? %) (re-matches #"\.\./pdf/nomination.*html" %)))
                     (map #(->> % (drop 2) (apply str) (str "http://www.elections.gov.hk/dc2015")))
                     (mapcat read-data)
                     to-dataset)
        mm      ($join [["選區代號" "獲提名人士姓名 (姓氏先行)"] ["選區號碼" "姓名"]] noms2 noms)]

    (-> ($join [["選區號碼" "候選人編號"] ["Constituency Code" "Candidate Number"]]
               mm
               results)
        (#($join ["Constituency Code" :district] % district-map))
        #_(save-xls "output.xls"))))

;;
;; Count by region
;;

(def by-region
  (->> ($where {"Win" 1} result)
       ($rollup :count "Win" [:region])))

(defn- region-color [val min max]
  (ip/lerp-color (ip/color 0x4499CC) (ip/color 0xCC4444) (ip/norm val min max)))

(defn- map-color [regions]
  (let [min (apply min ($ "Win" regions))
        max (apply max ($ "Win" regions))
        m   (into {} (to-vect regions))]
    (zipmap (keys m) (map #(region-color % min max) (vals m)))))

;;
;; Lookup map id from region name
;;

(def region->id
  (let [d (parse-slurp "hk.svg")]
    (->> (extract-from d "svg g"
                       [:id :title]
                       "path" (attr :id)
                       "path" (attr :title))
         first
         (#(zipmap (:title %) (:id %))))))

;;
;; Draw HK heatmap
;;

(defn hkmap [regions]
  (ip/sketch
   (setup [])
   (draw []
         (let [m (ip/load-shape this "hk.svg")]
           (.disableStyle m)
           (.fill this (ip/color 0xAEBABA))
           (.shape this m 0 0)
           (doseq [[k v] (map-color regions)]
               (let [k     (region->id (name k))
                     child (.getChild m k)]
               (.disableStyle child)
               (.fill this v)
               #_(.noStroke this)
               (.shape this child 0 0)
               ip/no-loop))))))

(view (hkmap by-region) :size [640 480])

