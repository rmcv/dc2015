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
;; District name to constituency code mapping
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
                     (add-derived-column :Votes ["No. of Votes Received"]
                                         #(let [x (-> (re-find #"[0-9,]*" %)
                                                      (clojure.string/replace "," ""))]
                                            (read-string (if (empty? x) "0" x))))
                     (add-derived-column :Win ["No. of Votes Received"]
                                         #(if (re-find #"\*" %) 1 0))
                     (add-derived-column :Contested ["No. of Votes Received"]
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
  (->> ($where {:Win 1} result)
       ($rollup :count :Win [:region])))

(def by-party-region
  (let [clean-party         #(if (nil? %)
                               "Others"
                               (condp re-find %
                                 #"(民建聯|工聯會|勞聯|自由黨|新民黨|經民聯|工會聯合會)" "Pan-Establishment"
                                 #"(民主黨|公民黨|新民主同盟|工黨|社民連|人民力量|民協)" "Pan-Democracy"
                                 #"[\s\S\w\W]"                                           "Others"))
        contesters          (->> ($where {:Contested 1} result)
                                 (add-derived-column :Party ["政治聯繫"] clean-party))
        group-by-key        [:region :Party]
        contested-by-region (->> contesters
                                 ($rollup :count :Contested group-by-key))
        win-by-party        (->> contesters
                                 ($where {:Win 1})
                                 ($rollup :count :Win group-by-key)
                                 ($order [:region :Contested] :desc))]
    (->> ($join [group-by-key group-by-key] contested-by-region win-by-party)
         (add-derived-column :hit [:Win :Contested] #(float (/ %1 %2))))))


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

(defn- region-color [val min max]
  (ip/lerp-color (ip/color 0xFFFFFF) (ip/color 0x0099FF) (ip/norm val min max)))

(defn- map-color [ds & {:keys [low high]}]
  (let [min (or low (apply min ($ :hit ds)))
        max (or high (apply max ($ :hit ds)))]
    (zipmap (->> ds
                 ($ :region)
                 (map name)
                 (map region->id))
            (map #(region-color % min max) ($ :hit ds)))))

(defn- hkmap [& data]
  (ip/sketch
   (setup [])
   (draw []
         (let [m (ip/load-shape this "hk.svg")]
           (.disableStyle m)
           (.fill this (ip/color 0xFFFFFF))
           (.shape this m 0 0)
           (doseq [[k v] (apply map-color data)]
             (let [child (.getChild m (name k))]
               (.disableStyle child)
               (.fill this v)
               (.shape this child 0 0)
               ip/no-loop))))))

(defn view-map [party]
  (let [ds ($ [:region :hit] (->> by-party-region ($where {:Party party})))]
    (view (hkmap ds :low 0) :size [640 480] :title (str "Elected Ratio - " party))))

(view-map "Pan-Democracy")
(view-map "Pan-Establishment")
(view-map "Others")
(view by-party-region)

