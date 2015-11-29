(ns dc2015.core
  (:require [reaver :refer [parse extract-from text attr]]))

(use '(incanter core stats charts datasets excel))

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
                   to-dataset)
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
      (save-xls "output.xls")))
