(ns fhirterm.naming-system.loinc
  (:require [honeysql.helpers :as sql]
            [clojure.string :as str]
            [fhirterm.db :as db]))

(def loinc-uri "http://loinc.org")

(defn lookup-code [params]
  (let [found-loinc (db/q-one (-> (sql/select :*)
                                  (sql/from :loinc_loincs)
                                  (sql/where [:= :loinc_num (:code params)])
                                  (sql/limit 1)))]
    (when found-loinc
      {:name "LOINC"
       :version "to.do"
       :abstract false
       :display (:shortname found-loinc)
       :designation [{:value (:shortname found-loinc)}
                     {:value (:long_common_name found-loinc)}]})))

(def property-to-column-map
  {"code" :loinc_num
   "order_obs" :order_obs
   "scale_typ" :scale_type})

(def value-fixup-map
  {:scale_type {"DOC" "Doc"}})

(defn- filter-to-sql-cond [f]
  (let [property (:property f)
        column (get property-to-column-map
                    (str/lower-case property)
                    (keyword (str/lower-case property)))]

    (case (:op f)
      "=" [:= column (get-in value-fixup-map [column (:value f)]
                             (:value f))]
      "in" [:in column (vec (keys (:value f)))]
      (throw (IllegalArgumentException. (format "Unknown filtering op: %s" (:op f)))))))

(defn- filters-to-sql-cond [filters]
  (let [predicate (if (or (nil? filters) (empty? (flatten filters)))
                    nil
                    (into [:or]
                          (map (fn [fs]
                                 (into [:and] (map filter-to-sql-cond fs)))
                               filters)))]
    predicate))

(defn- row-to-coding [row]
  {:system loinc-uri
   :abstract false
   :version "to.do"
   :code (:loinc_num row)
   :display (:shortname row)})

(defn- combine-preds [inc-pred excl-pred]
  (cond
   (and (empty? (flatten inc-pred)) (empty? (flatten excl-pred))) nil
   (and inc-pred excl-pred) [:and inc-pred [:not excl-pred]]
   (and inc-pred (not excl-pred)) inc-pred
   (and (not inc-pred) excl-pred) [:not excl-pred]
   :default nil))

(defn filters-to-query [filters]
  (let [pred (combine-preds (filters-to-sql-cond (:include filters))
                            (filters-to-sql-cond (:exclude filters)))]

    (-> (sql/from :loinc_loincs)
        ((fn [q]
           (if pred (sql/where q pred) q)))

        ((fn [q]
           (if (:limit filters)
             (sql/limit q (java.lang.Long. (:limit filters))) q)))

        ((fn [q]
           (if (:text filters)
             (sql/merge-where q [:ilike :shortname
                                 (str "%" (:text filters) "%")]) q))))))

(defn filter-codes [filters]
  (let [query (-> (filters-to-query filters)
                  (sql/select :loinc_num :shortname))]

    (map row-to-coding (db/q query))))

(defn costly? [filters threshold]
  (let [count (db/q-val (-> (filters-to-query filters)
                            (sql/select :%count.*)))]

    (> count threshold)))

(defn validate [filters coding]
  (when (= loinc-uri (:system coding))
    (let [query (-> (filters-to-query filters)
                    (sql/select [:loinc_num :code] [:shortname :display])
                    (sql/merge-where [:= :loinc_num (:code coding)]))
          found-coding (db/q-one query)]

      (if found-coding
        (if (and (:display coding)
                 (not= (:display coding) (:display found-coding)))
          [false {:message "Display is not correct!"
                  :display (:display found-coding)}]

          [true {:message "Coding is valid"}])

        nil))))
