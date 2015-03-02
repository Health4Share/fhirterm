(ns fhirterm.naming-system.core
  (:require [clojure.string :as str]
            [fhirterm.db :as db]
            [honeysql.helpers :as sql]
            [fhirterm.naming-system.vs-defined :as vs-defined-ns]))

(def uri-to-symbolic-name
  {"http://loinc.org" :loinc
   "http://snomed.info/sct" :snomed
   "http://unitsofmeasure.org" :ucum
   "http://www.nlm.nih.gov/research/umls/rxnorm" :rxnorm})

(def custom-naming-systems (atom nil))

(defn- normalize-system-uri [uri]
  (str/replace uri #"/$" ""))

(defn find-custom-ns [system-uri]
  (if (nil? @custom-naming-systems)
    ;; populate custom-naming-systems cache
    (let [naming-systems (db/q (-> (sql/select :*)
                                   (sql/from :custom_naming_systems)))
          naming-systems-map (reduce (fn [m n] (assoc m (:uri n) n))
                                     {} naming-systems)]

      ;; update atom
      (swap! custom-naming-systems (identity naming-systems-map))

      ;; return result from collected map
      (get naming-systems-map system-uri))

    ;; return result from cache
    (get @custom-naming-systems system-uri)))

(defn- resolve-system-ns-and-arg [system]
  (if (string? system)
    (let [system-uri (normalize-system-uri system)]
      (cond
       (contains? uri-to-symbolic-name system-uri)
       [(symbol (str "fhirterm.naming-system."
                     (name (get uri-to-symbolic-name system-uri))))
        nil]

       (find-custom-ns system-uri)
       ['fhirterm.naming-system.custom (find-custom-ns system-uri)]

       :else
       nil))

    ['fhirterm.naming-system.vs-defined system]))

(defn known? [system-uri]
  (let [uri (normalize-system-uri system-uri)]
    (or (contains? uri-to-symbolic-name uri)
        (find-custom-ns uri))))

(defn- invoke-ns-method [system method-name & args]
  (let [[system-ns arg] (resolve-system-ns-and-arg system)]
    (if system-ns
      (do
        (require system-ns)
        (apply (ns-resolve system-ns method-name)
               (if arg (into [arg] args) args)))

      (throw (IllegalArgumentException. (format "Unknown NamingSystem: %s"
                                                (pr-str system)))))))

(defn lookup-code [{:keys [system] :as params}]
  (invoke-ns-method system 'lookup-code params))

(defn filter-codes [system filters]
  (invoke-ns-method system 'filter-codes filters))

(def costly-threshold 1000)

(defn costly? [system filters]
  (invoke-ns-method system 'costly? filters costly-threshold))

(defn validate [system coding]
  (invoke-ns-method system 'validate coding))
