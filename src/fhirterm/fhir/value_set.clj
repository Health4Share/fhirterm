(ns fhirterm.fhir.value-set
  (:require [fhirterm.json :as json]
            [fhirterm.util :as util]
            [fhirterm.fhir.client :as fhir-client]
            [fhirterm.naming-system.core :as naming-system]
            [fhirterm.naming-system.vs-defined :as vs-defined-ns]
            [clj-time.core :as time]
            [clojure.string :as str]))

(defn find-by-id [id]
  (fhir-client/get-resource "ValueSet" id))

(defn find-by-identifier [identifier]
  (get-in (fhir-client/search "ValueSet" {:identifier identifier})
          [:entry 0 :resource]))

(defn find-vs-defining-ns [ns-uri]
  (get-in (fhir-client/search "ValueSet" {:system ns-uri})
          [:entry 0 :resource]))

(defn- params-to-filters [{:keys [limit filter] :as params}]
  (-> {:text filter :limit limit}
      (update-in [:limit] (fn [l] (if l (java.lang.Long. l) nil)))))

(defn- resolve-naming-system [ns-uri]
  (if (naming-system/known? ns-uri)
    ns-uri

    (let [vs-ns (find-vs-defining-ns ns-uri)]
      (if vs-ns
        vs-ns
        (throw (IllegalArgumentException. (format "Unknown NamingSystem: %s"
                                                  ns-uri)))))))

(defn- filters-from-include-or-exclude [includes]
  (map (fn [inc]
         (let [regular-filters (or (:filter inc) [])
               concepts (reduce (fn [acc c]
                                  (assoc acc (:code c) c))
                                {} (:concept inc))

               code-filter (if (empty? concepts)
                             []
                             [{:op "in" :property "code" :value concepts}])]

           (into regular-filters code-filter)))
       includes))

(defn- get-composing-filters [vs params]
  (let [includes-by-syst (group-by :system (get-in vs [:compose :include]))
        excludes-by-syst (group-by :system (get-in vs [:compose :exclude]))
        grouped-filters
        (reduce (fn [acc syst]
                  (assoc acc syst
                         {:include
                          (filters-from-include-or-exclude (get includes-by-syst syst))
                          :exclude
                          (filters-from-include-or-exclude (get excludes-by-syst syst))}))
                {} (keys includes-by-syst))]

    ;; additional filters (for expansion), if any
    (reduce (fn [acc [ns fs]]
              (assoc acc ns (merge (get acc ns)
                                   (params-to-filters params))))
            grouped-filters grouped-filters)))

(defn- expand-with-compose-include-and-exclude [expansion vs params]
  (let [filters-by-ns (get-composing-filters vs params)]
    (reduce (fn [res [ns filters]]
              (let [system (resolve-naming-system ns)]
                (into res (naming-system/filter-codes system filters))))
            expansion filters-by-ns)))

(defn- validate-with-compose-include-and-exclude [result vs coding]
  (let [filters-by-ns (get-composing-filters vs {})]
    (or result
        (reduce (fn [res [ns filters]]
                  (let [system (resolve-naming-system ns)]
                    (or res (naming-system/validate system coding))))
                false filters-by-ns))))

(defn- expand-with-define [expansion vs params]
  (into expansion
        (vs-defined-ns/filter-codes vs (merge (params-to-filters params)
                                              {:include []
                                               :exclude []}))))

(defn- validate-with-define [result vs coding]
  (or result
      (vs-defined-ns/validate vs coding)))

(declare expand*)
(defn- expand-with-compose-import [expansion vs params]
  (let [imports (get-in vs [:compose :import])]
    (reduce (fn [result identifier]
              (let [imported-vs (find-by-identifier identifier)]
                (if imported-vs
                  (into result (expand* imported-vs params))
                  result)))
            expansion imports)))

(declare validate)
(defn- validate-with-compose-import [result vs coding]
  (or result
      (let [imports (get-in vs [:compose :import])]
        (reduce (fn [res identifier]
                  (or res
                      (let [imported-vs (find-by-identifier identifier)]
                        (when imported-vs
                          (validate imported-vs coding)))))
                false imports))))

(declare costly-expansion?)
(defn- costly-import? [vs params]
  (let [imports (get-in vs [:compose :import])]
    (reduce (fn [result identifier]
              (if (not result)
                (let [imported-vs (find-by-identifier identifier)]
                  (if imported-vs
                    (costly-expansion? imported-vs params)
                    result))

                result))
            false imports)))

(defn- costly-compose? [vs params]
  (let [filters-by-ns (get-composing-filters vs params)]
    (reduce (fn [res [ns filters]]
              (if (not res)
                (let [system (resolve-naming-system ns)]
                  (naming-system/costly? system filters))
                res))
            false filters-by-ns)))

;; TODO: rewrite with algo.monads
(defn- expand* [vs params]
  (-> []
      (expand-with-define vs params)
      (expand-with-compose-import vs params)
      (expand-with-compose-include-and-exclude vs params)))

;; TODO: rewrite with algo.monads
(defn- costly-expansion? [vs params]
  (or (costly-import? vs params)
      (costly-compose? vs params)))

(defn expand [vs params]
  (if (costly-expansion? vs params)
    :too-costly

    (let [result (expand* vs params)]
      (assoc vs :expansion {:identifier (util/uuid)
                            :timestamp (time/now)
                            :contains (map #(update-in % [:code] str) result)}))))

(defn validate [vs coding]
  (-> false
      (validate-with-define vs coding)
      (validate-with-compose-import vs coding)
      (validate-with-compose-include-and-exclude vs coding)))
