(ns fhirterm.naming-system.vs-defined
  (:require [fhirterm.util :as util]))

(defn- concept-matches-filter? [{:keys [property op value]} path concept]
  (when (not (contains? #{"is-a" "in"} op) )
    (throw (IllegalArgumentException.
            (format "Unknown filtering operation: %s" op))))

  (when (not (contains? #{"concept" "code"} property))
    (throw (IllegalArgumentException.
            (format "Unknown filtering property: %s" concept))))

  (let [matches (condp = op
                  "is-a" (not (empty? (filter (partial = value) path)))
                  "in"   (contains? value (:code concept)))]

    (and matches
         (not (:abstract concept)))))

(defn- concept-matches-filters? [filters path concept result-when-empty]
  (if (empty? filters)
    result-when-empty

    (reduce (fn [r fs]
              (or r
                  (reduce (fn [r f]
                            (and r (concept-matches-filter? f path concept)))
                          true fs)))
            false filters)))

(defn- concept-matches-text-filter? [c text]
  (if (not text)
    true
    (util/string-contains? (:display c) text true)))

(defn- check-concept [{:keys [include exclude text] :as filters} path concept]
  (and (concept-matches-filters? include path concept true)
       (not (concept-matches-filters? exclude path concept false))
       (concept-matches-text-filter? concept text)))

(defn- filter-concepts [concepts f path]
  (reduce (fn [acc c]
            (let [nested (:concept c)
                  c-without-nested (dissoc c :concept)
                  acc (if (f path c-without-nested)
                        (conj acc c-without-nested)
                        acc)]

              (if (seq nested)
                (into acc (filter-concepts nested f
                                           (conj path (:code c))))
                acc)))
          [] concepts))

(defn filter-codes [{{concepts :concept system :system} :define :as vs} filters]
  (let [concepts (-> (filter-concepts concepts (partial check-concept filters) [])
                     ;; limit filter
                     ((fn [concepts]
                        (if (:limit filters)
                          (take (java.lang.Long. (:limit filters)) concepts)
                          concepts))))]

    (map (fn [c] (assoc c :system system)) concepts)))

(defn lookup-code [vs params]
  nil)

(defn costly? [vs filters threshold] false)

(defn validate [vs filters coding]
  (let [expansion (filter-codes vs filters)
        ks [:system :code]
        coding-selected-keys (select-keys coding ks)
        found-coding (first (filter (fn [c]
                                      (= (select-keys c ks) coding-selected-keys))
                                    expansion))]

    (if found-coding
      (if (and (:display coding)
               (not= (:display coding) (:display found-coding)))
        [false {:message "Display is not correct!"
                :display (:display found-coding)}]
        [true {:message "Coding is valid"}])

      nil)))
