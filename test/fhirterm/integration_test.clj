(ns fhirterm.integration-test
  (:require [fhirterm.system :as system]
            [fhirterm.db :as db]
            [clojure.test :refer :all]
            [fhirterm.json :as json]
            [clojure.string :as str]
            [fhirterm.tasks.import-vs :as import-vs]
            [org.httpkit.client :as http]))

(def ^:dynamic *config* nil)

(defn start-server-fixture [f]
  (println "Starting test server")

  (alter-var-root #'*config*
                  (constantly (system/read-config "test/config.json")))

  (system/stop)
  (let [system (system/start *config*)]
    (import-vs/perform db/*db* ["test/fixtures/value_sets"])

    (db/e! "DROP TABLE IF EXISTS custom_ns")
    (db/e! "DELETE FROM custom_naming_systems WHERE table_name = 'custom_ns'")

    (db/e! "CREATE TABLE custom_ns (code varchar primary key,
                                    display varchar,
                                    definition varchar)")

    (db/i! :custom_naming_systems {:uri "http://example.com/custom_ns"
                                   :table_name "custom_ns"})

    (db/i! :custom_ns {:code "a" :display "A code"})
    (db/i! :custom_ns {:code "b" :display "B code"})
    (db/i! :custom_ns {:code "c" :display "C code abc"})

    (f)
    (println "Stopping server")
    (system/stop)))

(use-fixtures :once start-server-fixture)

(defn make-url [& p]
  (let [base (format "http://localhost:%d" (get-in *config* [:http :port]))]
    (str/join "/" (into [base] p))))

(defn expand-vs [id & [params]]
  (let [{body :body :as response} @(http/get (make-url "ValueSet" id "$expand")
                                             {:query-params (or params {})})]

    (when (nil? body)
      (println "!!!" (pr-str response)))

    (json/parse (if (string? body) body (slurp body)))))

(defn get-expansion [r]
  (let [expansion (get-in r [:expansion :contains])
        codes (map :code expansion)
        codes-set (set codes)]

    (is (= (count codes) (count codes-set))
        "expansion codes are distinct")

    expansion))

(defn find-coding [codings code]
  (first (filter (fn [c] (= (:code c) (str code))) codings)))

(deftest ^:integration expansion-of-vs-with-enumerated-loinc-codes-test
  (let [result (get-expansion (expand-vs "lipid-ldl-codes"))]

    (is (find-coding result "13457-7")
        "enumerated code is present in expansion result")

    (is (find-coding result "18262-6")
        "enumerated code is present in expansion result")

    (is (= (count result) 2)
        "two codings in expansion")))

(deftest ^:integration expansion-of-vs-with-entire-loinc-included-test
  (let [result (get-expansion (expand-vs "valueset-observation-codes"
                                         {:filter "blood"}))]
    (is (= (count result) 51))))

(deftest ^:integration expansion-of-vs-with-loinc-filtered-by-order-obs-test
  (let [result (get-expansion (expand-vs "valueset-diagnostic-requests"
                                         {:filter "blood"}))]

    (is (find-coding result "888-8"))
    (is (find-coding result "55429-5"))

    (is (= (count result) 6))))

(deftest ^:integration expansion-of-snomed-vs-test
  (let [result (get-expansion (expand-vs "valueset-route-codes"
                                         {:filter "s"}))]
    (is (find-coding result 418877009))
    (is (find-coding result 445755006))

    (is (= (count result) 77)))

  (let [result (get-expansion (expand-vs "valueset-test-snomed-compose-only-exclude"
                                         {:filter "Anatomical"}))]
    (is (not (find-coding result 91723000))))

  (let [result (get-expansion (expand-vs "valueset-test-snomed-compose-only-exclude"
                                         {:filter "artery and vein"}))]
    (is (not (find-coding result 110748003)))))

(deftest ^:integration expansion-of-explicitely-defined-vs-test
  (let [result (get-expansion (expand-vs "valueset-practitioner-specialty"))]

    (is (find-coding result "dietary"))
    (is (find-coding result "cardio"))

    (is (= (count result) 5))))

(deftest ^:integration expansion-of-snomed-vs-composed-from-two-lookups-test
  (let [result (get-expansion (expand-vs "valueset-daf-problem"
                                         {:filter "s"}))]
    (is (find-coding result 162005007))
    (is (find-coding result 308698004))
    (is (find-coding result 163032006))
    (is (find-coding result 164006007))

    (is (= (count result) 3580))))

(deftest ^:integration expansion-of-vs-with-import-test
  (let [result (get-expansion (expand-vs "valueset-questionnaire-question-text-type"))]

    (doseq [c ["instruction" "security" "trailing" "tooltip" "units"]]
      (is (find-coding result c)))

    (is (= (count result) 6))))

(deftest ^:integration expansion-of-vs-with-inclusion-of-ns-defined-in-other-value-set-test
  (let [result (get-expansion (expand-vs "valueset-contraindication-mitigation-action"))]
    (doseq [c ["EMAUTH" "21" "1" "19" "2"]]
      (is (find-coding result c)))

    (is (= (count result) 24)))

  (let [result (get-expansion (expand-vs "valueset-relatedperson-relationshiptype"))]
    (doseq [c ["WIFE" "HUSB" "NBOR" "ROOM" "SPS"]]
      (is (find-coding result c)))

    (is (= (count result) 120))))

(deftest ^:integration expansion-of-ucum-value-sets-test
  (let [result (get-expansion (expand-vs "valueset-ucum-vitals-common"))]
    (doseq [c ["%" "cm" "kg" "Cel" "m2"]]
      (is (find-coding result c)))

    (is (= (count result) 8)))

  (let [result (get-expansion (expand-vs "valueset-ucum-common"))]
    (doseq [c ["%{Fat}" "/g{tot'nit}"]]
      (is (find-coding result c)))

    (is (= (count result) 1363)))

  (let [result (get-expansion (expand-vs "ccdaagecodes"))]
    (is (= (count result) 6))))

(deftest ^:integration expansion-of-rxnorm-value-sets-test
  (let [result (get-expansion (expand-vs "valueset-test-rxnorm-explicit-codes"))]
    (doseq [c ["38" "44" "61"]]
      (is (find-coding result c)))

    (is (= (count result) 4)))

  (let [result (get-expansion (expand-vs "valueset-test-rxnorm-filter-sty"))]
    (doseq [c ["2236" "1306059" "992396" "151343"]]
      (is (find-coding result c)))

    (is (= (count result) 957)))

  (let [result (get-expansion (expand-vs "valueset-test-rxnorm-filter-sab"
                                         {:filter "ba"}))]
    (doseq [c ["1116309" "221167" "1369787" "866305"]]
      (is (find-coding result c)))

    (is (= (count result) 624)))

  (let [excluded-codes {"310214" "estropipate"
                        "1012727" "CARBINOXAMINE"
                        "861035" "PRAMLINTIDE"
                        "397" "agar"}]
    (doseq [[code filter] excluded-codes]
      (is (not (find-coding
                (get-expansion
                 (expand-vs "valueset-test-rxnorm-filter-except-only"
                            {:filter filter}))
                code)))))

  (let [result (get-expansion (expand-vs "valueset-test-rxnorm-filter-except-only"
                                         {:filter "ban"}))]
    (doseq [c ["617515" "1167996" "564109" "379572"]]
      (is (find-coding result c))))

  (let [result (get-expansion (expand-vs "valueset-test-rxnorm-filter-combinations"
                                         {:filter "ban"}))]
    (doseq [c ["1148138" "115264" "904955" "1232082"]]
      (is (find-coding result c)))

    (doseq [c ["310214" "1012727" "861035" "397"]]
      (is (not (find-coding result c))))

    (is (= (count result) 30))))

(deftest ^:integration too-costly-expansions-test
  (doseq [vs ["valueset-test-rxnorm-all" "valueset-test-snomed-all"
              "valueset-observation-codes"]]
    (is (= "too-costly"
           (get-in (expand-vs vs) [:issue 0 :type :code]))
        vs)))

(deftest ^:integration custom-ns-expansions-test
  (let [r (get-expansion (expand-vs "valueset-custom-ns-no-filters"))]
    (is (find-coding r "a"))
    (is (find-coding r "b")))

  (let [r (get-expansion (expand-vs "valueset-custom-ns-no-filters"
                                    {:filter "abc"}))]
    (is (find-coding r "c")))

  (let [r (get-expansion (expand-vs "valueset-custom-ns-codes-filter"))]
    (is (not (find-coding r "c")))))

(deftest ^:integration expansion-with-text-filtering
  ;; defined ns
  (let [r (get-expansion (expand-vs "valueset-questionnaire-question-text-type"
                                    {:filter "too"}))]
    (is (find-coding r "tooltip"))
    (is (not (find-coding r "help"))))

  ;; UCUM
  (let [r (get-expansion (expand-vs "valueset-ucum-vitals-common"
                                    {:filter "mil"}))]
    (is (find-coding r "mm[Hg]"))
    (is (not (find-coding r "Cel"))))

  ;; SNOMED
  (let [r (get-expansion (expand-vs "valueset-daf-problem-severity"
                                    {:filter "fat"}))]
    (is (find-coding r 399166001))
    (is (not (find-coding r 371923003))))

  ;; LOINC
  (let [result (get-expansion (expand-vs "lipid-ldl-codes" {:filter "Direc"}))]
    (is (find-coding result "18262-6"))
    (is (not (find-coding result "13457-7")))
    (is (= (count result) 1)))

  ;; imported VS
  (let [result (get-expansion (expand-vs "valueset-contraindication-mitigation-action"
                                         {:filter "Lea"}))]
    (is (find-coding result "18"))
    (is (= (count result) 1))))

(deftest ^:integration expansion-with-limit-filter
  (doseq [vs ["valueset-questionnaire-question-text-type"
              "valueset-ucum-vitals-common"
              "valueset-daf-problem-severity"
              "lipid-ldl-codes"
              "valueset-contraindication-mitigation-action"]]
    (let [r (get-expansion (expand-vs vs {:limit 1}))]
      (is (= (count r) 1) vs))))
