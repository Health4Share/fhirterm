(ns fhirterm.fixtures
  (:require [fhirterm.system :as system]
            [fhirterm.db :as db]
            [fhirterm.tasks.import-vs :as import-vs]))

(def ^:dynamic *config* nil)

(defn import-vs-fixture [f]
  (import-vs/perform nil ["test/fixtures/value_sets"])
  (f))

(defn create-custom-ns-fixture [f]
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

  (f))

(defn start-server-fixture [f]
  (println "Starting test server")

  (alter-var-root #'*config*
                  (constantly (system/read-config "test/config.json")))

  (system/stop)
  (let [system (system/start *config*)]
    (f)
    (println "Stopping server")
    (system/stop)))
