(defproject fhirterm "0.1.0-SNAPSHOT"
  :description "FHIR Terminology Server"
  :url "http://github.com/fhirbase/fhirterm"

  :license {:name "MIT"
            :url "http://opensource.org/licenses/MIT"}

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/java.jdbc "0.3.6"]
                 [com.mchange/c3p0 "0.9.5-pre9"]
                 [org.xerial/sqlite-jdbc "3.8.7"]
                 [compojure "1.3.1"]
                 [http-kit "2.1.16"]
                 [org.clojure/tools.cli "0.3.1"]
                 [honeysql "0.4.3"]
                 [cheshire "5.4.0"]
                 [clj-time "0.8.0"]
                 [ring/ring-devel "1.3.1"]
                 [criterium "0.4.3"]
                 [com.taoensso/timbre "3.3.1"]
                 [instaparse "1.3.5"] ;; parser generator for UCUM parsing
                 [org.clojure/data.csv "0.1.2"]
                 [ring-cors "0.1.6"]]

  :java-source-paths ["src/java"]

  :profiles {:dev {:source-paths ["dev"]}
             :dependencies [[org.clojure/tools.namespace "0.2.4"]]}

  :test-selectors {:default (fn [m] (not (and (:integration m) (:tasks m))))
                   :integration :integration
                   :tasks :task
                   :all (constantly true)}

  :main fhirterm.core
  :aot [fhirterm.sqlite.connection-customizer])
