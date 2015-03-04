(ns fhirterm.sqlite.connection-customizer
  (:gen-class
   :main false
   :name fhirterm.sqlite.ConnectionCustomizer
   :extends com.mchange.v2.c3p0.AbstractConnectionCustomizer))

(defn -onAcquire [^fhirterm.sqlite.ConnectionCustomizer this
                  ^org.sqlite.SQLiteConnection conn
                  ^java.lang.String _]
  (println "!!! acquired connection: " (pr-str conn)))
