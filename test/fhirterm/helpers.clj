(ns fhirterm.helpers
  (:require [clojure.string :as str]
            [fhirterm.fixtures :as fixtures]))

(defn make-url [& p]
  (let [base (format "http://localhost:%d" (get-in fixtures/*config* [:http :port]))]
    (str/join "/" (into [base] p))))
