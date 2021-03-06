(ns fhirterm.server
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.params :as ring-params]
            [ring.middleware.keyword-params :as ring-kw-params]
            [ring.middleware.stacktrace :as ring-stacktrace]
            [ring.middleware.resource :as ring-resource]
            [ring.middleware.cors :as ring-cors]
            [fhirterm.json :as json]
            [fhirterm.util :as util]
            [fhirterm.naming-system.core :as ns-core]
            [fhirterm.fhir.core :as fhir]
            [fhirterm.fhir.value-set :as vs]
            [taoensso.timbre :as timbre]
            [clojure.string :as str]
            [org.httpkit.server :as http-kit]))

(timbre/refer-timbre)

(defn- non-blank-keys [params ks]
  (let [m (select-keys params ks)]
    (select-keys m (for [[k v] m :when (not (str/blank? v))] k))))

(defn respond-with [status obj]
  (let [json (if (string? obj) (json/parse obj) obj)
        json-string (json/generate json {:pretty true})]
    {:status status
     :body json-string
     :content-type "application/json"}))

(defn respond-with-outcome [severity type message & [http-status]]
  (respond-with (or http-status 500)
                (fhir/make-operation-outcome severity type message)))

(defn respond-with-not-found [& [msg]]
  (respond-with-outcome :fatal :not-found
                        (or msg
                            "The requested URL could not be processed")
                        404))

(defroutes app
  (context "/ValueSet" []
    (GET "/$lookup" {params :params :as request}
      (let [result (ns-core/lookup-code params)]
        (if result
          (respond-with 200 (fhir/make-parameters result))
          (respond-with-not-found "Could not find requested coding"))))

    (GET "/:id/$expand" {{id :id :as params} :params :as request}
      (let [vs (vs/find-by-id id)]
        (if vs
          (let [expansion (vs/expand vs (non-blank-keys params [:filter :limit]))]
            (if (= expansion :too-costly)
              (respond-with-outcome :fatal :too-costly
                                    "ValueSet expansion will consume too much resources"
                                    200)

              (respond-with 200 expansion)))

          (respond-with-not-found (format "Could not find ValueSet with id = %s" id)))))

    (GET "/:id/$validate" {{:keys [id] :as params} :params
                           :as request}
      (let [coding (non-blank-keys params [:code :display :system :version])
            vs (vs/find-by-id id)]
        (if vs
          (let [result (vs/validate vs coding)
                params-result (assoc (second result)
                                :result (first result))]

            (respond-with 200 (fhir/make-parameters params-result)))

          (respond-with-not-found (format "Could not find ValueSet with id = %s" id))))))

  (route/not-found (respond-with-not-found)))

(defn assoc-into-request-mw [handler data]
  (fn [request]
    (handler (merge request data))))

(defn wrap-with-benchmark [handler]
  (fn [request]
    (info (str/upper-case (name (:request-method request)))
          (str (:uri request) "?" (:query-string request))
          "\nHeaders:" (pr-str (:headers request))
          "\nParams:" (pr-str (:params request)))

    (let [[time result] (util/measure-time (handler request))]
      (info "Finished in" time "ms\n\n")
      result)))

(defn wrap-with-operation-outcome-exception-handler [handler]
  (fn [request]
    (try
      (handler request)
      (catch Exception e
        (respond-with-outcome :fatal :exception
                              (format "Unexpected error while processing your request:\n%s"
                                      (.getMessage e))
                              500)))))

(defn wrap-with-exception-handler [handler env]
  (if (= :production env)
    (wrap-with-operation-outcome-exception-handler handler)
    (ring-stacktrace/wrap-stacktrace handler)))

(defn wrap-with-static-files-server-in-development [handler env]
  (if (= :development env)
    (ring-resource/wrap-resource handler "public")
    handler))

(defn- make-handler [env]
  (-> #(app %)
      (ring-cors/wrap-cors :access-control-allow-origin #".*"
                           :access-control-allow-methods [:get :post])
      (wrap-with-benchmark)
      (ring-kw-params/wrap-keyword-params)
      (ring-params/wrap-params)
      (wrap-with-exception-handler env)
      (assoc-into-request-mw {:env env})
      (wrap-with-static-files-server-in-development env)))

(defn start [{env :env :as config} db]
  (let [port (get-in config [:http :port])]
    (info (format "Started fhirterm server on http://localhost:%d" port))

    (http-kit/run-server (make-handler env)
                         {:port port})))

(defn stop [server]
  (when server (server)))
