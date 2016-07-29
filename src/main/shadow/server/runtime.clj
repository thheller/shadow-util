(ns shadow.server.runtime
  (:require [loom.graph :as lg]
            [loom.alg :as la]
            [clojure.string :as str]))

;; util

(defn app->graph [app]
  (let [edges (for [[id {:keys [depends-on]}] app
                    dep depends-on]
                [id dep])]
    (apply lg/digraph edges)))

(defn services-without-deps [app]
  (reduce-kv (fn [result id {:keys [depends-on]}]
               (if (not (seq depends-on))
                 (conj result id)
                 result))
             []
             app))

;; stopping

(defn- stop-service [app state service-id]
  (if-let [service (get state service-id)]
    (if-let [{:keys [stop] :as service-def} (get app service-id)]
      (do (when stop
            (stop service))
          (dissoc state service-id))
      ;; not defined, do nothing
      state)
    ;; not present, do nothing
    state))

(defn stop-all [state app]
  (let [stop-order (-> app app->graph la/topsort)
        stop-order (concat stop-order (services-without-deps app))]
    (reduce (partial stop-service app) state stop-order)
    ))

(defn stop-single [state app service]
  (stop-service app state service))

;; starting

(defn- start-service [app state service-id]
  ;; already present, assume its started
  (if (contains? state service-id)
    state
    ;; lookup up definition, get deps (assumes deps are already started), start
    (if-let [{:keys [depends-on start] :as service-def} (get app service-id)]
      (let [deps (map #(get state %) depends-on)
            service-instance (try
                               (apply start deps)
                               (catch Exception e
                                 ;; stop services we attempted to start, hope that that works
                                 (reduce (partial stop-service app)
                                         state
                                         (->> (::started state)
                                              (reverse)))
                                 (throw (ex-info "failed to start service"
                                                 {:service service-id
                                                  :service-def service-def}
                                                 e))))]

        (-> state
            (assoc service-id service-instance)
            (update-in [::started] conj service-id)))
      ;; not defined
      (throw (ex-info (format "cannot start/find undefined service %s (%s)" service-id (str/join "," (keys state))) {:service service-id :provided (keys state)}))
      )))

(defn start-all
  "start all services in dependency order"
  [state app]
  (let [start-order (-> app app->graph la/topsort reverse)
        start-order (concat start-order (services-without-deps app))]
    (-> (reduce (partial start-service app)
                (assoc state
                  ::started [])
                start-order)
        (dissoc ::started))))

(defn start-single
  "start a single service (and its deps)"
  [state app service]
  (let [start-order (-> app app->graph (la/topsort service) reverse)]
    (-> (reduce (partial start-service app)
                (assoc state
                  ::started [])
                start-order)
        (dissoc ::started))))

(defn start-services
  "start a subset of services (and their deps)"
  [state app services]
  (reduce (fn [state service-id]
            (start-single state app service-id))
          state
          services))

