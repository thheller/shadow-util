(ns shadow.server.runtime
  (:require [loom.graph :as lg]
            [loom.alg :as la]
            [clojure.string :as str]))

(defn- rt-state? [x]
  (and (map? x) (map? (::app x))))

(defn init [state app]
  {:pre [(map? state)
         (map? app)]}
  (assoc state ::app app))

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

(defn- stop-service
  [{::keys [app] :as state} service-id]
  (if-let [service (get state service-id)]
    (if-let [{:keys [stop] :as service-def}
             (get app service-id)]
      (do (when stop
            (stop service))
          (dissoc state service-id))
      ;; not defined, do nothing
      state)
    ;; not present, do nothing
    state))

(defn stop-all
  [{::keys [app] :as state}]
  {:pre [(rt-state? state)]}
  (let [stop-order (-> app app->graph la/topsort)
        stop-order (concat stop-order (services-without-deps app))]
    (reduce stop-service state stop-order)
    ))

(defn stop-single
  [state service]
  {:pre [(rt-state? state)]}
  (stop-service state service))

(defn stop-many
  [state services]
  (reduce stop-single state services))

;; starting

(defn- start-one
  [{::keys [app] :as state} service-id]
  ;; already present, assume its started
  (if (contains? state service-id)
    state
    ;; lookup up definition, get deps (assumes deps are already started), start
    (if-let [{:keys [depends-on start] :as service-def} (get app service-id)]
      (let [deps
            (map #(get state %) depends-on)

            service-instance
            (apply start deps)]

        (assoc state service-id service-instance))
      ;; not defined
      (throw (ex-info (format "cannot start/find undefined service %s (%s)" service-id (str/join "," (keys state))) {:service service-id :provided (keys state)}))
      )))

(defn- start-many
  "start services and return updated state
   will attempt to stop all if one startup fails"
  [state services]
  {:keys [(rt-state? state)]}
  (loop [state
         state

         start
         services

         started
         []]

    (let [service-id (first start)]
      (if (nil? service-id)
        ;; nothing left to start
        state

        ;; start next service
        (let [state
              (try
                (start-one state service-id)
                (catch Exception e
                  ;; FIXME: ignores an exception if a rollback fails
                  (try
                    (stop-many state started)
                    (catch Exception x
                      (prn [:failed-to-rollback started x])))

                  (throw (ex-info "failed to start service" {:id service-id} e))))]
          (recur state (rest start) (conj started service-id)))
        ))))

(defn start-all
  "start all services in dependency order, will attempt to properly shutdown if once service fails to start"
  [{::keys [app] :as state}]
  {:pre [(rt-state? state)]}
  (let [start-order (-> app app->graph la/topsort reverse)
        start-order (concat start-order (services-without-deps app))]
    (start-many state start-order)
    ))

(defn start-single
  "start a single service (and its deps)"
  [{::keys [app] :as state} service]
  {:pre [(rt-state? state)]}
  (let [start-order (-> app app->graph (la/topsort service) reverse)]
    (start-many state start-order)))

(defn start-services
  "start a multiple services (and their deps)"
  [state services]
  {:pre [(rt-state? state)]}
  (reduce start-single state services))

