(ns shadow.runtime.services
  (:require [clojure.string :as str]))

(defn- rt-state? [x]
  (and (map? x) (map? (::app x))))

(defn init [state app]
  {:pre [(map? state)
         (map? app)]}
  (assoc state ::app app))

;; util

(defn topo-sort-services*
  [{:keys [services deps visited] :as state} name]
  (let [{:keys [depends-on] :as mod}
        (get services name)]
    (cond
      ;; undefined service dependency is ok, assuming it is provided
      (nil? mod)
      state

      (contains? deps name)
      (throw (ex-info "service circular dependeny" {:deps deps :name name}))

      (contains? visited name)
      state

      :else
      (-> state
          (update :visited conj name)
          (update :deps conj name)
          (as-> state
            (reduce topo-sort-services* state depends-on))
          (update :deps disj name)
          (update :order conj name)))))

(defn topo-sort-services
  ([services]
    (topo-sort-services services (keys services)))
  ([services sort-keys]
   (let [{:keys [deps visited order] :as result}
         (reduce
           topo-sort-services*
           {:deps #{}
            :visited #{}
            :order []
            :services services}
           sort-keys)]

     (assert (empty? deps))
     (assert (= (count visited) (count services)))

     order
     )))

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
  (let [stop-order (-> (topo-sort-services app) (reverse))]
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
  (let [start-order (topo-sort-services app)]
    (start-many state start-order)
    ))

(defn start-single
  "start a single service (and its deps)"
  [{::keys [app] :as state} service]
  {:pre [(rt-state? state)]}
  (let [start-order (topo-sort-services app [service])]
    (start-many state start-order)))

(defn start-services
  "start a multiple services (and their deps)"
  [state services]
  {:pre [(rt-state? state)]}
  (reduce start-single state services))

