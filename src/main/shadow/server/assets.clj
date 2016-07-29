(ns shadow.server.assets
  (:import [java.io File])
  (:require [hiccup.core :as hiccup :refer (html)]
            [clojure.pprint :refer (pprint)]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [clojure.java.io :as io]))

(defn css-path [{:keys [state] :as assets} file-id]
  (if-let [path (get-in @state [:css-files file-id :path])]
    path
    (throw (ex-info "no css file by id" {:file-id file-id :css-files (:css-files @state)}))))

(defn load-manifest
  [{:keys [js-root js-manifest
           css-root css-manifest
           state] :as assets}]
  (let [now
        (System/currentTimeMillis)

        js-root-fs
        (-> (io/file js-manifest)
            (.getParentFile))

        js-pack
        (->> (json/read-str (slurp js-manifest) :key-fn keyword)
             (map (fn [{:keys [js-name] :as mod}]
                    (let [js-file (io/file js-root-fs js-name)]
                      (assoc mod
                        :js-path (str js-root "/" js-name)
                        :last-modified (.lastModified js-file)))))
             (into []))

        order
        (->> js-pack
             (map (fn [{:keys [name js-path depends-on]}]
                    {:name name :js-path js-path :depends-on (or depends-on [])}))
             (into []))

        modules
        (reduce (fn [modules {:keys [name] :as mod}]
                  (assoc modules name mod))
          {}
          js-pack)

        css-root-fs
        (-> (io/file css-manifest)
            (.getParentFile))

        css-files
        (->> (json/read-str (slurp css-manifest))
             (reduce-kv (fn [x k v]
                          (let [css-file (io/file css-root-fs v)]
                            (assoc x k {:name k
                                        :path (str css-root "/" v)
                                        :last-modified (.lastModified css-file)})))
               {}))]

    (reset! state {:order order
                   :modules modules
                   :css-files css-files
                   :loaded-at now})

    assets))

(defn ^String html-head
  "returns a string to be included in the html <head> of your page"
  [{:keys [package-name] :as env} css-files]
  (html
    (for [file-id css-files
          :when file-id]
      [:link {:href (css-path env file-id) :rel "stylesheet" :data-css-module file-id :data-css-package package-name}])
    [:script {:type "text/javascript"} "var _sqs = new Date().getTime();"]))

(def known-dom-refs
  #{:none
    :self
    :parent
    :next-sibling
    :previous-sibling})

(defn ^String js-queue [ref func & args]
  "returns a string calling the given js-function once the module it belongs to is loaded"
  (let [js-fun (-> (str func)
                   (str/replace #"/" ".")
                   (str/replace #"-" "_"))]
    (when-not (contains? known-dom-refs ref)
      (throw (ex-info "invalid dom ref for js queue" {:ref ref :func func})))

    (html [:script {:type "shadow/run" :data-ref (name ref) :data-fn js-fun}
           (when args
             (pr-str args))])))

(defn ^String html-body
  "returns a string to be included in the html just before </body>"
  [{:keys [state] :as assets} mods]
  (let [{:keys [order modules]} @state
        mods (set mods)]
    (when-not (every? #(contains? modules %) mods)
      (throw (ex-info "invalid js module, not defined in manifest" {:known (keys modules)
                                                                    :requested mods})))

    (html
      (let [mods-to-load (reduce (fn [load {:keys [name] :as mod}]
                                   (if (contains? mods name)
                                     (conj load mod)
                                     load))
                           []
                           order)]
        (for [mod mods-to-load]
          [:script (cond-> {:type "text/javascript"
                            :src (:js-path mod)}
                     ;; FIXME: async might cause issues?
                     (= 0 (count mods-to-load))
                     (assoc :async true))]))
      )))

(defn last-modified-set [files]
  (set (map #(.lastModified ^File %) files)))

(defn watch-thread-fn
  [{:keys [keep-running css-manifest js-manifest] :as assets}]
  (let [files (vector (io/file css-manifest)
                (io/file js-manifest))]

    (try
      (loop [last-mod (last-modified-set files)]
        (if (or (not @keep-running)
                (.isInterrupted (Thread/currentThread)))
          false ;; just exit
          (do (Thread/sleep 500)
              (let [new-mod (last-modified-set files)]
                (if (not= last-mod new-mod)
                  (do (load-manifest assets)
                      (recur new-mod))
                  ;; no mod
                  (recur last-mod)
                  )))))
      (catch InterruptedException e
        false))))

(defn watch-manifests [assets]
  (assoc assets
    :watch-thread
    (doto (Thread. #(watch-thread-fn assets) "asset-watcher")
      (.setDaemon true)
      (.start))))


(defn start
  [{:keys [css-manifest css-root
           js-manifest js-root]
    :as config}]

  (-> config
      (assoc :state (atom {})
             :keep-running (atom true))
      (load-manifest)
      (watch-manifests)))

(defn stop [{:keys [watch-thread server keep-running] :as assets}]
  (reset! keep-running false)
  (when server
    (server))
  (.interrupt watch-thread)
  (.join watch-thread 600))
