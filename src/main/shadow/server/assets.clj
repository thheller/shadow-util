(ns shadow.server.assets
  (:import [java.io File])
  (:require [hiccup.core :as hiccup :refer (html)]
            [clojure.pprint :refer (pprint)]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [clojure.java.io :as io]))



(defn load-manifest
  [{:keys [js-manifest
           css-manifest
           state] :as assets}]

  (prn [:loading-manifest])
  (let [now
        (System/currentTimeMillis)

        js-manifest-file
        (io/file js-manifest)

        js-modules
        (if-not (.exists js-manifest-file)
          []
          (-> js-manifest-file
              (slurp)
              (json/read-str :key-fn keyword)))

        css-manifest-file
        (io/file css-manifest)

        css-files
        (if-not (.exists css-manifest-file)
          {}
          (-> css-manifest-file
              (slurp)
              (json/read-str)))]

    (reset! state {:js-modules js-modules
                   :css-files css-files

                   :js-timestamp
                   (when (.exists js-manifest-file)
                     (.lastModified js-manifest-file))

                   :css-timestamp
                   (when (.exists css-manifest-file)
                     (.lastModified css-manifest-file))

                   :loaded-at now})

    assets))

(defn ^String html-head
  "returns a string to be included in the html <head> of your page"
  [{:keys [css-root package-name state] :as env} files-to-include]
  (let [{:keys [css-files]} @state]

    (html
      (for [file-id files-to-include
            :when file-id
            :let [filename
                  (if-let [filename (get css-files file-id)]
                    filename
                    (throw (ex-info "no css file by id" {:file-id file-id :css-files css-files})))]]

        [:link {:href (str css-root "/" filename) :rel "stylesheet" :data-css-module file-id :data-css-package package-name}]))))

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

(defn ^String html-js-preload
  "returns a string to be included in the html head as close to the top as possible

   emits <link rel='preload' as='script'> elements"
  [{:keys [js-root state] :as assets} mods]
  (let [{:keys [js-modules]}
        @state

        mods-to-link
        (into #{} mods)]

    (html
      (for [{:keys [name js-name foreign] :as js-mod} js-modules
            :when (contains? mods-to-link name)]
        (html
          (when (seq foreign)
            (for [foreign-lib foreign]
              [:link {:rel "preload"
                      :as "script"
                      :href (str js-root "/" (:js-name foreign-lib))}]))

          [:link {:rel "preload"
                  :as "script"
                  :href (str js-root "/" js-name)}]))
      )))

(defn ^String html-body
  "returns a string to be included in the html just before </body>"
  [{:keys [js-root state] :as assets} mods-to-load]
  (let [{:keys [js-modules]}
        @state

        mods-to-load
        (into #{} mods-to-load)]

    (html
      (for [{:keys [name js-name foreign] :as mod} js-modules
            :when (contains? mods-to-load name)]
        (html
          (when (seq foreign)
            (for [foreign-lib foreign]
              [:script {:type "text/javascript" :src (str js-root "/" (:js-name foreign-lib))}]))

          [:script {:type "text/javascript"
                    :src (str js-root "/" js-name)}]))
      )))

(defn watch-thread-fn
  [{:keys [keep-running css-manifest js-manifest state] :as assets}]
  (let [css-manifest-fs
        (io/file css-manifest)

        js-manifest-fs
        (io/file js-manifest)]

    (try
      (loop []
        (if (or (not @keep-running)
                (.isInterrupted (Thread/currentThread)))
          false ;; just exit
          (do (Thread/sleep 500)
              (let [css-mod
                    (when (.exists css-manifest-fs)
                      (.lastModified css-manifest-fs))

                    js-mod
                    (when (.exists js-manifest-fs)
                      (.lastModified js-manifest-fs))

                    {:keys [css-timestamp
                            js-timestamp]}
                    @state]

                (when (or (not= js-mod js-timestamp)
                          (not= css-mod css-timestamp))

                  ;; reload manifest if either changes
                  ;; FIXME: seperate js/css manifests?
                  (load-manifest assets))

                (recur)))))
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
