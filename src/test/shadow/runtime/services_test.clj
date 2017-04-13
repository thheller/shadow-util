(ns shadow.runtime.services-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer (pprint)]
            [shadow.runtime.services :as rt]))


(def app
  {:db
   {:depends-on [:config]
    :start
    (fn [config]
      (assert (= ::config config))
      ::db)

    :stop
    (fn [db]
      (prn [:stop-db])
      (assert (= db ::db)))}

   :standalone
   {:depends-on []
    :start (fn [] ::standalone)
    :stop
    (fn [s]
      (prn [:stop-standalone])
      (assert (= s ::standalone)))}

   :cms
   {:depends-on [:db]
    :start
    (fn [db]
      (assert (= ::db db))
      ::cms)

    :stop
    (fn [cms]
      (prn [:stop-cms])
      (assert (= cms ::cms)))}
   })

(deftest test-basic-comp
  (let [input
        {:config ::config}

        init
        (rt/init input app)

        started
        (rt/start-all init)

        without-cms
        (rt/stop-single started :cms)

        stopped
        (rt/stop-all started)]

    (is (= ::db (:db started)))
    (is (= ::standalone (:standalone started)))
    (is (= ::cms (:cms started)))
    (is (not (contains? without-cms :cms)))
    (is (= stopped init))
    ))
