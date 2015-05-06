(ns whats-next.pouch
  (:require [cljs.core.async :refer [<! >! chan close! put!]]

            [cljsjs.pouchdb])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defonce db (js/PouchDB. #js {:name "work"}))

(defn all-docs
  ([db opts]
   (let [c (chan)]
     (.allDocs db (clj->js opts) (fn [result]
                                   (when result
                                     (put! c result))
                                   (close! c)))
     c))
  ([opts]
   (all-docs db opts))
  ([]
   (all-docs db {:include_docs true})))

(defn do-fetch [db opts]
  (let [c (chan)]
    (go
      (let [request (.allDocs db)]))))

(defn fetch [db & opts]
  (do-fetch db (clj->js opts)))
