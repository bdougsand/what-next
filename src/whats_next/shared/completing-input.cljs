(ns whats-next.shared.completing-input
  (:require [cljs.core.async :refer [<! chan close! put! timeout sliding-buffer]]

            [om.core :as om]
            [om.dom :as dom]

            [goog.events.KeyCodes :as kc])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defprotocol CompletionSource
  (completions [this s]))

(defn coll-complete
  "Expects a collection of strings and a string to use as a match."
  [coll s]
  (let [s (.toLowerCase s)]
    (go (filter #(= (.indexOf (.toLowerCase %) s) 0) coll))))

(defn completer [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:input-chan (sliding-buffer (chan))
       :text nil})

    om/IWillMount
    (will-mount [_]
      (let [c (om/get-state owner :input-chan)
            names (into #{} (map :type) (:work app))]
        (go-loop []
          (when-let [n (<! c)]
            (om/set-state! owner :completion
                           (first (filter #(= (.indexOf n %) 0) names)))
            (recur)))))

    om/IWillUnmount
    (will-unmount [_]
      (close! (om/get-state owner :input-chan)))

    om/IRenderState
    (render-state [_ {:keys [completion input-chan text]}]
      (dom/div #js {:className "completing-input"}
               (when completion
                 (dom/input #js {:readonly true
                                 :type "text"
                                 :style #js {:color "lightgray"
                                             :position "absolute"}
                                 :value completion}))
               (dom/input #js {:type "text"
                               :value text
                               :onChange (fn [e]
                                           (let [val (.. e -target -value)]
                                             (om/set-state! owner :text val)
                                             (when val
                                               (put! input-chan val))))
                               :onKeyDown (fn [e]
                                            (when (= (.-keyCode e) kc/ENTER)
                                              (om/set-state! owner :text completion)
                                              (om/set-state! owner :completion nil)))})))))
