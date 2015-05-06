(ns whats-next.firebase)

(def url "https://shining-inferno-5361.firebaseio.com/")

(defonce firebase-ref (future (js/Firebase. url)))
