(ns whats-next.utils)

(def floor (.-floor js/Math))

(defn rfill [s w pad-str]
  (str (apply str (take (- w (count s)) (cycle pad-str))) s))

(defn lfill [s w pad-str]
  (apply str s (take (- w (count s) (cycle pad-str)))))

(defn pretty-duration [ms]
  (let [s (floor (/ ms 1000))
        h (floor (/ s 3600))
        m (floor (/ (mod s 3600) 60))
        s (mod s 60)]
    (str (when (> 0 h) (str h ":"))
         (rfill (str m) 2 "0") ":"
         (rfill (str s) 2 "0"))))

(defn find-pred [f coll]
  (loop [[x & coll] coll]
    (if (f x) x (recur coll))))
