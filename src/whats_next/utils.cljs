(ns whats-next.utils
  (:require [cljs.core.async :refer [>! chan put! sliding-buffer]])
  (:import [goog.i18n DateTimeSymbols]))

;; String Utilities
(defn rfill [s w pad-str]
  (str (apply str (take (- w (count s)) (cycle pad-str))) s))

(defn lfill [s w pad-str]
  (apply str s (take (- w (count s) (cycle pad-str)))))

(def floor (.-floor js/Math))

(defn pretty-duration [ms]
  (let [s (floor (/ ms 1000))
        h (floor (/ s 3600))
        m (floor (/ (mod s 3600) 60))
        s (mod s 60)]
    (str (when (> h 0) (str h ":"))
         (rfill (str m) 2 "0") ":"
         (rfill (str s) 2 "0"))))

(defn pretty-duration-long [ms]
  (let [s (floor (/ ms 1000))
        h (floor (/ s 3600))
        m (floor (/ (mod s 3600) 60))
        s (mod s 60)]
    (if (pos? h)
      (str h " hour" (when (> h 1) "s")
           (when (pos? m) (str ", " m "minute" (when (< m 1) "s"))))
      (str m " minute" (when (not= m 1) "s")
           (when (pos? s) (str ", " s " second" (when (> s 1) "s")))))))

(defn pretty-ago [ms]
  (if (< ms 60000)
    "just now"
    (let [m (floor (/ ms 60000))]
      (str m " minute" (when (not= m 0) "s") " ago"))))

(defn find-pred [f coll]
  (loop [[x & xs] coll]
    (when x
      (if (f x) x (recur xs)))))

(defn interval-chan [ms]
  (let [c (chan (sliding-buffer 1))
        i (atom 0)]
    (js/setInterval (fn [] (put! c (swap! i inc))) ms)
    c))

(defn memo-last [f]
  (let [m (atom nil)
        last (atom nil)]
    (fn [x]
      (if (= @last x)
        @m
        (do
          (reset! last x)
          (reset! m (f x)))))))


;; Date helpers:
(defn now [] (js/Date.))

(defn ->date [i]
  (if (instance? js/Date i)
    i
    (js/Date. i)))

(defn ->stamp [x]
  (if (instance? js/Date x)
    (.valueOf x)
    x))

(def day-components
  (juxt #(.getFullYear %) #(inc (.getMonth %)) #(.getDate %)))

(defn same-day? [d dr]
  (= (day-components d)
     (day-components dr)))

(defn yesterday? [d dr]
  (and (= (.getFullYear d) (.getFullYear dr))
       (= (.getMonth d) (.getMonth dr))
       (= (.getDate d) (dec (.getDate dr)))))

(defn recent? [d dr]
  (> (* 86400000 5) (- (->stamp dr) (->stamp d))))

(defn start-of-day
  "Returns a new Date object with the same day, month, and year as Date
  d and the time set to midnight."
  [d]
  (js/Date. (.getFullYear d) (.getMonth d) (.getDate d)))

(defn start-of-month
  "Returns a new Date object to midnight on the first day of the same
  month and year as the given Date d."
  [d]
  (js/Date. (.getFullYear d) (.getMonth d)))

(defn inc-date [d]
  (js/Date. (+ (.valueOf d) 86400000)))

(defn inc-month [d]
  ;; This works even if the month of d is December
  (js/Date. (.getFullYear d) (inc (.getMonth d))))

(defn morning? [d]
  (< (.getHours d) 12))

(defn time-of-day [d]
  (condp > (.getHours d)
    4 :late-night
    12 :morning
    17 :afternoon
    20 :evening
    :night))

(def day-names
  (js->clj (aget DateTimeSymbols "STANDALONEWEEKDAYS")))

(defn day-name [d]
  (nth day-names (.getDay d)))

(def am-pm
  (js->clj (aget DateTimeSymbols "AMPMS")))

(defn pretty-relative-date
  "Returns a string describing the Date d with respect to "
  ([d dr]
   (let [elapsed (- (->stamp dr) (->stamp d))
         d (->date d)
         dr (->date dr)]
     (if (< elapsed 3600000)
       (pretty-ago elapsed)

       (str
        (if (recent? d dr)
          (cond
           (same-day? d dr)
           "today"

           (yesterday? d dr)
           "yesterday"

           :else
           (day-name dr)))
        " at "
        (let [h (mod (.getHours d) 12)]
          (if (= h 0) "12" h))
        ":"
        (.getMinutes d)
        " "
        (am-pm (quot (.getHours d) 12))))))
  ([d]
   (pretty-relative-date d (js/Date.))))

(defn insert-where [f x coll]
  (concat (take-while f coll)
          (cons x (drop-while f coll))))
