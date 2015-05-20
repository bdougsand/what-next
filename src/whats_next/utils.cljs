(ns whats-next.utils
  (:require [cljs.core.async :refer [>! chan put! sliding-buffer]])
  (:import [goog.i18n DateTimeFormat DateTimeSymbols]))

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

;; Channel helpers
(defn interval-chan [ms]
  (let [c (chan (sliding-buffer 1))
        i (atom 0)
        itvl (atom nil)]
    (reset! itvl
            (js/setInterval (fn []
                              (when-not (put! c (swap! i inc))
                                (js/clearInterval @itvl)))
                            ms))
    c))

;; Function helpers
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
(def ms-in-day 86400000)

(defn date? [x]
  (instance? js/Date x))

(defn ->date [i]
  (if (instance? js/Date i)
    i
    (js/Date. i)))

(defn ->stamp [x]
  (if (instance? js/Date x)
    (.getTime x)
    x))

(def day-components
  (juxt #(.getFullYear %) #(inc (.getMonth %)) #(.getDate %)))

(defn same-day? [d dr]
  (= (day-components d)
     (day-components dr)))

(defn same-month? [d dr]
  (and (= (.getMonth d) (.getMonth dr))
       (= (.getYear d) (.getYear dr))))

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

(defn start-of-week
  [d]
  (js/Date. (- (.getTime (start-of-day d))
               (* ms-in-day (.getDay d)))))

(defn start-of-month
  "Returns a new Date object to midnight on the first day of the same
  month and year as the given Date d."
  [d]
  (js/Date. (.getFullYear d) (.getMonth d)))

(defn now [] (js/Date.))

(defn yesterday
  "Returns a Date for the end of the day yesterday."
  []
  (js/Date. (dec (.getTime (start-of-day (now))))))

(defn inc-date
  ([d days]
   (js/Date. (+ (.getTime d) (* days ms-in-day))))
  ([d]
   (inc-date d 1)))

(defn dec-date
  [d days]
  (js/Date. (- (.getTime d) (* days ms-in-day))))

(defn date-range [d1 d2]
  (take-while #(< (.getTime %) (.getTime d2))
              (iterate inc-date d1)))

(defn date-range-len [d1 days]
  (let [d2 (js/Date. (+ (.getTime days) (* days 86400000)))]
    (if (pos? days)
      (date-range d1 d2)
      (date-range d2 d1))))

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

(def short-day-names
  (js->clj (aget DateTimeSymbols "SHORTWEEKDAYS")))

(defn day-name [d]
  (nth day-names (.getDay d)))

(def time-formatter
  (DateTimeFormat. "H:mm a"))

(def date-formatter
  (DateTimeFormat. "MMMM d"))

(defn pretty-date [d]
  (.format date-formatter d))

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
        (.format time-formatter d)))))
  ([d]
   (pretty-relative-date d (js/Date.))))

(defn insert-where [f x coll]
  (concat (take-while f coll)
          (cons x (drop-while f coll))))
