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

;; Something like this may already exist in the Closure library
(defn pretty-duration-med [ms]
  (let [s (floor (/ ms 1000))
        h (floor (/ s 3600))
        m (floor (/ (mod s 3600) 60))
        s (mod s 60)]
    (if (pos? h)
      (str h "h"
           (when (pos? m) (str ", " m "m")))
      (str m "m"
           (when (pos? s) (str ", " s "s"))))))

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

;; Transducers
(defn staggered
  "Creates a transducer that calls f with the previous step's input and
  the current step's input."
  [f]
  (fn [xf]
    (let [last (volatile! nil)]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (let [last-val @last]
           (vreset! last input)
           (xf result (f last-val input))))))))

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

(defn ->date
  ([i]
   (if (instance? js/Date i)
     i
     (js/Date. i)))
  ([y m d]
   (js/Date. y (dec m) d)))

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

(declare dec-date inc-date start-of-day)
(defn yesterday?
  "Is date d yesterday with respect to dr?"
  [d dr]
  (= (start-of-day d)
     (dec-date (start-of-day dr))))


(defn tomorrow?
  "Is Date d 'tomorrow' with respect to dr?"
  [d dr]
  (= (start-of-day d)
     (inc-date (start-of-day dr))))

(defn recent? [d dr]
  (> (* 86400000 5) (- (->stamp dr) (->stamp d))))

(defn start-of-day
  "Returns a new Date object with the same day, month, and year as Date
  d and the time set to midnight."
  [d]
  (js/Date. (.getFullYear d) (.getMonth d) (.getDate d)))

(defn end-of-day [d]
  (js/Date. (dec (.getTime (js/Date. (.getFullYear d) (.getMonth d) (inc (.getDate d)))))))

(defn start-of-week
  [d]
  (js/Date. (- (.getTime (start-of-day d))
               (* ms-in-day (.getDay d)))))

(defn start-of-month
  "Returns a new Date object to midnight on the first day of the same
  month and year as the given Date d."
  [d]
  (js/Date. (.getFullYear d) (.getMonth d)))

(defn start-of-year
  [d]
  (js/Date. (.getFullYear d) 0 1))

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

(defn date-range
  "Returns a lazy sequence of Date objects, starting at Date d1 and
  incrementing by one day up to but not including d2."
  [d1 d2]
  (take-while #(< (.getTime %) (.getTime d2))
              (iterate inc-date d1)))

(defn date-range-len [d1 days]
  (let [d2 (js/Date. (+ (.getTime days) (* days 86400000)))]
    (if (pos? days)
      (date-range d1 d2)
      (date-range d2 d1))))

(defn inc-week
  ([d & [n]]
   (js/Date. (+ (.getTime d) (* 86400000 7 (or n 1)))))
  ([]
   (inc-week (now))))

(defn inc-month [d & [n]]
  ;; This works even if the month of d is December
  (js/Date. (.getFullYear d) (+ (.getMonth d) (or n 1))))

(defn inc-year [d & [n]]
  (js/Date. (+ (or n 1) (.getFullYear d)) (.getMonth d)))

(defn inc-type [d unit n]
  ((case unit
     :day inc-date
     :week inc-week
     :month inc-month
     :year inc-year) d n))

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
  (DateTimeFormat. "h:mm a"))

(defn pretty-time [d]
  (.format time-formatter d))

(def date-formatter
  (DateTimeFormat. "MMMM d"))

(defn pretty-date [d]
  (.format date-formatter d))

(defn pretty-relative-date
  "Returns a string describing the Date d with respect to Date dr."
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

(defn ms-to-hour
  "Returns the number of milliseconds until the next hour after the
  given Date d."
  ([d]
   (+ (* (- 60 (.getMinutes d)) 60000)
      (* (- 60 (.getSeconds d)) 1000)
      (- 1000 (.getMilliseconds d))))
  ([]
   (ms-to-hour (js/Date.))))

(defn ms-to-day
  ([d]
   (+ (ms-to-hour d)
      (* (- 24 (.getHours d)) 3600000)))
  ([]
   (ms-to-day (js/Date.))))

(defn insert-where [f x coll]
  (concat (take-while f coll)
          (cons x (drop-while f coll))))

(defn cancel [e]
  (doto e (.stopPropagation) (.preventDefault)))

(defn commas
  "Returns a comma-separated list string of the strings in coll."
  ([coll conjunction]
   (let [init (butlast coll)
         end (last coll)]
     (str (when init
            (str (apply str (interpose ", " init))
                 (when (second init) ",")
                 " " conjunction " "))
          end)))
  ([coll]
   (commas coll "and")))
