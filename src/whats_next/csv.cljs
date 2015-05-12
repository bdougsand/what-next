(ns whats-next.csv
  (:require [clojure.string :as str]))

(defn encode-cell [s]
  (str "\"" (str/replace s #"(\")" "\\$1") "\""))

(defn row-to-csv [row]
  (str/join "," (map encode-cell row)))

(defn csv [rows]
  (str/join "\n" (map row-to-csv rows)))
