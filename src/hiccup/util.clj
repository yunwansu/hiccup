(ns hiccup.util
  (:require [clojure.string :as str])
  (:import java.net.URI
           java.net.URLEncoder))

(def ^:dynamic ^:no-doc *escape-strings?* true)

(def ^:dynamic ^:no-doc *base-url* nil)

(defprotocol ToString
  (^String to-str [x] "Convert a value into a string"))

(extend-protocol ToString
  clojure.lang.Keyword
  (to-str [k] (name k))
  clojure.lang.Ratio
  (to-str [r] (str (float r)))
  java.net.URI
  (to-str [u]
    (if (or (.getHost u)
            (nil? (.getPath u))
            (not (-> (.getPath u) (.startsWith "/"))))
      (str u)
      (let [base (str *base-url*)]
        (if (.endsWith base "/")
          (str (subs base 0 (dec (count base))) u)
          (str base u)))))
  Object
  (to-str [x] (str x))
  nil
  (to-str [_] ""))

(defn ^String as-str [& xs]
  (apply str (map to-str xs)))
