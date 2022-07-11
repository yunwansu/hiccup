(ns hiccup.core
  {:deprecated "2.0"}
  (:require [hiccup2.core :as hiccup2]
            [hiccup.util :as util]))

(defmacro html [options & content]
  (if (map? options)
    `(str (hiccup2/html ~(assoc options :escape-string? false) ~@content))
    `(str (hiccup2/html {:escape-string? false} ~options ~@content))))

(defn h
  {:deprecated "2.0"}
  [text]
  (if util/*escape-strings?*
    (util/as-str text)
    (util/escape-html text)))

