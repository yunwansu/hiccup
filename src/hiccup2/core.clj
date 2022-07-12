(ns hiccup2.core
  {:added "2.0"}
  (:require [hiccup.compiler :as compiler]
            [hiccup.util :as util]))

(defmacro html
  {:added "2.0"}
  [options & content]
  (if (map? options)
    (let [mode (:mode options :xhtml)
          escape-strings? (:escape-strings? options true)]
      `(binding [util/*html-mode* ~mode
                 util/*escape-strings?* ~escape-strings?]
         (util/raw-string ~(apply compiler/compile-html-with-bindings content))))
    `(util/raw-string ~(apply compiler/compile-html-with-bindings options content))))

(def ^{:added "2.0"} raw util/raw-string)
