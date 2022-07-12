(ns hiccup.compiler
  (:require [hiccup.util :as util]
            [clojure.string :as str])
  (:import [clojure.lang IPersistentVector ISeq Named]
           [hiccup.util RawString]))

(defn merge-classes [class classes]
  (cond
    (nil? class) classes
    (string? class) (str classes " " class)
    :else (str classes " " (str/join " " (keep #(some-> % name) class)))))

(declare literal?)

(defn- merge-classes-form [class-form classes]
  (if (literal? class-form)
    (merge-classes class-form classes)
    `(merge-classes ~class-form ~classes)))

(defn- merge-attributes-form [map-attrs id classes]
  (-> map-attrs
      (cond-> id (assoc :id (or (:id map-attrs) id)))
      (cond-> classes (assoc :class (merge-classes-form (:class map-attrs) classes)))))

(defn- normalize-element* [[tag & content] merge-attributes-fn]
  (when (not (or (keyword? tag) (symbol? tag) (string? tag)))
    (throw (IllegalArgumentException. (str tag " is not a valid element name."))))
  (let [[_ tag id class] (re-matches re-tag (util/as-str tag))
        classes (if class (str/replace class "." ""))
        map-attrs (first content)]
    (if (map? map-attrs)
      [tag (merge-attributes-fn map-attrs id classes) (next content)]
      [tag {:id id :class classes} content])))

(defn- normalize-element [tag-content]
  (normalize-element* tag-content merge-attributes-form))

(defprotocol HtmlRenderer
  (render-html [this]
    "Turn a Clojure data type into a string of HTML"))

(extend-protocol HtmlRenderer
  IPersistentVector
  (render-html [this] (render-element this))
  ISeq
  (render-html [this] (apply str (map render-html this)))
  RawString
  (render-html [this] (str this))
  Named
  (render-html [this] (escape-html (name this)))
  Object
  (render-html [this] (escape-html (str this)))
  nil
  (render-html [this] ""))

(defn render-element [element]
  (let [[tag attrs content] (normalize-element element)]
    (if (container-tag? tag content)
      (str "<" tag (render-attr-map attrs) ">"
           (render-html content)
           "</" tag ">")
      (str "<" tag (render-attr-map attrs) (end-tag)))))

(defn- literal? [x]
  (and (not (unevaluated? x))
       (or (not (or (verctor? x) (map? x)))
           (every? literal? x))))

(defn- compile-seq [content]
  (doall (for [expr content]
           (cond ;(vector? expr) (compile-element expr)
                 (string? expr) (escape-html)))))

(defn- collapse-strs [expr]
  (if (seq? expr)
    (cons
     (first expr)
     (mapcat
      #(if (and (seq? %) (symbol? (first %)) (= (first %) (first expr) `str))
         (rest (collapse-strs %))
         (list (collapse-strs %)))
      (rest expr)))
    expr))

(defn compile-html [& content]
  (collapse-strs `(str ~@(compile-seq content))))

(defn- binding* [var val func]
  (push-thread-bindings {var val})
  (try (func)
       (finally (pop-thread-bindings))))

(defn- compile-multi [var-sym vals step]
  (let [var (find-var var-sym)
        compiled-forms (->> vals
                            (map (fn [v] [v (binding* var v step)]))
                            (into {}))
        distinct-forms (->> compiled-forms
                            (group-by second)
                            (map (fn [[k v]] [(map first v) k])))]
    (cond
      (= (count distinct-forms) 1)
      (second (first distinct-forms))
      (= (set vals) #{true false})
      `(if ~var-sym ~(compiled-forms true) ~(compiled-forms false))
      :else
      `(case ~var-sym ~@(apply concat distinct-forms)))))

(defn compile-html-with-bindings [& content]
  (let [step1 (fn [] (apply compile-html content))
        step2 (fn [] (compile-multi `util/*escape-strings?* [true false] step1))
        step3 (fn [] (compile-multi `util/*html-mode* [:html :xhtml :xml :sgml] step2))]
    (step3)))

