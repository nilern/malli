(ns malli.clj-kondo
  (:require [clojure.java.io :as io]
            [malli.core :as m]))

(declare convert)

(defn emit-config [config]
  (let [cfg-file (io/file ".clj-kondo" "configs" "malli" "config.edn")]
    (io/make-parents cfg-file)
    (spit cfg-file config)
    config))

(def mappings
  {'int? (constantly :int)
   :int (constantly :int)
   'pos-int? (constantly :pos-int)
   'any? (constantly :any)
   :tuple (fn [x] (mapv convert (m/children x)))})

(defn convert [x]
  ((get mappings (m/type x) (constantly :any)) x))

(defn from-var [x]
  (if-not (:malli/fn (meta x))
    (m/-fail! ::invalid-target)
    (let [{:keys [schema ns name]} (meta x)]
      (let [ns-name (-> ns str symbol)
            schema (m/schema schema)]
        (assert (= :or (m/type schema)))
        (reduce
          (fn [acc schema]
            (let [[input return] (m/children schema)
                  args (mapv convert (m/children input))
                  ret (convert return)
                  arity (count args)]
              (conj
                acc
                {:ns ns-name
                 :name name
                 :arity arity
                 :args args
                 :ret ret})))
          [] (m/children schema))))))

(defn from-ns [ns]
  (->> ns (ns-publics) (vals) (filter #(-> % meta :malli/fn))
       (reduce (fn [acc x] (into acc (from-var x))) [])))

(defn linter-config [xs]
  (reduce
    (fn [acc {:keys [ns name arity args ret]}]
      (assoc-in
        acc [:linters :type-mismatch :namespaces ns name :arities arity]
        {:args args, :ret ret})) {} xs))
