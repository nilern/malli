(ns malli.clj-kondo
  (:require [clojure.java.io :as io]
            [malli.core :as m]))

(declare convert)

(defn save! [config]
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

(defn from [x]
  (if-not (:malli/fn x)
    (m/-fail! ::invalid-target)
    (let [{:keys [schema ns name]} x]
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

(defn from-var [x] (from (meta x)))

(defn from=> [ns]
  (->> (m/=>schemas)
       (keep (fn [[k v]] (if (= k (symbol (str ns))) (vals v))))
       (mapcat identity)))

(defn from-ns [ns]
  (->> ns (ns-publics) (vals) (filter #(-> % meta :malli/fn))
       (reduce (fn [acc x] (into acc (from-var x))) [])
       (into (mapcat from (from=> ns)))))

(defn linter-config [xs]
  (reduce
    (fn [acc {:keys [ns name arity args ret]}]
      (-> acc
          (assoc-in
            [:linters :type-mismatch :namespaces ns name :arities arity]
            {:args args, :ret ret})))
    {:lint-as {'malli.schema/defn 'schema.core/defn}} xs))
