(ns malli.coercer
  (:require [malli.core :as m]))

;;; For each alternative (usually one, but more on :or, :maybe etc.):
;;;
;;; 1. x = `enter` on data node
;;; 2. Shallowly validate x, return ::m/invalid on false
;;; 3. x = Map children of x
;;; 4. x = `leave` on x
;;;
;;; If any part returns ::m/invalid, skip to next alternative or if none left return the ::m/invalid.
;;;
;;; The interceptor and shallow validator are flattened from all the layers like :maybe, :schema, :ref
;;; etc. that surround a "concrete" (collection or scalar) schema as well as that of the concrete schema.
;;; The children mapper is pulled from just the concrete schema.

;;; But what about :and? It is wrong in existing code (and in Spec), but is there a right way at all:
;;; (m/parse [:and
;;;          [:or* [:n int?] [:s double?]]
;;;          pos?]
;;;         5)
;;; Execution error (ClassCastException) at malli.core/-simple-schema$reify$reify$fn (core.cljc:319).
;;; clojure.lang.MapEntry cannot be cast to java.lang.Number
;;;
;;; The corresponding Plumatic Schema (`both`) is deprecated, just like with :or (`either`).

(defn- shallow-validator [s]
  (case (m/type s)
    :and (let [validators (distinct (map shallow-validator (m/children s)))]
           (if (second validators) (apply every-pred validators) (first validators)))

    :or (let [validators (distinct (map shallow-validator (m/children s)))]
          (if (second validators) (fn [x] (boolean (some #(% x) validators))) (first validators)))

    :maybe (let [validate (shallow-validator (m/-get s 0 nil))]
             (fn [x] (or (nil? x) (validate x))))

    :sequential sequential?
    :int (m/-validator s)))

(declare coercer)

(defn- children-mapper [s node-interceptor options]
  (case (m/type s)
    :and (let [mappers (map #(children-mapper % node-interceptor options) (m/children s))]
           (fn [x] (reduce (fn [x convert] (convert x)) x mappers)))

    :or (let [mappers (map #(children-mapper % node-interceptor options) (m/children s))]
          (fn [x]
            (reduce (fn [acc convert]
                      (let [x (convert x)]
                        (if (m/-invalid? x)
                          acc
                          (reduced x))))
                    ::m/invalid mappers)))

    :maybe (let [convert-some (children-mapper (m/-get s 0 nil) node-interceptor options)]
             (fn [x] (if (nil? x) x (convert-some x))))

    :sequential (let [convert-child (coercer (m/-get s 0 nil) node-interceptor options)]
                  (fn [coll]
                    (reduce (fn [acc x]
                              (let [x (convert-child x)]
                                (if (m/-invalid? x)
                                  (reduced x)
                                  (conj acc x))))
                            (empty coll) coll)))

    :int identity))

(defn coercer [s node-interceptor options]
  (letfn [(coercer [s]
            (let [{:keys [enter leave]} (node-interceptor s)
                  enter (or enter identity)
                  leave (or leave identity)
                  validate-node (shallow-validator s)
                  coerce-children (children-mapper s node-interceptor options)]
              (fn [x]
                (let [x (enter x)]
                  (if-not (m/-invalid? x)
                    (if (validate-node x)
                      (let [x (coerce-children x)]
                        (if-not (m/-invalid? x)
                          (leave x)
                          x))
                      ::m/invalid)
                    x)))))]
    (coercer (m/schema s))))

(defn- node-parser [s]
  (case (m/type s)
    :and (let [itors (mapv node-parser (m/children s))]
           {:enter (apply m/-comp (map #(get % :enter identity) (rseq itors)))
            :leave (apply m/-comp (map #(get % :leave identity) itors))})

    :maybe (let [{:keys [enter leave]} (node-parser (m/-get s 0 nil))
                 enter (or enter identity)
                 leave (or leave identity)]
             {:enter (fn [x] (if (nil? x) x (enter x)))
              :leave (fn [x] (if (nil? x) x (leave x)))})

    :sequential nil
    :int nil))
