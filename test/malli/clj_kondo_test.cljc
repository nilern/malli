(ns malli.clj-kondo-test
  (:require [clojure.test :refer [deftest is]]
            [malli.clj-kondo :as clj-kondo]))

(deftest clj-kondo-integration-test
  (is (= {:op :keys
          :opt {::price :double
                ::y :boolean
                :tags :set}
          :req {::id :string
                :name :string
                :nested {:op :keys
                         :req {:z :string}}
                :z :vector}}
         (clj-kondo/transform
           [:map {:registry {::id string?
                             ::price double?}}
            ::id
            [::price {:optional true}]
            [:name string?]
            [:tags {:optional true} [:set qualified-keyword?]]
            [::y {:optional true} boolean?]
            [:nested [:map [:z ::id]]]
            [:z [:vector [:map-of int? int?]]]]))))


