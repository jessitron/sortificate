(ns sortificate.core-test
  (:require [clojure.test :refer :all]
            [sortificate.core :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]i
            [clojure.algo.monads :as m]))
(m/defmonad gen-m
    [m-bind gen/bind
     m-result gen/return])

(def mygen-direction-of-sort (gen/elements [:ascending :descending]))

(defn mygen-non-empty-set [elem-g] (gen/such-that seq (gen/fmap set (gen/vector elem-g))))

(def maps-and-sort-instructions
  (m/domonad
    gen-m
     [set-of-keys  (mygen-non-empty-set gen/keyword)
      set-of-value-generators  (gen/vector  (gen/elements  [gen/string gen/int]) (count set-of-keys))
      some-maps (gen/vector (apply gen/hash-map (mapcat vector set-of-keys set-of-value-generators)))
      some-keys (mygen-non-empty-set (gen/elements set-of-keys))
      some-directions (gen/vector mygen-direction-of-sort (count some-keys))
     ]
     (let [instructions (map vector some-keys some-directions)]
       [some-maps instructions])))


;; just the property, no "is"
(def sort-instructions-are-compatible-with-maps
  (prop/for-all
    [[rows instructions] maps-and-sort-instructions]
    (every? identity (for [[k direction] instructions]
              (and (#{:ascending :descending} direction)
                   (every? k rows))))))

(defspec my-generator-works 20
  sort-instructions-are-compatible-with-maps )

;; now can I write the real spec? maybe??
(def a-real-spec
  (prop/for-all
    [[rows instructions] maps-and-sort-instructions]
    true
    ;; damn. out of brainpower.
    ))
