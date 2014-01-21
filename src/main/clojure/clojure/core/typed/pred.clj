(ns clojure.core.typed.pred
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.current-impl :as impl]
            [clojure.set :as set]))

(defn type->pred 
  "Returns syntax representing a runtime predicate on the
  given type."
  [t]
  {:pre [(r/Type? t)]}
  (letfn [(gen-inner [t arg]
            (cond
              (r/F? t) (u/int-error "Cannot generate predicate for free variable")
              (r/Poly? t) (u/int-error "Cannot generate predicate for polymorphic type")
              (r/PolyDots? t) (u/int-error "Cannot generate predicate for dotted polymorphic type")
              (r/FnIntersection? t) (u/int-error "Cannot generate predicate for function type")
              (r/Name? t) (recur (c/-resolve t) arg)
              (r/RClass? t) (cond
                              (empty? (:poly? t)) `(instance? ~(:the-class t) ~arg)
                              :else (u/int-error (str "Cannot generate predicate for polymorphic Class")))
              (r/Union? t) `(or ~@(mapv gen-inner (:types t) (repeat arg)))
              (r/Intersection? t) `(and ~@(mapv gen-inner (:types t) (repeat arg)))
              (r/HeterogeneousVector? t) `(and (vector? ~arg)
                                               ~(cond
                                                  (:rest t)
                                                    `(<= ~(count (:types t)) (count ~arg))
                                                  (:drest t)
                                                    (u/int-error (str "Cannot generate predicate for dotted HVec"))
                                                  :else
                                                    `(== ~(count (:types t)) (count ~arg)))
                                               ~@(doall
                                                   (map-indexed 
                                                     (fn [i t*]
                                                       (let [vlocal (gensym "vlocal")]
                                                         `(let [~vlocal (nth ~arg ~i)]
                                                            ~(gen-inner t* vlocal))))
                                                     (:types t)))
                                               ~@(when (:rest t)
                                                   (let [nfixed (count (:types t))]
                                                     [`(let [rstvec# (subvec ~arg ~nfixed)]
                                                         (every? ~(let [vlocal (gensym "vlocal")]
                                                                    `(fn [~vlocal] 
                                                                       ~(gen-inner (:rest t) vlocal)))
                                                                 rstvec#))])))
              (r/CountRange? t) (let [cnt (gensym "cnt")]
                                  `(let [~cnt (count ~arg)]
                                     (<= ~@(let [{:keys [lower upper]} t]
                                             (concat [lower cnt]
                                                     (when upper
                                                       [upper]))))))
              (r/Value? t) (cond
                             (nil? t) `(nil? ~arg)
                             (symbol? t) `(= '~t ~arg)
                             (keyword? t) `(identical? '~t ~arg)
                             (number? t) `(when (number? ~arg)
                                            (== '~t ~arg))
                             :else (u/int-error 
                                     (str "Cannot generate predicate for value type: " t)))
              (r/HeterogeneousMap? t) `(and (map? ~arg)
                                            ; check keys match
                                            ~(let [req-ks (set (keys (:types t)))
                                                   absent-ks (set (:absent-keys t))
                                                   _ (when-not (every? c/keyword-value? (concat req-ks absent-ks))
                                                       (u/int-error 
                                                         (str "HMap keys must be keywords")))]
                                               (cond
                                                 (c/complete-hmap? t)
                                                   `(= (set (keys ~arg))
                                                       ~req-ks)
                                                 :else
                                                   `(let [actual-ks# (set (keys ~arg))]
                                                      (and 
                                                        ;required keys are a subset of actual keys
                                                        (set/subset? 
                                                          ~req-ks
                                                          actual-ks#)
                                                        ;no absent-keys are present
                                                        (empty?
                                                          (set/intersection
                                                            ~req-ks
                                                            actual-ks#))))))
                                            ;check values match
                                            ~@(mapv (fn [[k v]]
                                                      {:pre [(c/keyword-value? k)]}
                                                      (let [klocal (gensym "klocal")]
                                                        `(let [~klocal (~(:val k) ~arg)]
                                                           ~(gen-inner v klocal))))
                                                    (:types t)))

              :else (u/int-error (str (class t) " not supported in type->pred: " t))))]
    (let [arg (gensym "arg")]
      `(fn [~arg] 
         (boolean
           ~(gen-inner t arg))))))

(comment
(impl/with-clojure-impl
  (type->pred (prs/parse-type 'Number)))

(impl/with-clojure-impl
  (type->pred (prs/parse-type ''[Number])))

(clojure.pprint/pprint
  (impl/with-clojure-impl
    (type->pred (prs/parse-type ''[Number Number *]))))

(clojure.pprint/pprint
  (impl/with-clojure-impl
    (type->pred (prs/parse-type '(HMap :mandatory {:a Number})))))
(clojure.pprint/pprint
  (impl/with-clojure-impl
    (type->pred (prs/parse-type '(HMap :optional {:a Number :b Number})))))

  )
