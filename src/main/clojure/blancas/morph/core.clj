;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "The core Morph library."
      :author "Armando Blancas"}
  blancas.morph.core)


;; +-------------------------------------------------------------+
;; |                     Curried Functions.                      |
;; +-------------------------------------------------------------+


(defmacro mcf
  "Makes a curried function off the arglist and body."
  [args & body]
  (case (count args)
    0  (let [f (gensym)]
         `(fn ~f [] ~@body))
    1  (let [[x] args f (gensym)]
         `(fn ~f ([] ~f) ([~x] ~@body)))
    2  (let [[x y] args f (gensym)]
         `(fn ~f
	    ([] ~f)
	    ([~x] (fn [~y] (~f ~x ~y)))
	    ([~x ~y] ~@body)))
    3  (let [[x y z] args f (gensym)]
	 `(fn ~f
	    ([] ~f)
            ([~x] (mcf [~y ~z] ~@body))
            ([~x ~y] (fn [~z] (~f ~x ~y ~z)))
            ([~x ~y ~z] ~@body)))
    4  (let [[w x y z] args f (gensym)]
	 `(fn ~f
	    ([] ~f)
            ([~w] (mcf [~x ~y ~z] ~@body))
            ([~w ~x] (mcf [~y ~z] ~@body))
            ([~w ~x ~y] (fn [~z] (~f ~w ~x ~y ~z)))
            ([~w ~x ~y ~z] ~@body)))
    5  (let [[v w x y z] args f (gensym)]
	 `(fn ~f
	    ([] ~f)
            ([~v] (mcf [~w ~x ~y ~z] ~@body))
            ([~v ~w] (mcf [~x ~y ~z] ~@body))
            ([~v ~w ~x] (mcf [~y ~z] ~@body))
            ([~v ~w ~x ~y] (fn [~z] (~f ~v ~w ~x ~y ~z)))
            ([~v ~w ~x ~y ~z] ~@body)))
    6  (let [[u v w x y z] args f (gensym)]
	 `(fn ~f
	    ([] ~f)
            ([~u] (mcf [~v ~w ~x ~y ~z] ~@body))
            ([~u ~v] (mcf [~w ~x ~y ~z] ~@body))
            ([~u ~v ~w] (mcf [~x ~y ~z] ~@body))
            ([~u ~v ~w ~x] (mcf [~y ~z] ~@body))
            ([~u ~v ~w ~x ~y] (fn [~z] (~f ~u ~v ~w ~x ~y ~z)))
            ([~u ~v ~w ~x ~y ~z] ~@body)))
    7  (let [[t u v w x y z] args f (gensym)]
	 `(fn ~f
	    ([] ~f)
            ([~t] (mcf [~u ~v ~w ~x ~y ~z] ~@body))
            ([~t ~u] (mcf [~v ~w ~x ~y ~z] ~@body))
            ([~t ~u ~v] (mcf [~w ~x ~y ~z] ~@body))
            ([~t ~u ~v ~w] (mcf [~x ~y ~z] ~@body))
            ([~t ~u ~v ~w ~x] (mcf [~y ~z] ~@body))
            ([~t ~u ~v ~w ~x ~y] (fn [~z] (~f ~t ~u ~v ~w ~x ~y ~z)))
            ([~t ~u ~v ~w ~x ~y ~z] ~@body)))))


(defmacro defcurry
  "Defines a curried function that may be called as a partial
   or total function using the regular function-call notation.
   Partial applications yield functions that work the same way.
   When all arguments have been given it evaluates the body
   and returns its value. Takes an optional docstring, as defn."
  [fname & [f1 & fs :as all]]
  (let [[doc parts] (if (string? f1) [f1 fs] [(str fname) all])
        args (first parts)
        body (rest parts)]
    `(do (def ~fname ~doc (mcf ~args ~@body))
	 (alter-meta! (var ~fname) assoc :arglists (list '~args))
	 (var ~fname))))


(defmacro curry
  "Returns a curried version of a function. Variadic functions
   must supplied the number of arguments."
  ([f]
    (let [args (-> f resolve meta :arglists first)]
      (assert (not (some #{'&} args)) "can't curry a variadic function")
      `(curry ~f ~(count args))))
  ([f n]
    (let [args (vec (repeatedly n gensym))
          body (list* f args)]
      `(mcf ~args ~body))))


(defn flip
  "Returns a version of f with its first two arguments reversed."
  [f]
  (fn [x y & more]
    (apply f y x more)))


;; +-------------------------------------------------------------+
;; |                      Lazy Evaluation.                       |
;; +-------------------------------------------------------------+


(defmacro deflazytype
  "Like deftype but fields are evaluated lazily.

   This macro creates a deftype definition and a constructor macro
   that will delay its arguments. The type implements IKeywordLookup
   so that fields may work as accessors like in defrecord. These
   accessors will first force the field's value. Example:

   (deflazytype MyRec [foo bar])  ;; defines MyRec, implements IKeywordLookup
   (def rec (MyRec* form1 form2)) ;; returns immediately, won't eval forms
   (:foo rec)                     ;; (force)'ed value of form1
   (:bar rec)                     ;; (force)'ed value of form2"
  [name fields]
  (let [clauses (interleave (map (comp keyword str) fields)
			    (for [f fields] (list 'clojure.core/force f)))
	delays  (for [f fields] (list 'list (list 'quote 'clojure.core/delay) f))
	ctorfun (symbol (str name "*"))
	ctorsym (symbol (str name "."))
	typdecl `(deftype ~name ~fields
                   clojure.lang.IKeywordLookup
                     (getLookupThunk [t1# k#]
                       (reify clojure.lang.ILookupThunk
                         (get [t2# tgt#] (case k# ~@clauses nil)))))
	macdecl (list 'defmacro ctorfun fields
		  (concat (list 'list  (list 'quote ctorsym)) delays))]
    (list 'do typdecl macdecl)))


;; +-------------------------------------------------------------+
;; |                          Monoids.                           |
;; +-------------------------------------------------------------+


(defprotocol Monoid
  "Monoids have an associative binary operator with an identity value."
  (mempty [this]
    "Returns the identity value of the receiver. Though this is actually
     a constructor, implementing this function makes it possible to write
     generic functions over monoids.")
  (mappend [this x]
    "Applies the operator defined by the instance on this and x."))


(defn mconcat
  "Reduces the collection of monoids using their defined
   mappend for accumulation and mempty as initial value."
  [coll]
  (if (seq coll)
    (reduce #(mappend %1 %2) (mempty (first coll)) coll)
    coll))


(declare sum-id prod-id any-id all-id)

;; +-------+
;; |  Sum  |
;; +-------+

(deftype Sum [sum]
  Object
    (equals [this x] (= sum (.sum ^Sum x)))
  Comparable
    (compareTo [this x] (compare sum (.sum ^Sum x)))
  Monoid
    (mempty [this] sum-id)
    (mappend [this x] (Sum. (+ sum (.sum ^Sum x)))))

  (defn sum
  "Accessor for a Sum type."
  [^Sum s] (.sum s))

(defmethod print-method Sum [r, ^java.io.Writer w]
  (.write w "Sum ")
  (print (sum r)))

(def sum-id
  "The identity value for the Sum monoid."
  (->Sum 0))

;; +-----------+
;; |  Product  |
;; +-----------+

(deftype Product [product]
  Object
    (equals [this x] (= product (.product ^Product x)))
  Comparable
    (compareTo [this x] (compare product (.product ^Product x)))
  Monoid
    (mempty [this] prod-id)
    (mappend [this x] (Product. (* product (.product ^Product x)))))

(defn product
  "Accessor for a Product type."
  [^Product s] (.product s))

(defmethod print-method Product [r, ^java.io.Writer w]
  (.write w "Product ")
  (print (product r)))

(def prod-id
  "The identity value for the Product monoid."  
  (->Product 1))

;; +---------------------+
;; |  Any -- logical OR  |
;; +---------------------+

(deftype Any [any]
  Object
    (equals [this x] (= any (.any ^ Any x)))
  Comparable
    (compareTo [this x] (compare any (.any ^Any x)))
  Monoid
    (mempty [this] any-id)
    (mappend [this x] (Any. (or any (.any ^Any x)))))

(defn any
  "Accessor for the Any type."
  [^Any s] (.any s))

(defmethod print-method Any [r, ^java.io.Writer w]
  (.write w "Any ")
  (print (any r)))

(def any-id
  "The identity value for the Any monoid."
  (->Any false))

;; +----------------------+
;; |  All -- logical AND  |
;; +----------------------+

(deftype All [all]
  Object
    (equals [this x] (= all (.all ^All x)))
  Comparable
    (compareTo [this x] (compare all (.all ^All x)))
  Monoid
    (mempty [this] all-id)
    (mappend [this x] (All. (and all (.all ^All x)))))

(defn all
  "Accessor for the All type."
  [^All s] (.all s))

(defmethod print-method All [r, ^java.io.Writer w]
  (.write w "All ")
  (print (all r)))

(def all-id
  "The identity value for the All monoid."
  (->All true))

;; +--------+
;; |  Pair  |
;; +--------+

(deftype Pair [fst snd]
  Object
    (equals [this x]
      (and (= fst (.fst ^Pair x)) (= snd (.snd ^Pair x))))
  Comparable
    (compareTo [this x]
      (let [result (compare fst (.fst ^Pair x))]
        (if (zero? result)
          (compare snd (.snd ^Pair x))
          result)))
  Monoid
    (mempty [this] (Pair. (mempty fst) (mempty snd)))
    (mappend [this x] (Pair. (mappend fst (.fst ^Pair x))
			     (mappend snd (.snd ^Pair x)))))

(defn fst
  "Accessor for the first element of a pair."
  [^Pair p] (.fst p))

(defn snd
  "Accessor for the second element of a pair."
  [^Pair p] (.snd p))

(defmethod print-method Pair [r, ^java.io.Writer w]
  (.write w "Pair(")
  (print (fst r))
  (.write w ",")
  (print (snd r))
  (.write w ")"))

;; +----------------------------------+
;; |  Monoids for Clojure data types. |
;; +----------------------------------+

(def empty-string      "")
(def empty-list        ())
(def empty-vector      [])
(def empty-hash-set    #{})
(def empty-sorted-set  (sorted-set))
(def empty-array-map   {})
(def empty-hash-map    (empty (hash-map 0 0)))
(def empty-sorted-map  (sorted-map))
(def empty-queue       clojure.lang.PersistentQueue/EMPTY)

(extend-protocol Monoid
  java.lang.String
    (mempty [this] empty-string)
    (mappend [this x] (str this x))
  clojure.lang.PersistentList
    (mempty [this] empty-list)
    (mappend [this x] (concat this x))
  clojure.lang.PersistentList$EmptyList
    (mempty [this] empty-list)
    (mappend [this x] (concat this x))
  clojure.lang.PersistentVector
    (mempty [this] empty-vector)
    (mappend [this x] (into this x))
  clojure.lang.APersistentVector$SubVector
    (mempty [this] empty-vector)
    (mappend [this x] (into this x))
  clojure.lang.PersistentHashSet
    (mempty [this] empty-hash-set)
    (mappend [this x] (into this x))
  clojure.lang.PersistentTreeSet
    (mempty [this] empty-sorted-set)
    (mappend [this x] (into this x))
  clojure.lang.PersistentArrayMap
    (mempty [this] empty-array-map)
    (mappend [this x] (into this x))
  clojure.lang.PersistentHashMap
    (mempty [this] empty-hash-map)
    (mappend [this x] (into this x))
  clojure.lang.PersistentTreeMap
    (mempty [this] empty-sorted-map)
    (mappend [this x] (into this x))
  clojure.lang.PersistentQueue
    (mempty [this] empty-queue)
    (mappend [this x] (into this x))
  clojure.lang.Cons
    (mempty [this] empty-list)
    (mappend [this x] (concat this x))
  clojure.lang.LazySeq
    (mempty [this] (lazy-seq ()))
    (mappend [this x] (concat this x)))


;; +-------------------------------------------------------------+
;; |                          Functors.                          |
;; +-------------------------------------------------------------+


(defprotocol Functor
  "The Functor protocol is used for types that can be mapped over.
   Instances of Functor should satisfy the following laws:

   fmap id  ==  id
   fmap (f . g)  ==  fmap f . fmap g"
  (fun [this f]
    "Applies a function to a functor's data, producing a new functor."))


(extend-protocol Functor
  clojure.lang.PersistentList
    (fun [this f] (map f this))
  clojure.lang.PersistentList$EmptyList
    (fun [this f] (lazy-seq ()))
  clojure.lang.PersistentVector
    (fun [this f] (into [] (map f this)))
  clojure.lang.APersistentVector$SubVector
    (fun [this f] (into [] (map f this)))
  clojure.lang.PersistentHashSet
    (fun [this f] (into #{} (map f this)))
  clojure.lang.PersistentTreeSet
    (fun [this f] (into (sorted-set) (map f this)))
  clojure.lang.PersistentArrayMap
    (fun [this f] (into {} (for [[k v] this] [k (f v)])))
  clojure.lang.PersistentHashMap
    (fun [this f] (into (empty this) (for [[k v] this] [k (f v)])))
  clojure.lang.PersistentTreeMap
    (fun [this f] (into (sorted-map) (for [[k v] this] [k (f v)])))
  clojure.lang.PersistentQueue
    (fun [this f] (into clojure.lang.PersistentQueue/EMPTY (map f this)))
  clojure.lang.Cons
    (fun [this f] (cons (f (first this)) (map f (rest this))))
  clojure.lang.IFn
    (fun [this f] (comp f this))
  clojure.lang.LazySeq
    (fun [this f] (map f this))
  Pair
    (fun [this f] (->Pair (f (fst this)) (f (snd this)))))


(defcurry fmap
  "A version of (fun) where the functor is last."
  [function functor]
  (fun functor function))


(defcurry <$
  "Maps a functor to the same value."
  [value functor]
  (fmap (constantly value) functor))


;; +-------------------------------------------------------------+
;; |                    Applicative Functors.                    |
;; +-------------------------------------------------------------+


(defprotocol Applicative
  "A functor with application, providing operations to embed pure
   expressions and sequence computations to combine their results."
  (app [this x]
    "Applies the first functor on its argument, which is either
     a single value of a collection."))


(defn <*>
  "Applies the receiver to the rest of the arguments. Unlike (app),
   this function takes a variable number of arguments."
  [af f & more]
  (app af (if more (conj more f) f)))


;; +-------------------------------------------------------------+
;; |                           Monads.                           |
;; +-------------------------------------------------------------+


(defprotocol Monad
  "A monadic type supports the chaining of sequential actions."
  (>>= [m k]
    "Performs computation m and applies k to the result. Function k
     must return a monadic type.")
  (return [this x]
    "Creates a monadic value of type this with value x. The receiver
     object is not used except for dispatching the right constructor."))


(defmacro monad
  "Monadic binding that expands into nested >>= forms and a function body.

   The pattern:

   (>>= p1 (fn [v1]
   (>>= p2 (fn [v2]
   ...
     (return (f v1 v2 ...))))))

   can be more conveniently be written as:

   (monad [v1 p1 v2 p2 ...] (return (f v1 v2 ...)))"
  [[& bindings] & body]
  (let [[sym p] (take 2 bindings)]
    (if (= 2 (count bindings))
      `(>>= ~p (fn [~sym] ~@body))
      `(>>= ~p (fn [~sym] (monad ~(drop 2 bindings) ~@body))))))


(defn >>
  "Performs two or more actions; ignores the result of the first."
  ([m k]
   (>>= m (fn [_] k)))
  ([m k & more]
   (reduce >> (list* m k more))))


(defn <<
  "Performs two or more actions; keeps the result of the first."
  ([p q]
   (>>= p (fn [x] (>> q (return p x)))))
  ([p q & more]
   (reduce << (list* p q more))))


(defn seqm
  "Executes actions in sequence; collects the results in a vector.
   The result is a new monad with the vector as its value. The
   collection argument must not be empty."
  [coll]
  (reduce #(monad [x %1 y %2]
	     (return %1 (conj x y)))
	  (return (first coll) []) coll))


(defn seqm_
  "Executes actions in sequence, ignoring the results.
   The result is a new monad with an empty vector as its value."
  [ms]
  (reduce #(<< %1 %2) (return (first ms) []) ms))


(defn mapm
  "Maps a one-arg monad constructor over a collection of values.
   Then it executes the actions in a sequence with (seqm)."
  [f coll] (seqm (map f coll)))


(defn liftm
  "Lifts a one-arg function to work with monads."
  [f m] (monad [x m] (return m (f x))))


(defn liftm2
  "Lifts a two-arg function to work with monads."
  [f m1 m2]
  (monad [x m1 y m2] (return m1 (f x y))))


(defn liftm3
  "Lifts a three-arg function to work with monads."
  [f m1 m2 m3]
  (monad [x m1 y m2 z m3] (return m1 (f x y z))))


(defn liftm4
  "Lifts a four-arg function to work with monads."
  [f m1 m2 m3 m4]
  (monad [w m1 x m2 y m3 z m4] (return m1 (f w x y z))))


;; +-------------------------------------------------------------+
;; |                         MonadPlus.                          |
;; +-------------------------------------------------------------+


(defprotocol MonadPlus
  "Monads that support choice and failure; should satisfy:

   mzero >>= f  =  mzero
   v >> mzero   =  mzero"
  (mplus [this m]
    "An associative operation with identity value mzero.")
  (mzero [this]
    "Creates an identity value of type this with value x.
     The receiver is used to dispatch the right constructor."))
