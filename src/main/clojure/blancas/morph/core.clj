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
    2  (let [[x y] args f (gensym)]
         `(fn ~f
	    ([~x] (fn [~y] (~f ~x ~y)))
	    ([~x ~y] ~@body)))
    3  (let [[x y z] args f (gensym)]
	 `(fn ~f
            ([~x] (mcf [~y ~z] ~@body))
            ([~x ~y] (fn [~z] (~f ~x ~y ~z)))
            ([~x ~y ~z] ~@body)))
    4  (let [[w x y z] args f (gensym)]
	 `(fn ~f
            ([~w] (mcf [~x ~y ~z] ~@body))
            ([~w ~x] (mcf [~y ~z] ~@body))
            ([~w ~x ~y] (fn [~z] (~f ~w ~x ~y ~z)))
            ([~w ~x ~y ~z] ~@body)))
    5  (let [[v w x y z] args f (gensym)]
	 `(fn ~f
            ([~v] (mcf [~w ~x ~y ~z] ~@body))
            ([~v ~w] (mcf [~x ~y ~z] ~@body))
            ([~v ~w ~x] (mcf [~y ~z] ~@body))
            ([~v ~w ~x ~y] (fn [~z] (~f ~v ~w ~x ~y ~z)))
            ([~v ~w ~x ~y ~z] ~@body)))
    6  (let [[u v w x y z] args f (gensym)]
	 `(fn ~f
            ([~u] (mcf [~v ~w ~x ~y ~z] ~@body))
            ([~u ~v] (mcf [~w ~x ~y ~z] ~@body))
            ([~u ~v ~w] (mcf [~x ~y ~z] ~@body))
            ([~u ~v ~w ~x] (mcf [~y ~z] ~@body))
            ([~u ~v ~w ~x ~y] (fn [~z] (~f ~u ~v ~w ~x ~y ~z)))
            ([~u ~v ~w ~x ~y ~z] ~@body)))
    7  (let [[t u v w x y z] args f (gensym)]
	 `(fn ~f
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
   and returns its value. The comment string is required."
  [fname doc args & body]
   `(do (def ~fname ~doc (mcf ~args ~@body))
	(alter-meta! (var ~fname) assoc :arglists (list '~args))
	(var ~fname)))


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
    (equals [this x] (= sum (.sum x)))
  Comparable
    (compareTo [this x] (compare sum (.sum x)))
  Monoid
    (mempty [this] sum-id)
    (mappend [this x] (Sum. (+ sum (.sum x)))))

  (defn sum
  "Accessor for a Sum type."
  [s] (.sum s))

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
    (equals [this x] (= product (.product x)))
  Comparable
    (compareTo [this x] (compare product (.product x)))
  Monoid
    (mempty [this] prod-id)
    (mappend [this x] (Product. (* product (.product x)))))

(defn product
  "Accessor for a Product type."
  [s] (.product s))

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
    (equals [this x] (= any (.any x)))
  Comparable
    (compareTo [this x] (compare any (.any x)))
  Monoid
    (mempty [this] any-id)
    (mappend [this x] (Any. (or any (.any x)))))

(defn any
  "Accessor for the Any type."
  [s] (.any s))

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
    (equals [this x] (= all (.all x)))
  Comparable
    (compareTo [this x] (compare all (.all x)))
  Monoid
    (mempty [this] all-id)
    (mappend [this x] (All. (and all (.all x)))))

(defn all
  "Accessor for the All type."
  [s] (.all s))

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
      (and (= fst (.fst x)) (= snd (.snd x))))
  Comparable
    (compareTo [this x]
      (let [result (compare fst (.fst x))]
        (if (zero? result)
          (compare snd (.snd x))
          result)))
  Monoid
    (mempty [this] (Pair. (mempty fst) (mempty snd)))
    (mappend [this x] (Pair. (mappend fst (.fst x)) (mappend snd (.snd x)))))

(defn fst
  "Accessor for the first element of a pair."
  [p] (.fst p))

(defn snd
  "Accessor for the second element of a pair."
  [p] (.snd p))

(defmethod print-method Pair [r, ^java.io.Writer w]
  (.write w "Pair(")
  (print (fst r))
  (.write w ",")
  (print (snd r))
  (.write w ")"))

;; +----------+
;; |  String  |
;; +----------+

(def str-id "The identity value for the String monoid." "")

(extend-protocol Monoid
  java.lang.String
    (mempty [this] str-id)
    (mappend [this x] (str this x)))


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
    (fun [this f] (map f this))
  clojure.lang.APersistentVector$SubVector
    (fun [this f] (map f this))
  clojure.lang.PersistentHashSet
    (fun [this f] (map f this))
  clojure.lang.PersistentTreeSet
    (fun [this f] (map f this))
  clojure.lang.PersistentArrayMap
    (fun [this f] (for [[k v] this] (clojure.lang.MapEntry. k (f v))))
  clojure.lang.PersistentHashMap
    (fun [this f] (for [[k v] this] (clojure.lang.MapEntry. k (f v))))
  clojure.lang.PersistentTreeMap
    (fun [this f] (for [[k v] this] (clojure.lang.MapEntry. k (f v))))
  clojure.lang.PersistentQueue
    (fun [this f] (map f this))
  clojure.lang.Cons
    (fun [this f] (map f this))
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
  (<*> [this x]
    "Applies the first functor on its argument."))


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
