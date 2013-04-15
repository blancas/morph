;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Basic Monads."
      :author "Armando Blancas"}
  blancas.morph.monads
  (:use [blancas.morph.core]))


;; +-------------------------------------------------------------+
;; |                     The Identity Monad.                     |
;; +-------------------------------------------------------------+


(deftype Identity [value])


(defn run-id
  "Returns the value held by an Identity type."
  [^Identity m] (.value m))


(defmethod print-method Identity [r, ^java.io.Writer w]
  (print "Identity" (run-id r)))


(def id
  "Alias for the Identity constructor."
  ->Identity)


(extend-type Identity
  Functor
    (fun [this f]
      (monad [x this] (id (f x))))
  Applicative
    (app [this m]
      (if (coll? m)
        (monad [f this ms (seqm m)] (id (apply f ms)))
	(monad [f this x m] (id (f x)))))
  Monad
    (>>= [m k]
      (k (run-id m)))
    (return [this x]
      (id x)))


;; +-------------------------------------------------------------+
;; |                       The Maybe Monad.                      |
;; +-------------------------------------------------------------+


(deftype Maybe [value]
  Object
    (equals [this other]
      (= value (.value ^Maybe other)))
    (hashCode [this]
      (if value (.hashCode value) 0))
    (toString [this]
      (with-out-str (print this))))


(defn just
  "Maybe constructor. If x is not nil, it creates a Just value.
   If x is nil it creates a Nothing."
  [x] (->Maybe x))


(def nothing
  "Maybe constructor for no value."
  (->Maybe nil))


(defmacro maybe
  "Maybe constructor for a boxed value. It takes a value or a form
   that may evaluate to nil or throw an exception, both of which
   cases result in a Nothing value."
  ([form]
   `(maybe (constantly true) ~form))
  ([pred form]
   `(->Maybe (try
	       (let [v# ~form]
		 (if (~pred v#) v#))
	       (catch java.lang.Exception t# nil)))))


(defn run-just
  "Returns the boxed value of m."
  [^Maybe m] (.value m))


(defn just?
  "Checks for a maybe with a value."
  [^Maybe m] (not (nil? (.value m))))


(defn nothing?
  "Checks for a maybe with nothing."
  [^Maybe m] (nil? (.value m)))


(defmacro may
  "Like if-let for Maybe values. If m has a value, binds it to x
   and evaluates the then-form. Otherwise, evaluates the optional
   else-form."
  ([[x m] then-form]
    `(may [~x ~m] ~then-form nil))
  ([[x m] then-form else-form]
    `(if-let [~x (run-just ~m)] ~then-form ~else-form)))


(defmethod print-method Maybe [r, ^java.io.Writer w]
  (may [x r]
    (print "Just" x)
    (print "Nothing")))


(extend-type Maybe
  Monoid
    (mempty [this] nothing)
    (mappend [this x]
      (cond (nothing? this) x
            (nothing? x)    this
            :else           (just (mappend (run-just this) (run-just x)))))
  Functor
    (fun [this f]
      (monad [x this] (just (f x))))
  Applicative
    (app [this m]
      (if (coll? m)
        (monad [f this ms (seqm m)] (just (apply f ms)))
	(monad [f this x m] (just (f x)))))
  Monad
    (>>= [m k]
      (if (nothing? m) m (k (run-just m))))
    (return [this x]
      (just x)))


(defn justs
  "Filters Just values from a collection. Returns unboxed values."
  [coll]
  (for [m coll :when (just? m)]
    (run-just m)))


(defn map-maybe
  "Maps a Maybe-producing function over a collection.
   Filters out Nothing values and returns unboxed values."
  [f coll] (justs (map f coll)))


;; +-------------------------------------------------------------+
;; |                      The Either Monad.                      |
;; +-------------------------------------------------------------+


(deftype Either [left right])


(defn left
  "Constructor for a Left value."
  [x] (->Either (or x "nil value") nil))


(defn right
  "Constructor for a Right value."
  [x]
  (if (nil? x)
    (left nil)
    (->Either nil x)))


(defmacro make-either
  "Either constructor for a boxed value. It takes a value or a form
   that may evaluate to nil or throw an exception, both of which
   cases result in a Left value. Otherwise it is a Right value."
  ([form]
   `(make-either nil (constantly true) ~form))
  ([message form]
   `(make-either ~message (constantly true) ~form))
  ([message pred form]
   `(try
      (if-let [v# ~form]
        (if (~pred v#)
	  (right v#)
	  (left (or ~message "failed predicate")))
        (left (or ~message "nil value")))
      (catch java.lang.Exception t#
        (left (str (if ~message (str ~message \newline))
		   (or (.getMessage t#) (class t#))))))))


(defn run-left
  "Accessor for the Left value."
  [^Either m] (.left m))


(defn run-right
  "Accessor for the Right value."
  [^Either m] (.right m))


(defn left?
  "Tests if the receiver is a Left value."
  [x] (not (nil? (run-left x))))


(defn right?
  "Tests if the receiver is a Right value."
  [x] (nil? (run-left x)))


(defmacro either
  "Evaluates m. If the result is Left, binds its Left contents
   to v and evaluates left-form; otherwise binds its Right
   contents to v and evaluates right-form."
  ([[v m] right-form]
   `(either [~v ~m] nil ~right-form))
  ([[v m] left-form right-form]
   `(let [val# ~m]
      (if (left? val#)
        (let [~v (run-left val#)] ~left-form)
        (let [~v (run-right val#)] ~right-form)))))


(defmethod print-method Either [r, ^java.io.Writer w]
  (either [v r]
    (print "Left" v)
    (print "Right" v)))


(extend-type Either
  Functor
    (fun [this f]
      (monad [x this] (right (f x))))
  Applicative
    (app [this m]
      (if (coll? m)
        (monad [f this ms (seqm m)] (right (apply f ms)))
	(monad [f this x m] (right (f x)))))
  Monad
    (>>= [m k]
      (if (left? m) m (k (run-right m))))
    (return [this x]
      (right x)))


(defn lefts
  "Extracts the Left values from a collection. Returns unboxed values."
  [coll]
  (for [x coll :when (left? x)]
    (run-left x)))


(defn rights
  "Extracts the Right values from a collection. Returns unboxed values."
  [coll]
  (for [x coll :when (right? x)]
    (run-right x)))


(defn map-either
  "Maps an Either-producing function over a collection.
   Returns a sequence with Left values removed, as unboxed values."
  [f coll] (rights (map f coll)))


;; +-------------------------------------------------------------+
;; |                      The Reader Monad.                      |
;; +-------------------------------------------------------------+


(deftype Reader [f])  ;; f :: e -> v


(defn run-reader
  "Performs a Reader action m with an environment e.
   Returns the value produced by the action."
  [^Reader m e] ((.f m) e))


(defmethod print-method Reader [r, ^java.io.Writer w]
  (.write w "Reader@")
  (print (mod (hash r) 100)))


(defn reader
  "The Reader monad constructor."
  [x] (->Reader (fn [_] x)))


(extend-type Reader
  Functor
    (fun [this f]
      (monad [x this] (reader (f x))))
  Applicative
    (app [this x]
      (if (coll? x)
        (monad [f this vs (seqm x)] (reader (apply f vs)))
        (monad [f this v x] (reader (f v)))))
  Monad
  (>>= [m k]
    (->Reader (fn [e] (run-reader (k (run-reader m e)) e))))
  (return [this x]
    (reader x)))


(def ask
  "Returns a Reader whose value is the environment."
  (->Reader identity))


(defn asks
  "Returns a Reader whose value the result of applying f
   to the current environment."
  [f] (->Reader (fn [e] (f e))))


(defn local
  "Returns a Reader whose value is the result of running the Reader m
   under a changed environment, which is produced by applying f on
   the previous environment."
  [f m]
  (->Reader (fn [e] (run-reader m (f e)))))


;; +-------------------------------------------------------------+
;; |                      The Writer Monad.                      |
;; +-------------------------------------------------------------+


(deftype Writer [value output])  ;; output must implement Monoid.


(defn eval-writer
  "Returns the value of a writer."
  [^Writer m] (.value m))


(defn exec-writer
  "Returns the output of a writer."
  [^Writer m] (.output m))


(defmethod print-method Writer [r, ^java.io.Writer w]
  (.write w "Writer(")
  (print (.value ^Writer r))
  (.write w ",")
  (print (.output ^Writer r))
  (.write w ")"))


(defn writer
  "Writer monad constructor for value a and output w.
   The value w must implement Monoid. If w is not
   supplied, it defaults to the empty vector."
  ([a] (->Writer a []))
  ([a w] (->Writer a w)))


(extend-type Writer
  Functor
    (fun [this f]
      (monad [x this] (return this (f x))))
  Applicative
    (app [this m]
      (if (coll? m)
        (monad [f this vs (seqm m)] (return this (apply f vs)))
        (monad [f this x m] (return this (f x)))))
  Monad
  (>>= [m k]
    (let [w (k (eval-writer m))]
      (writer (eval-writer w)
	      (mappend (exec-writer m) (exec-writer w)))))
  (return [this x]
    (writer x (mempty (.output this)))))


(defn tell
  "Returns a Writer with output x."
  [x] (writer nil x))


(defn listen
  "Allows chained Writers to peek at the current output w
   by returning a value as a pair (v, w)."
  [m]
  (let [a (eval-writer m)
	w (exec-writer m)]
    (writer (->Pair a w) w)))


(defn pass
  "Performs an action that returns a Writer similar to (listen),
   but instead of output it adds an output-transforming function
   to the value's pair. Returns a Writer whose output is the
   result of that function."
  [m]
  (let [p (eval-writer m)
	w (exec-writer m)]
    (writer (fst p) ((snd p) w))))


;; +-------------------------------------------------------------+
;; |                       The State Monad.                      |
;; +-------------------------------------------------------------+


(deftype State [f])  ;; f :: s -> Pair [a s]


(defn run-state
  "Performs an action m with an initial state s.
   Returns a pair with the value and the state."
  [^State m s] ((.f m) s))


(defn eval-state
  "Performs an action m with an initial state s. Returns the value."
  [m s] (fst (run-state m s)))


(defn exec-state
  "Performs an action m with an initial state s. Returns the state."
  [m s] (snd (run-state m s)))


(defmethod print-method State [r, ^java.io.Writer w]
  (.write w "State@")
  (print (mod (hash r) 100)))


(defn state
  "State monad constructor."
  [x] (->State (fn [s] (->Pair x s))))


(extend-type State
  Functor
    (fun [this f]
      (monad [x this] (state (f x))))
  Applicative
    (app [this m]
      (if (coll? m)
        (monad [f this vs (seqm m)] (state (apply f vs)))
        (monad [f this v m] (state (f v)))))
  Monad
    (>>= [m k]
      (->State (fn [s]
	         (let [p (run-state m s)]
	           (run-state (k (fst p)) (snd p))))))
    (return [this x]
      (state x)))


(def get-state
  "Produces an action to return current state as the value."
  (->State (fn [s] (->Pair s s))))


(defn put-state
  "Produces an action to set the current state."
  [s] (->State (fn [_] (->Pair nil s))))


(defn modify-state
  "Produces an action to apply f to the current state, using any
   given (optional) arguments. The result becomes the new state."
  [f & more]
  (->State (fn [s] (->Pair nil (apply f s more)))))


(defn gets
  "Like get-state, but applies the function f (usually a selector)
   on the state being return as the value of the inner monad."
  [f]
  (->State (fn [s] (->Pair (f s) s))))
