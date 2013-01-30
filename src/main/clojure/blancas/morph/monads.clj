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
  [m] (.value m))


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
      (= value (.value other)))
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
   (let [v (gensym)
	 t (gensym)]
     `(->Maybe (try
		 (let [~v ~form]
		   (if (~pred ~v) ~v))
		 (catch java.lang.Exception ~t nil))))))


(defn run-just
  "Returns the boxed value of m."
  [m] (.value m))


(defn just?
  "Checks for a maybe with a value."
  [m] (not (nil? (.value m))))


(defn nothing?
  "Checks for a maybe with nothing."
  [m] (nil? (.value m)))


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
  "Filters Just values from a collection."
  [coll]
  (for [m coll :when (just? m)]
    (run-just m)))


(defn map-maybe
  "Maps a Maybe-producing function over a collection.
   Returns a sequence with Nothing values removed."
  [f coll]
  (for [x (map f coll) :when (just? x)]
    (run-just x)))


;; +-------------------------------------------------------------+
;; |                      The Either Monad.                      |
;; +-------------------------------------------------------------+


(deftype Either [left right])


(defn left
  "Constructor for a Left value."
  [x] (->Either (or x "nil value") nil))


(defn right
  "Constructor for a Right value."
  [x] (->Either nil x))


(defmacro make-either
  "Either constructor for a boxed value. It takes a value or a form
   that may evaluate to nil or throw an exception, both of which
   cases result in a Left value. Otherwise it is a Right value."
  ([form]
   `(make-either nil (constantly true) ~form))
  ([message form]
   `(make-either ~message (constantly true) ~form))
  ([message pred form]
   (let [v (gensym) t (gensym)]
     `(try
        (if-let [~v ~form]
          (if (~pred ~v)
	    (right ~v)
	    (left (or ~message "failed predicate")))
          (left (or ~message "nil value")))
        (catch java.lang.Exception ~t
          (left (str (if ~message (str ~message \newline))
		     (or (.getMessage ~t) (class ~t)))))))))


(defn run-left
  "Accessor for the Left value."
  [m] (.left m))


(defn run-right
  "Accessor for the Right value."
  [m] (.right m))


(defn left?
  "Tests if the receiver is a Left value."
  [x] (not (nil? (run-left x))))


(defn right?
  "Tests if the receiver is a Right value."
  [x] (not (nil? (run-right x))))


(defmacro either
  "Evaluates m. If the result is Left, binds it to x
   and evaluates left-form; otherwise binds it to y
   and evaluates right-form."
  [[[x y] m] left-form right-form]
  `(if (left? ~m)
     (let [~x (run-left ~m)] ~left-form)
     (let [~y (run-right ~m)] ~right-form)))


(defmethod print-method Either [r, ^java.io.Writer w]
  (either [[x y] r]
    (print "Left" x)
    (print "Right" y)))


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
  "Extracts the Left values from a collection."
  [coll]
  (for [x coll :when (left? x)]
    (run-left x)))


(defn rights
  "Extracts the Right values from a collection."
  [coll]
  (for [x coll :when (right? x)]
    (run-right x)))


(defn map-either
  "Maps an Either-producing function over a collection.
   Returns a sequence with Left values removed."
  [f coll]
  (for [x (map f coll) :when (right? x)]
    (run-right x)))


;; +-------------------------------------------------------------+
;; |                      The Reader Monad.                      |
;; +-------------------------------------------------------------+


(deftype Reader [f])  ;; f :: e -> v


(defn run-reader
  "Performs a Reader action m with an environment e.
   Returns the value produced by the action."
  [m e] ((.f m) e))


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
  [m] (.value m))


(defn exec-writer
  "Returns the output of a writer."
  [m] (.output m))


(defmethod print-method Writer [r, ^java.io.Writer w]
  (.write w "Writer(")
  (print (.value r))
  (.write w ",")
  (print (.output r))
  (.write w ")"))


(defn writer
  "Writer monad constructor for value a and output w.
   The value w must implement Monoid."
  [a w] (->Writer a w))


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
  [m s] ((.f m) s))


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
