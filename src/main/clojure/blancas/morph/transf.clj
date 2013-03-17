;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Monads Transformers."
      :author "Armando Blancas"}
  blancas.morph.transf
  (:use [blancas.morph core monads]))


;; +-------------------------------------------------------------+
;; |                The Maybe Monad Transformer.                 |
;; +-------------------------------------------------------------+


;; ret   - return method for the inner monad.
;; im    - inner monad: m (Maybe a).
(deftype MaybeT [ret im])


(defn just-t
  "MaybeT constructor that uses the constructor of the inner monad
   (its return function) and a value for its Just instance, such
   that the result is: Monad (Just x)."
  [ret x]
  (->MaybeT ret (ret (just x))))


(defn nothing-t
  "MaybeT constructor that uses the constructor of the inner monad
   (its return function) to wrap a Nothing value."
  [ret]
  (->MaybeT ret (ret nothing)))


(defn run-just-t
  "Returns the inner monad, which contains a Maybe value."
  [m] (.im m))


(defn eval-just-t
  "Returns the Just value inside the inner monad in a new instance;
   thus, from m (Just a) it returns m a."
  [m]
  (monad [may (run-just-t m)]
    ((.ret m) (run-just may))))


(defmethod print-method MaybeT [r, ^java.io.Writer w]
  (.write w "MaybeT ")
  (print (run-just-t r)))


(extend-type MaybeT
  Functor
    (fun [this f]
      (monad [x this] (return this (f x))))
  Applicative
    (app [this m]
      (if (coll? m)
        (monad [f this ms (seqm m)] (return this (apply f ms)))
	(monad [f this x m] (return this (f x)))))
  Monad
    (>>= [m k]
      (->MaybeT (.ret m)
		(monad [mv (run-just-t m)]
		  (run-just-t (if (nothing? mv) m (k (run-just mv)))))))
    (return [this x]
      (just-t (.ret this) x)))


(defn lift-mt
  "Lifts an inner monad into the outer MaybeT monad."
  [im] (->MaybeT (fn [x] (return im x)) (liftm just im)))


;; +-------------------------------------------------------------+
;; |                The Either Monad Transformer.                |
;; +-------------------------------------------------------------+


;; ret   - return method for the inner monad.
;; im    - inner monad: m (Either e a).
(deftype EitherT [ret im])


(defn run-either-t
  "Returns the inner monad, which contains an Either value."
  [m] (.im m))


(defn left-t
  "Constructor for an inner monad with a Left value."
  [ret x] (->EitherT ret (ret (->Either x nil))))


(defn right-t
  "Constructor for an inner monad with a Right value."
  [ret x] (->EitherT ret (ret (->Either nil x))))


(defn run-left-t
  "Accessor for the left value as an instance of the inner monad;
   in effect: m (Either e a) -> m e"
  [m]
  (monad [x (run-either-t m)]
    ((.ret m) (run-left x))))


(defn run-right-t
  "Accessor for the right value as an instance of the inner monad;
   in effect: m (Either e a) -> m a"
  [m]
  (monad [x (run-either-t m)]
    ((.ret m) (run-right x))))


(defmethod print-method EitherT [r, ^java.io.Writer w]
  (.write w "EitherT ")
  (print (run-either-t r)))


(extend-type EitherT
  Functor
    (fun [this f]
      (monad [x this] (return this (f x))))
  Applicative
    (app [this m]
      (if (coll? m)
        (monad [f this ms (seqm m)] (return this (apply f ms)))
	(monad [f this x m] (return this (f x)))))
  Monad
    (>>= [m k]
      (->EitherT (.ret m)
                 (monad [e (run-either-t m)]
	           (run-either-t (if (left? e) m (k (run-right e)))))))
    (return [this x]
      (right-t (.ret this) x)))


(defn lift-et
  "Lifts an inner monad into the outer Right monad."
  [im] (->EitherT (fn [x] (return im x)) (liftm right im)))


(defn fail-et
  "Lifts an inner monad into the outer Left monad."
  [im] (->EitherT (fn [x] (return im x)) (liftm left im)))


;; +-------------------------------------------------------------+
;; |                The Reader Monad Transformer.                |
;; +-------------------------------------------------------------+


;; ret   - return function for the inner monad.
;; run   - a function for the change of state: e -> m a
(deftype ReaderT [ret run])


(defn run-reader-t
  "Performs a Reader action m with an environment e.
   Returns the inner monad produced by the action."
  [m e] ((.run m) e))


(defmethod print-method ReaderT [r, ^java.io.Writer w]
  (.write w "ReaderT@")
  (print (mod (hash r) 100)))


(defn reader-t
  "Reader monad constructor."
  [ret x] (->ReaderT ret (fn [_] (ret x))))


(extend-type ReaderT
  Functor
    (fun [this f]
      (monad [x this] (return this (f x))))
  Applicative
    (app [this x]
      (if (coll? x)
        (monad [f this vs (seqm x)] (return this (apply f vs)))
        (monad [f this v x] (return this (f v)))))
  Monad
  (>>= [m k]
    (->ReaderT (.ret m)
	       (fn [e]
		 (monad [v (run-reader-t m e)]
		   (run-reader-t (k v) e)))))
  (return [this x]
    (reader-t (.ret this) x)))


(defn ask-rt
  "Returns a ReaderT whose inner monad contains the environment."
  [ret]
  (->ReaderT ret (fn [e] (ret e))))


(defn asks-rt
  "Returns a Reader whose value the result of applying f
   to the current environment."
  [ret f]
  (->ReaderT ret (fn [e] (ret (f e)))))


(defn local-rt
  "Returns a Reader whose value is the result of running the Reader m
   under a changed environment, which is produced by applying f on
   the previous environment."
  [ret f m]
  (->ReaderT ret (fn [e] (run-reader-t m (f e)))))


(defn lift-rt
  "Lifts an inner monad into the outer ReaderT monad."
  [im] (->ReaderT (fn [x] (return im x)) (fn [_] im)))


;; +-------------------------------------------------------------+
;; |                The Writer Monad Transformer.                |
;; +-------------------------------------------------------------+


;; ret   - return method for the inner monad.
;; me    - the writer's monoid identity value
;; im    - inner monad: m (a, w), w must implement Monoid.
(deftype WriterT [ret me im])


(defn run-writer-t
  "Returns the inner monad."
  [m] (.im m))


(defn eval-writer-t
  "Returns the value part as an instance of the inner monad: m (a)."
  [m]
  (monad [p (run-writer-t m)]
    ((.ret m) (fst p))))


(defn exec-writer-t
  "Returns the output part as an instance of the inner monad: m (w)."
  [m]
  (monad [p (run-writer-t m)]
    ((.ret m) (snd p))))


(defmethod print-method WriterT [r, ^java.io.Writer w]
  (.write w "WriterT ")
  (print (run-writer-t r)))


(defn writer-t
  "WriterT monad constructor. Takes the inner monad, a value,
   and and output value. Alternatively, for functions that
   make instances out of existing ones, the second form
   takes a WriterT and an inner monad value."
  ([ret a w]
   (->WriterT ret (mempty w) (ret (->Pair a w))))
  ([m im]
   (->WriterT (.ret m) (.me m) im)))


(extend-type WriterT
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
    (->WriterT (.ret m) (.me m)
               (monad [p1 (run-writer-t m)
		       p2 (run-writer-t (k (fst p1)))]
                 ((.ret m) (->Pair (fst p2) (mappend (snd p1) (snd p2)))))))
  (return [this x]
    (writer-t (.ret this) x (.me this))))


(defn tell-wt
  "Returns a WriterT with output x, using the inner monad
   constructor ret."
  [ret x]
  (writer-t ret nil x))


(defn listen-wt
  "Allows chained WriterT's to peek at the current output w
   by returning a value as a pair (a, w)."
  [m]
  (writer-t m (monad [p (run-writer-t m)]
                ((.ret m) (->Pair p (snd p))))))


(defn listens-wt
  "Allows chained WriterT's to peek at the current output w
   by returning a value as a pair (a, f w)."
  [f m]
  (writer-t m (monad [p (run-writer-t m)]
	        (let [a (fst p) w (snd p)]
                  ((.ret m) (->Pair (->Pair a (f w)) w))))))


(defn pass-wt
  "Performs an action that returns a WriterT similar to (listen-wt),
   but instead of output it adds an output-transforming function
   to the value's pair. Returns a WriterT whose output is the
   result of that function."
  [m]
  (writer-t m (monad [p (run-writer-t m)]
	        (let [a (fst (fst p)) f (snd (fst p)) w (snd p)]
                  ((.ret m) (->Pair a (f w)))))))


(defn lift-wt
  "Lifts an inner monad into the outer WriterT monad with
   an output value w, which defaults to the empty vector."
  ([im]
   (lift-wt im empty-vector))
  ([im w]
   (->WriterT (fn [x] (return im x))
	      (mempty w)
              (monad [x im] (return im (->Pair x w))))))


;; +-------------------------------------------------------------+
;; |                 The State Monad Transformer.                |
;; +-------------------------------------------------------------+


;; ret   - return function for the wrapped monad.
;; run   - a function for the change of state: s -> m (Pair a s)
(deftype StateT [ret run])


(defmethod print-method StateT [r, ^java.io.Writer w]
  (.write w "StateT@")
  (print (mod (hash r) 100)))


(defn run-state-t
  "Performs an action m using the initial state s; returns
   the wrapped monad having a pair of the value and state."
  [m s] ((.run m) s))


(defn eval-state-t
  "Performs an action m using the initial state s; returns
   the wrapped monad with the resulting value."
  [m s]
  (monad [p (run-state-t m s)]
    ((.ret m) (fst p))))


(defn exec-state-t
  "Performs an action m using the initial state s; returns
   the wrapped monad with the resulting state."
  [m s]
  (monad [p (run-state-t m s)]
    ((.ret m) (snd p))))


(defn state-t
  "Makes a StateT moand whose value is another monad. The wrapped
   monad is made out of the constructor ctor and value x."
  [ret x]
  (->StateT ret (fn [s] (ret (->Pair x s)))))


(extend-type StateT
  Functor
    (fun [this f]
      (monad [x this] (return this (f x))))
  Applicative
    (app [this m]
      (if (coll? m)
        (monad [f this vs (seqm m)] (return this (apply f vs)))
        (monad [f this v m] (return this (f v)))))
  Monad
    (>>= [m k]
      (->StateT (.ret m)
		(fn [s]
	          (monad [p (run-state-t m s)]
	            (run-state-t (k (fst p)) (snd p))))))
    (return [this x]
      (state-t (.ret this) x)))


(defn get-st
  "Gets the outer monad's state; uses the given constructor
   to return that state as the value of the inner monad."
  [ret]
  (->StateT ret (fn [s] (ret (->Pair s s)))))


(defn put-st
  "Sets the outer monad's state; uses the given constructor
   to return that state in the inner monad."
  [ret s]
  (->StateT ret (fn [_] (ret (->Pair nil s)))))


(defn modify-st
  "Tranforms the outer monad's state; uses the given constructor
   to return that state in the inner monad. The function f is
   applied on the current state, along with any optional arguments."
  [ret f & more]
  (->StateT ret (fn [s] (ret (->Pair nil (apply f s more))))))


(defn gets-st
  "Like get-st, but applies the function f (usually a selector)
   on the state being return as the value of the inner monad."
  [ret f]
  (->StateT ret (fn [s] (ret (->Pair (f s) s)))))


(defn lift-st
  "Lifts an inner monad into the outer StateT monad."
  [im]
  (->StateT (fn [x] (return im x))
	     (fn [s] (monad [x im] (return im (->Pair x s))))))


;; +-------------------------------------------------------------+
;; |          The Imperative monad: StateT Either.               |
;; +-------------------------------------------------------------+


(defn ->left
  "Makes a Left value inside a State. This makes possible
   to get a Left off `run-se` whose value is not a pair."
  [x] (->StateT left (fn [_] (left x))))


(defn ->right
  "Makes a Right value inside a State."
  [x] (state-t right x))


(defn run-se
  "Returns the Either inner monad."
  [m s] (eval-state-t m s))


(def get-se
  "Gets the outer monad's state as a Right value."
  (->StateT right (fn [s] (right (->Pair s s)))))


(defn put-se
  "Sets the outer monad's state."
  [s]
  (->StateT right (fn [_] (right (->Pair nil s)))))


(defn modify-se
  "Tranforms the outer monad's state, to be returned as a
   Right value. The function f is applied on the current state,
   along with any optional arguments."
  [f & more]
  (->StateT right (fn [s] (right (->Pair nil (apply f s more))))))


(defn gets-se
  "Like get-st, but applies the function f (usually a selector)
   on the state being return as a Right value."
  [f]
  (->StateT right (fn [s] (right (->Pair (f s) s)))))
