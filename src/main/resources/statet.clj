(ns ^{:doc "A sample State Monad Transformer."
      :author "Armando Blancas"}
  statet
  (:use [blancas.morph core monads transf]))

;; To try:
(comment
(load "statet")
(ns statet)
(run)
)

;; StateT s Maybe a

(defn make-sm
  "Makes an instance of the composed monad StateT s Just x."
  [x] (state-t just x))

(def nothing-sm
  "Makes an instance of the compose monad StateT s Nothing.
   StateT expects a one-arg constructor and a value; for
   Nothing instances the arg and the value are ignored." 
  (state-t (fn [_] nothing) nil))

;; Convenience functions that extract and then evaluate the inner
;; Maybe monad; which is the opposite order of the constructor.
(defn run-sm
  "Returns the Maybe inner monad."
  [m s] (eval-state-t m s))

(defn eval-sm
  "Returns the Just value of the inner monad."
  [m s] (run-just (eval-state-t m s)))

(defn exec-sm
  "Returns the final state of the outer monad as a Just value."
  [m s] (run-just (exec-state-t m s)))

;; Create monadic values.
(def m1 (make-sm 500))
(def m2 (make-sm 100))
(def m3 (make-sm 999))
(def m4 nothing-sm)

;; Variables x and y bind to the values of the inner monad.
(def m5 (monad [x m1 y m2] (make-sm (+ x y))))

;; A Nothing will short-circuit the computations.
(def m6 (monad [x m1 _ m4 y m2 z m3] (make-sm (+ x y z))))

;; (put-st) replaces the state on the outer monad.
(def m7 (monad [x m1
                _ (put-st ->Maybe "State: Now is the time")
                y m2
		z m3]
          (make-sm (+ x y z))))

(defn run []
  (println "just? m5:   " (just? (run-sm m5 "")))
  (println "value m5:   " (eval-sm m5 ""))
  (println "nothing? m6:" (nothing? (run-sm m6 "")))
  (println "value m7:   " (eval-sm m7 ""))
  (println "state m7:   " (exec-sm m7 "")))
