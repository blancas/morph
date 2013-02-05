(ns ^{:doc "A sample Writer Monad Transformer."
      :author "Armando Blancas"}
  writert
  (:use [blancas.morph core monads transf]))

;; To try:
(comment
(load "writert")
(ns writert)
(run)
)

;; WriterT String State

(defn make-ws
  "Makes an instance of the composed monad WriterT String State."
  [x] (writer-t state x	empty-string))

;; Convenience functions that extract and then evaluate the inner
;; State monad; which is the opposite order of the constructor.

(defn eval-ws
  "Returns the value of the inner monad."
  [m s] (eval-state (eval-writer-t m) s))

(defn exec-ws
  "Returns the final state of the inner monad."
  [m s] (exec-state (eval-writer-t m) s))

(defn get-log
  "Returns the final value of the output in the outer monad."
  [m s] (eval-state (exec-writer-t m) s))

;; Create monadic values.
(def m1 (make-ws 500))
(def m2 (make-ws 100))
(def m3 (make-ws 999))

;; Variables x and y bind to the values of the inner monad.
(def m4 (monad [x m1 y m2] (make-ws (+ x y))))

;; We can append output to the outer monad with (tell).
(def m5 (monad [x m1 
                _ (tell-wt state "What, me worry?")
                y m2]
	  (make-ws 0)))

;; Using lift we can work with inner monads and convert
;; them to the outer monad. Here we use all the data.
(def m6 (monad [x m1
                _ (lift-wt (put-state "State: Now is the time") empty-string)
                _ (tell-wt state "Wrote: What, me worry?") 
                y m2
		z m3]
          (make-ws (+ x y z))))

(defn run []
  (println "m4 value:"  (eval-ws m4 ""))
  (println "m5 output:" (get-log m5 ""))
  (println "m6 value:"  (eval-ws m6 ""))
  (println "m6 state:"  (exec-ws m6 ""))
  (println "m6 output:" (get-log m6 "")))
