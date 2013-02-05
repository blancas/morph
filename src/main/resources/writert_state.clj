(ns ^{:doc "A sample Writer Transformer."
      :author "Armando Blancas"}
  writert-state
  (:use [blancas.morph core monads transf]))

;; To try:
(comment
(load "writert_state")
(ns writert-state)
(run-samples)
)

(defn make-ws
  "Makes an instance of the composed monad WriterT-State."
  ([x] (make-ws x empty-vector))
  ([x out] (writer-t state x out)))

(defn eval-ws
  "Returns the value of the inner monad."
  [m s] (eval-state (eval-writer-t m) s))

(defn exec-ws
  "Returns the final state of the inner monad."
  [m s] (exec-state (eval-writer-t m) s))

(defn get-log
  "Returns the final value of the output in the outer monad."
  [m s] (eval-state (exec-writer-t m) s))

(declare run)

(def table {'DEG 57.295779 'E 2.718281 'PI 3.141592})

(defn calc [op x y log]
  (monad [a (run x) b (run y)]
    (make-ws (op a b) [log])))

(defn const [x]
  (if (symbol? x)
    (lift-wt (gets x))
    (make-ws x)))

(defn decl [x y]
  (>> (lift-wt (modify-state assoc x y))
      (make-ws y)))

(defn clear [x]
  (>> (lift-wt (put-state {}))
      (make-ws x)))

(defn run [op]
  (if (list? op)
    (case (second op)
      + (calc + (first op) (last op) "add")
      - (calc - (first op) (last op) "subtrac")
      * (calc * (first op) (last op) "multiply")
      / (calc / (first op) (last op) "divide")
      = (decl   (first op) (last op))
      % (clear  (first op)))
    (const op)))

(defn run-samples []
  (let [v1 (eval-ws (run '((9 / 3) + (2 * (PI - E)))) table)
        v2 (exec-ws (run '((9 / (k = 3)) + (k * (PI - E)))) table)
        v3 (exec-ws (run '((2 * (PI - E)) + (10 %))) table)
        v4 (get-log (run '((9 / 3) + (2 * (PI - E)))) table)]
    (println "result=" v1)
    (println "table= " v2)
    (println "table= " v3)
    (println "output=" v4)))
