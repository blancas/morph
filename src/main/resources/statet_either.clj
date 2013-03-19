(ns ^{:doc "A sample State Transformer."
      :author "Armando Blancas"}
  statet-either
  (:use [blancas.morph core monads transf]))

;; To try:
(comment
(load "statet_either")
(ns statet-either)
(run-samples)
)

(defn make-left
  "Makes a Left value inside a State. This makes possible
   to get a Left off `run-se` whose value is not a pair."
  [x] (->StateT left (fn [_] (left x))))

(defn make-right
  "Makes an Right value inside a State."
  [x] (state-t right x))

(defn run-ste
  "Returns the Either inner monad."
  [m s] (eval-state-t m s))

(defn eval-ste
  "Returns the Right value of the inner monad."
  [m s] (run-right (eval-state-t m s)))

(defn exec-ste
  "Returns the final state of the outer monad as an Either value."
  [m s] (run-right (exec-state-t m s)))

(declare run)

(def table {'DEG 57.295779 'E 2.718281 'PI 3.141592})

(defn calc [op x y]
  (monad [a (run x) b (run y)]
    (lift-st (make-either (op a b)))))

(defn const [x]
  (if (symbol? x)
    (gets-st right x)
    (make-right x)))

(defn decl [x y]
  (>> (modify-st right assoc x y)
      (make-right y)))

(defn run [op]
  (if (list? op)
    (case (second op)
      + (calc + (first op) (last op))
      - (calc - (first op) (last op))
      * (calc * (first op) (last op))
      / (calc / (first op) (last op))
      = (decl   (first op) (last op)))
    (const op)))

(defn run-samples []
  (let [v1 (run-ste  (run '((9 / 3) + (2 * (PI - E)))) table)
	v2 (eval-ste (run '((9 / 3) + (2 * (PI - E)))) table)
	v3 (run-ste  (run '((9 / 0) + (2 * (PI - E)))) table)
	v4 (eval-ste (run '((9 / 0) + (2 * (PI - E)))) table)
        v5 (exec-ste (run '((9 / (k = 3)) + (k * (PI - E)))) table)]
    (println "run="   v1)
    (println "eval="  v2)
    (println "run="   v3)
    (println "eval="  v4)
    (println "table=" v5)))
