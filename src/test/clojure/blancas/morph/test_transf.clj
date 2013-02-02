;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns blancas.morph.test-transf
  (:use [blancas.morph core monads transf]
	[clojure.test]
	[midje.sweet :only (fact)]))


;; +-------------------------------------------------------------+
;; |                         Utilities.                          |
;; +-------------------------------------------------------------+


(defn square [x] (* x x))

(defn upper [^String x] (.toUpperCase x))


;; +-------------------------------------------------------------+
;; |                 The State Monad Transformer.                |
;; +-------------------------------------------------------------+

;; ===================
;; StateT s Identity a
;; ===================

(defn return-stid
  [x] (state-t id x))

(defn eval-stid
  [m s] (run-id (eval-state-t m s)))


(deftest test-0000
  (let [st (return-stid "foo")]
    (fact "run a state monad"
	  (run-id (eval-state-t st 0))   => "foo"
	  (run-id (exec-state-t st 747)) => 747)))


(deftest test-0005
  (let [st1 (return-stid 787)
	st2 (return-stid "foo")]
    (fact "prints the transformer monad"
	  (with-out-str (print st1)) => #(.startsWith % "StateT@")
	  (with-out-str (print st2)) => #(.startsWith % "StateT@"))
    (fact "prints the wrapped monad"
	  (with-out-str (print (eval-state-t st1 0))) => "Identity 787")))


(deftest test-0010
  (let [st1 (return-stid 12)
	st2 (return-stid "jib-jab")]
    (fact "A state transformer is a functor on its wrapped monad"
	  (eval-stid (fmap square st1) 0) => 144
	  (eval-stid (fmap upper st2) 0)  => "JIB-JAB")))


(deftest test-0015
  (let [sf1 (return-stid square)
	sf2 (return-stid upper)
	st1 (return-stid 12)
	st2 (return-stid "jib-jab")]
    (fact "A state transformer is an applicative functor on its wrapped monad"
	  (eval-stid (<*> sf1 st1) 0) => 144
	  (eval-stid (<*> sf2 st2) 0) => "JIB-JAB")
    (fact "A state transformer applicative functor can take multiple args"
	  (eval-stid (<*> (return-stid +)
			  st1 st1 st1 st1) 0) => 48)))


(deftest test-0020
  (let [sm1 (return-stid 60)
	sm2 (return-stid "jib-jab")
	st1 (monad [x sm1 y sm2 z (get-st ->Identity)]
	      (return-stid (+ x (count y) z)))]
    (fact "(get-st) provides the state as a value in chained actions"
	  (eval-stid st1 100) => 167)))


(deftest test-0025
  (let [sm1 (return-stid 60)
	sm2 (return-stid "jib-jab")
	st1 (monad [x (get-st ->Identity)
		    _ (put-st ->Identity 10)
		    y (get-st ->Identity)]
	      (return-stid (+ x y)))]
    (fact "(put-st) replaces the state in chained actions"
	  (eval-stid st1 100) => 110)))


(deftest test-0030
  (let [sm1 (return-stid 60)
	sm2 (return-stid "jib-jab")
	st1 (monad [x (get-st ->Identity)
		    _ (modify-st ->Identity * 4)
		    y (get-st ->Identity)]
	      (return-stid (+ x y)))]
    (fact "(modify-st) tranforms the state in chained actions"
	  (eval-stid st1 25) => 125)))


(deftest test-0035
  (let [sm1 (return-stid 60)
	sm2 (return-stid "jib-jab")
	st1 (monad [x sm1 y sm2 z (gets-st ->Identity :total)]
	      (return-stid (+ x (count y) z)))]
    (fact "(gets-st) provides the result of applying a selector on the state"
	  (eval-stid st1 {:id 1 :total 100}) => 167)))

;; =====================
;; StateT String Maybe a
;; =====================

(defn make-sm
  [x] (state-t just x))

(def nothing-sm (state-t (fn [_] nothing) nil))

(defn run-sm
  [m s] (eval-state-t m s))

(defn eval-sm
  [m s] (run-just (eval-state-t m s)))

(defn exec-sm
  [m s] (run-just (exec-state-t m s)))


(deftest test-0040
  (let [st (make-sm "foo")]
    (fact "run a state monad"
	  (eval-sm st 747) => "foo"
	  (exec-sm st 747) => 747)))


(deftest test-0045
  (let [st1 (make-sm 787)
	st2 (make-sm "foo")]
    (fact "prints the transformer monad"
	  (with-out-str (print st1)) => #(.startsWith % "StateT@")
	  (with-out-str (print st2)) => #(.startsWith % "StateT@"))
    (fact "prints the wrapped monad"
	  (with-out-str (print (run-sm st1 0))) => "Just 787")))


(deftest test-0050
  (let [st1 (make-sm 12)
	st2 (make-sm "jib-jab")]
    (fact "A state transformer is a functor on its wrapped monad"
	  (eval-sm (fmap square st1) 0) => 144
	  (eval-sm (fmap upper st2) 0)  => "JIB-JAB")))


(deftest test-0055
  (let [sf1 (make-sm square)
	sf2 (make-sm upper)
	st1 (make-sm 12)
	st2 (make-sm "jib-jab")]
    (fact "A state transformer is an applicative functor on its wrapped monad"
	  (eval-sm (<*> sf1 st1) 0) => 144
	  (eval-sm (<*> sf2 st2) 0) => "JIB-JAB")
    (fact "A state transformer applicative functor can take multiple args"
	  (eval-sm (<*> (make-sm +) st1 st1 st1 st1) 0) => 48)))


(deftest test-0060
  (let [m1 (make-sm 512)
	m2 (make-sm 256)
	m3 (monad [x m1 y m2] (make-sm (+ x y)))]
    (fact "(monad) chains instances and binds the values of the inner monad"
	  (eval-sm m3 "") => 768)))


(deftest test-0065
  (let [m1 (make-sm 512)
	m2 (make-sm 256)
	m3 nothing-sm
	m4 (monad [x m1 _ m3 y m2 z m3] (make-sm (+ x y z)))]
    (fact "(monad) short-circuits Nothing values"
	  (run-sm m4 0) => nothing?)))


(deftest test-0070
  (let [sm1 (make-sm 60)
	sm2 (make-sm "jib-jab")
	st1 (monad [x sm1 y sm2 z (get-st just)]
	      (make-sm (+ x (count y) z)))]
    (fact "(get-st) provides the state as a value in chained actions"
	  (eval-sm st1 100) => 167)))


(deftest test-0075
  (let [st (lift-st (just "foo"))]
    (fact "any Just value can be lifted to the StateT monad"
	  (run-just (eval-state-t st 0))   => "foo"
	  (run-just (exec-state-t st 747)) => 747)))


(deftest test-0080
  (let [st (lift-st (reader "foo"))]
    (fact "any monad with a value can be lifted to the StateT monad"
	  (run-reader (eval-state-t st 0) 0)   => "foo"
	  (run-reader (exec-state-t st 747) 0) => 747)))


;; +-------------------------------------------------------------+
;; |                The Reader Monad Transformer.                |
;; +-------------------------------------------------------------+

;; ====================
;; ReaderT r Identity a
;; ====================

(defn return-rtid
  [x] (reader-t id x))

(defn run-rtid
  [m s] (run-id (run-reader-t m s)))


(deftest test-0100
  (let [rm1 (return-rtid 60)]
    (fact "(return-reader-t) makes a ReaderT action."
	  (run-rtid rm1 "foo") => 60)))


(deftest test-0105
  (let [rm1 (return-rtid 60)]
    (fact "A ReaderT prints as ReaderT@nn"
	  (with-out-str (print rm1)) => #(.startsWith % "ReaderT@"))))


(deftest test-0110
  (let [rm1 (return-rtid 12)
	rm2 (return-rtid "jib-jab")]
    (fact "A ReaderT monad is a functor"
	  (run-rtid (fmap square rm1) "ENV") => 144
	  (run-rtid (fmap upper rm2) "ENV")  => "JIB-JAB")))


(deftest test-0115
  (let [rf1 (return-rtid square)
	rf2 (return-rtid upper)
	rm1 (return-rtid 12)
	rm2 (return-rtid "jib-jab")
	rdr (<*> (return-rtid +) [rm1 rm1 rm1])]
    (fact "A ReaderT monad is an applicative functor"
	  (run-rtid (<*> rf1 rm1) "ENV") => 144
	  (run-rtid (<*> rf2 rm2) "ENV") => "JIB-JAB")
    (fact "A ReaderT can be applied to multiple args"
	  (run-rtid rdr "ENV") => 36)))

	    
(deftest test-0120
  (let [rm1 (return-rtid 12)
	rm2 (return-rtid "jib-jab")
	rdr (monad [x rm1 y rm2] (return-rtid (+ x (count y))))]
    (fact "(monad) chains ReaderT actions"
	  (run-rtid rdr "env") => 19)))


(deftest test-0125
  (let [rm1 (return-rtid 12)
	rm2 (return-rtid "jib-jab")
	rdr (monad [x rm1 y (ask-rt ->Identity) z rm2]
	      (return-rtid (+ x y (count z))))]
    (fact "Reader ask gets the environment"
	  (run-rtid rdr 101) => 120)))


(deftest test-0130
  (let [rm1 (return-rtid 12)
	env {:name "Fernando" :speed 198}
	rdr (monad [x rm1 y (asks-rt ->Identity :speed)]
	      (return-rtid (+ x y)))]
    (fact "ReaderT asks applies a selector on the environment"
	  (run-rtid rdr env) => 210)))


(deftest test-0135
  (let [rm1 (return-rtid 15)
	rm2 (return-rtid 5)
	rm3 (monad [x rm2 y (ask-rt ->Identity)]
	      (return-rtid (+ x y)))
	rdr (monad [x rm1
		    y (ask-rt ->Identity)
		    z (local-rt ->Identity square rm3)]
	      (return-rtid (+ x y z)))]
    (fact "(local) performs an action uner a modified environment"
	  (run-rtid rdr 10) => 130)))


(deftest test-0140
  (let [rm1 (lift-rt (state 60))]
    (fact "(lift-rt) makes a ReaderT action from any monad."
	  (eval-state (run-reader-t rm1 0) "foo") => 60
	  (exec-state (run-reader-t rm1 0) "foo") => "foo")))


;; +-------------------------------------------------------------+
;; |                The Writer Monad Transformer.                |
;; +-------------------------------------------------------------+

;; =========================
;; WriterT String Identity a
;; =========================

(defn make-wtid
  ([a] (writer-t id a ""))
  ([a w] (writer-t id a w)))

(defn run-wtid
  [m] (run-id (run-writer-t m)))

(defn eval-wtid
  [m] (run-id (eval-writer-t m)))

(defn exec-wtid
  [m] (run-id (exec-writer-t m)))


(deftest test-0200
  (let [wm1 (make-wtid 60)
	wm2 (make-wtid 777 "foobar")]
    (fact "(make-wtid x) makes a WriterT with the empty string as output."
	  (eval-wtid wm1) => 60
	  (exec-wtid wm1) => empty?)
    (fact "(make-wtid x y) makes a WriterT with value x and output y."
	  (eval-wtid wm2) => 777
	  (exec-wtid wm2) => "foobar")))

(deftest test-0205
  (let [wm1 (make-wtid 60)
	wm2 (make-wtid 777 "foobar")]
    (fact "A WriterT prints as WriterT m (a, w)"
	  (with-out-str (print wm1)) => "WriterT Identity Pair(60,)"
	  (with-out-str (print wm2)) => "WriterT Identity Pair(777,foobar)")))


(deftest test-0210
  (let [wm1 (make-wtid 60)
	wm2 (make-wtid "foobar")]
    (fact "A WriterT is a functor"
	  (eval-wtid (fmap square wm1)) => 3600
	  (eval-wtid (fmap upper wm2))  => "FOOBAR")))


(deftest test-0215
  (let [wf1 (make-wtid square)
	wf2 (make-wtid upper)
	wm1 (make-wtid 60 "foo")
	wm2 (make-wtid "foobar")]
    (fact "A WriterT is an applicative functor"
	  (eval-wtid (<*> wf1 wm1)) => 3600
	  (eval-wtid (<*> wf2 wm2))  => "FOOBAR")
    (fact "A WriterT can be applied to multiple args"
	  (eval-wtid (<*> (make-wtid +)
			  wm1 wm1 wm1 wm1)) => 240)))


(deftest test-0220
  (let [wm1 (make-wtid 60 "foo")
	wm2 (make-wtid 99 "bar")
	wm3 (monad [x wm1 y wm2] (make-wtid (+ x y)))]
    (fact "(monad) chains writers and combines their output"
	  (eval-wtid wm3) => 159
	  (exec-wtid wm3) => "foobar")))


(deftest test-0225
  (let [wm1 (make-wtid 60 "foo")
	wm2 (make-wtid 99 "bar")
	wm3 (monad [x wm1 _ (tell-wt ->Identity " ") y wm2]
	      (make-wtid (+ x y)))]
    (fact "(tell x) makes a writer with output x"
	  (eval-wtid wm3) => 159
	  (exec-wtid wm3) => "foo bar")
    (fact "(tell x) contributes output"
	  (exec-wtid (>> (tell-wt ->Identity "The result is: ")
			 wm1
			 (tell-wt ->Identity "-")
			 wm2)) => "The result is: foo-bar")))


(deftest test-0230
  (let [wm1 (make-wtid 60 "foo")
	wm2 (make-wtid 99 "bar")
	wm3 (make-wtid 21 "baz")
	wm4 (make-wtid 76 "faz")
	out (listen-wt (monad [_ wm1 _ wm2] (make-wtid nil)))
	wrt (monad [x out] (if (= (snd x) "foobar") wm3 wm4))]
    (fact "(listen) provides the current chained output"
	  (eval-wtid wrt) => 21
	  (exec-wtid wrt) => "foobarbaz")))


(deftest test-0235
  (let [wm1 (make-wtid 60 "foo")
	wm2 (make-wtid 99 "bar")
	wf1 (monad [x wm1 y wm2] (make-wtid (->Pair (+ x y) upper)))
	wrt (pass-wt wf1)]
    (fact "(pass) uses an applicative output for transformations"
	  (eval-wtid wrt) => 159
	  (exec-wtid wrt) => "FOOBAR")))

;; ======================
;; WriterT String State a
;; ======================

(defn make-ws
  [x] (writer-t state x ""))

(defn eval-ws
  [m s] (eval-state (eval-writer-t m) s))

(defn exec-ws
  [m s] (exec-state (eval-writer-t m) s))

(defn get-log
  [m s] (eval-state (exec-writer-t m) s))

(defn liftw
  [m] (lift-wt m ""))

(defn tellw
  [s] (tell-wt state s))


(deftest test-0240
  (let [m1 (make-ws 500)
        m2 (make-ws 100)
        m3 (monad [x m1 y m2] (make-ws (+ x y)))]
    (fact "(monad) chains writers and combines their output"
	  (eval-ws m3 "") => 600)))


(deftest test-0245
  (let [m1 (make-ws 500)
        m2 (make-ws 100)
        m3 (monad [x m1 _ (tellw "What, me worry?") y m2] (make-ws (+ x y)))]
    (fact "(tellw) appends output to the writer"
	  (eval-ws m3 "") => 600
	  (get-log m3 "") => "What, me worry?")))


(deftest test-0250
  (let [m1 (make-ws 500)
        m2 (make-ws 100)
	m3 (make-ws 900)
        m4 (monad [x m1
                   _ (liftw (put-state "State: Now is the time"))
		   _ (tellw "What, me worry?")
		   y m2
		   z m3] (make-ws (+ x y z)))]
    (fact "(liftw) turns the inner monad from (put-state) into the outer monad"
	  (eval-ws m4 "") => 1500
	  (exec-ws m4 "") => "State: Now is the time"
	  (get-log m4 "") => "What, me worry?")))


;; +-------------------------------------------------------------+
;; |                The Maybe Monad Transformer.                 |
;; +-------------------------------------------------------------+

;; =================
;; MaybeT Identity a
;; =================

(defn make-mtid
  [a] (just-t id a))

(def mt-nothing (nothing-t id))

(defn run-mtid
  [m] (run-id (run-just-t m)))

(defn eval-mtid
  [m] (run-id (eval-just-t m)))

(defn mt-just?
  [m] (just? (run-mtid m)))

(defn mt-nothing?
  [m] (nothing? (run-mtid m)))


(deftest test-0300
  (let [mm1 (make-mtid 60)
	mm2 (make-mtid "foobar")]
    (fact "(make-mtid x) makes a MaybeT with the Identity monad"
	  (eval-mtid mm1) => 60
          (eval-mtid mm2) => "foobar")))


(deftest test-0305
  (let [mm1 (make-mtid 60)
	mm2 (make-mtid "foobar")
	mm3 mt-nothing]
    (fact "MaybeT prints as MaybeT <Monad> Maybe x"
	  (with-out-str (print mm1)) => "MaybeT Identity Just 60"
	  (with-out-str (print mm2)) => "MaybeT Identity Just foobar"
	  (with-out-str (print mm3)) => "MaybeT Identity Nothing")))


(deftest test-0310
  (let [mm1 (make-mtid 60)
	mm2 mt-nothing]
    (fact "(mt-just) checks for a Just value"
	  mm1   =>   mt-just?
          mm2   =>   mt-nothing?
	  mm1 =not=> mt-nothing?
	  mm2 =not=> mt-just?)))


(deftest test-0315
  (let [mm1 (make-mtid 60)
	mm2 (make-mtid "foobar")
	mm3 mt-nothing]
    (fact "A MaybeT is a functor"
	  (eval-mtid (fmap square mm1)) => 3600
	  (eval-mtid (fmap upper mm2))  => "FOOBAR"
	              (fmap square mm3) => mt-nothing?)))


(deftest test-0320
  (let [mf1 (make-mtid square)
	mf2 (make-mtid upper)
	mm1 (make-mtid 60)
	mm2 (make-mtid "foobar")
	mm3 mt-nothing]
    (fact "A MaybeT is an applicative functor"
	  (eval-mtid (<*> mf1 mm1)) => 3600
	  (eval-mtid (<*> mf2 mm2))  => "FOOBAR")
    (fact "A MaybeT can be applied to multiple args"
	  (eval-mtid (<*> (make-mtid +)
		          mm1 mm1 mm1 mm1 mm1)) => 300)
    (fact "A nothing value will short-circuit the computation to nothing"
	  (<*> (make-mtid +) mm1 mm1 mm3 mm1 mm1 mm1) => mt-nothing?)))


(deftest test-0325
  (let [mm1 (make-mtid 60)
	mm2 (make-mtid "foobar")]
    (fact "(monad) chains MaybeT values."
	  (eval-mtid (monad [x mm1 y mm2]
		       (make-mtid (* x (count y))))) => 360))) 


(deftest test-0330
  (let [mm1 (make-mtid 60)
	mm2 (make-mtid "foobar")
	mm3 mt-nothing]
    (fact "(monad) short-circuits Nothing values."
	  (monad [x mm1 _ mm3 y mm2]
	    (make-mtid (* x (count y)))) => mt-nothing?))) 


(deftest test-0335
  (let [mm1 (lift-mt (state 60))
	mm2 (lift-mt (reader "foobar"))]
    (fact "(lift-mt) makes a MaybeT with any monad"
	  (eval-state (eval-just-t mm1) "")    => 60
	  (exec-state (eval-just-t mm1) "baz") => "baz"
          (run-reader (eval-just-t mm2) 0)     => "foobar")))


;; +-------------------------------------------------------------+
;; |                The Either Monad Transformer.                |
;; +-------------------------------------------------------------+

;; ==================
;; EitherT Identity a
;; ==================

(defn make-right-etid
  [a] (right-t id a))

(defn make-left-etid
  [a] (left-t id a))

(defn run-etid
  [m] (run-id (run-either-t m)))

(defn right-etid
  [m] (run-id (run-right-t m)))

(defn left-etid
  [m] (run-id (run-left-t m)))


(deftest test-0400
  (let [em1 (make-right-etid 60)
	em2 (make-right-etid "foobar")]
    (fact "(make-right-mtid x) makes a Right with the Identity monad"
	  (right-etid em1) => 60
          (right-etid em2) => "foobar")))


(deftest test-0405
  (let [em1 (make-left-etid 60)
	em2 (make-left-etid "foobar")]
    (fact "(make-left-mtid x) makes a Left with the Identity monad"
	  (left-etid em1) => 60
          (left-etid em2) => "foobar")))


(deftest test-0410
  (let [em1 (make-right-etid 60)
	em2 (make-right-etid "foobar")
        em3 (make-left-etid 49)
	em4 (make-left-etid "jib-jab")]
    (fact "A Right prints as EitherT <Monad> Right x"
	  (with-out-str (print em1)) => "EitherT Identity Right 60"
	  (with-out-str (print em2)) => "EitherT Identity Right foobar")
    (fact "A Left prints as EitherT <Monad> Left x"
	  (with-out-str (print em3)) => "EitherT Identity Left 49"
	  (with-out-str (print em4)) => "EitherT Identity Left jib-jab")))


(deftest test-0415
  (let [em1 (make-right-etid 60)
	em2 (make-right-etid "foobar")
        em3 (make-left-etid 49)
	em4 (make-left-etid "jib-jab")]
    (fact "A Right is a functor"
	  (right-etid (fmap square em1)) => 3600
	  (right-etid (fmap upper em2))  => "FOOBAR")
    (fact "A Left will ignore its use as a functor"
	  (left-etid (fmap square em3))  => 49
	  (left-etid (fmap upper em4))   => "jib-jab")))


(deftest test-0420
  (let [ef1 (make-right-etid square)
	ef2 (make-right-etid upper)
	em1 (make-right-etid 60)
	em2 (make-right-etid "foobar")]
    (fact "A Right is an applicative functor"
	  (right-etid (<*> ef1 em1)) => 3600
	  (right-etid (<*> ef2 em2)) => "FOOBAR")
    (fact "A Right can be applied to multiple args"
	  (right-etid (<*> (make-right-etid +)
			   em1 em1 em1 em1)) => 240)))


(deftest test-0425
  (let [em1 (make-right-etid 50)
	em2 (make-right-etid "foobar")
        em3 (make-right-etid 20)
	em4 (make-right-etid "jib-jab")
        em5 (make-left-etid 99)
	val (monad [w em1 x em2 y em3 z em4]
	      (return em1 (+ w (count x) y (count z))))]
    (fact "(monad) chains Right values"
	  (right-etid val) => 83)
    (fact "(monad) short-circuits Left values"
	  (left-etid (monad [x em1 _ em5 y em2 z em3]
		       (make-right-etid (+ x y z)))) => 99)))


(deftest test-0430
  (let [mm1 (lift-et (state 60))
	mm2 (lift-et (reader "foobar"))]
    (fact "(lift-mt) makes an EitherT (Right) with any monad"
	  (eval-state (run-right-t mm1) "")    => 60
	  (exec-state (run-right-t mm1) "baz") => "baz"
          (run-reader (run-right-t mm2) 0)     => "foobar")))
