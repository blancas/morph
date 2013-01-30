;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns blancas.morph.test-monads
  (:use [blancas.morph core monads]
	[clojure.test]
	[midje.sweet :only (fact)]))


;; +-------------------------------------------------------------+
;; |                         Utilities.                          |
;; +-------------------------------------------------------------+


(defn square [x] (* x x))

(defn upper [^String x] (.toUpperCase x))


;; +-------------------------------------------------------------+
;; |                     The Identity Monad.                     |
;; +-------------------------------------------------------------+


(deftest test-0000
  (fact "run a trivial id"
	(run-id (id "foo")) => "foo"))


(deftest test-0005
  (let [idm (id 787)]
    (fact "print an identity"
	  (with-out-str (print idm)) => "Identity 787")))


(deftest test-0010
  (let [idm1 (id 12)
	idm2 (id "Jumbo Jet")]
    (fact "an identity is a functor - numeric"
	  (run-id (fmap square idm1)) => 144)
    (fact "an identity is a functor - string"
	  (run-id (fmap upper idm2)) => "JUMBO JET")))


(deftest test-0015
  (let [idf1 (id square)
	idf2 (id upper)
	idm1 (id 12)
	idm2 (id "Jumbo Jet")]
    (fact "an identity is an applicative functor - numeric"
	  (run-id (<*> idf1 idm1)) => 144)
    (fact "an identity is a applicative functor - string"
	  (run-id (<*> idf2 idm2)) => "JUMBO JET")))


(deftest test-0020
  (let [idf1 (id +)
	idm1 (id 512)
	idm2 (id 128)]
    (fact "an identity is an applicative functor - multiple args, numeric"
	  (run-id (<*> idf1 idm1 idm2)) => 640)))


(deftest test-0025
  (let [idm1 (id 512)
	idm2 (id 128)
	idm3 (id 256)
	sum  (monad [x idm1 y idm2 z idm3] (id (+ x y z)))]
    (fact "identities can be chained with (monad) - numeric"
	  (run-id sum) => 896)))


(deftest test-0030
  (let [idm1 (id 512)
	idm2 (id 128)
	idm3 (id 256)
	sum  (monad [x idm1 y idm2 z idm3] (return idm1 (+ x y z)))]
    (fact "(return) can be polymorphic on an identity"
	  (run-id sum) => 896)))


;; +-------------------------------------------------------------+
;; |                      The Maybe Monad.                       |
;; +-------------------------------------------------------------+


(deftest test-0200
  (let [mm1 nothing]
    (fact "nothing is the value for Nothing"
	  mm1   =>   nothing?
	  mm1 =not=> just?)))


(deftest test-0205
  (let [mm1 nothing]
    (fact "A nothing value prints as Nothing"
	  (with-out-str (print mm1)) => "Nothing")))


(deftest test-0210
  (let [mm1 (just 5005)]
    (fact "just makes a valued Maybe called Just"
	  (run-just mm1)  =>   5005
	  mm1             =>   just?
	  mm1           =not=> nothing?)))


(deftest test-0215
  (let [mm1 (just 5005)]
    (fact "A Just value prints as Just x"
	  (with-out-str (print mm1)) => "Just 5005")))


(deftest test-0220
  (let [mm1 (just 12)
	mm2 (monad [x mm1] (just (square x)))]
    (fact "(monad) chains Just computations"
	  (run-just mm2) => 144)))


(deftest test-0225
  (let [mm1 nothing
	mm2 (monad [x mm1] (just (square x)))]
    (fact "(monad) short-circuits Nothing computations"
	  mm2 => nothing?)))


(deftest test-0230
  (let [mm1 (just 12)
	mm2 (just "jib-jab")
	mm3 nothing]
    (fact "A Maybe value is a functor"
	  (run-just (fmap square mm1)) => 144
	  (run-just (fmap upper mm2))  => "JIB-JAB"
	  (fmap upper mm3)             => nothing?)))


(deftest test-0235
  (let [mf1 (just square)
	mf2 (just upper)
	mm1 (just 12)
	mm2 (just "jib-jab")
	mm3 nothing]
    (fact "A Maybe value is an applicative functor"
	  (run-just (<*> mf1 mm1)) => 144
	  (run-just (<*> mf2 mm2)) => "JIB-JAB"
	  (<*> mf1 mm3)            => nothing?)))


(deftest test-0240
  (let [mf1 (just max)
	mm1 (just 50)
	mm2 (just 99)
	mm3 (just 20)]
    (fact "A Maybe value is an applicative functor -- multiple args"
	  (run-just (<*> mf1 mm1 mm2 mm3)) => 99)))


(deftest test-0245
  (let [mf1 (just max)
	mm1 (just 50)
	mm2 nothing
	mm3 (just 20)
	mm4 (just 35)]
    (fact "applicative functor -- multiple args, short-circuit Nothing"
	  (<*> mf1 mm1 mm2 mm3 mm4) => nothing?)))


(deftest test-0250
  (let [mm1 (just 12)
	mm2 (just "jib-jab")
	val (monad [x mm1 y mm2] (just (+ x (count y))))]
    (fact "(monad) chains Maybe values with access to their values"
	  (run-just val) => 19)))


(deftest test-0255
  (let [mm1 (just 50)
	mm2 (just 12)
	mm3 (just 99)
	mm4 (just 20)]
    (fact "(monad) with multiple bindings"
	  (run-just (monad [w mm1 x mm2 y mm3 z mm4]
	          (just (max w x y z)))) => 99)))


(deftest test-0260
  (let [mm1 (just 50)
	mm2 (just 12)
	mm3 nothing
	mm4 (just 99)]
    (fact "(monad) will short-circuit Nothing."
	  (monad [w mm1 x mm2 y mm3 z mm4]
	    (just (max w x y z))) => nothing?)))


(deftest test-0265
  (let [mm1 (just 50)
	mm2 (just 12)
	mm3 (just 99)
	mm4 (just 20)
	zap nothing]
    (fact "(justs) filters Just values"
	  (justs [mm1 zap mm2 zap mm3 zap mm4]) => [50 12 99 20])))


(deftest test-0270
  (fact "map-maybe maps f over a collection and collects Just values."
	(map-maybe just (range 10)) => (range 10)))


(deftest test-0275
  (let [f (fn [n] (if (even? n) (just (- n)) nothing))]
    (fact "map-maybe filters out Nothing values."
	  (map-maybe f (range 12)) => '(0 -2 -4 -6 -8 -10))))


;; +-------------------------------------------------------------+
;; |                      The Either Monad.                      |
;; +-------------------------------------------------------------+


(deftest test-0400
  (let [em1 (left 60)]
    (fact "(left) makes a Left value."
	  (run-left em1) =>   60
	  em1            =>   left?
	  em1          =not=> right?)))


(deftest test-0405
  (let [em1 (left 60)]
    (fact "A Left value prints as Left x"
	  (with-out-str (print em1)) => "Left 60")))


(deftest test-0410
  (let [em1 (right 5005)]
    (fact "(right) makes a Right value"
	  (run-right em1) =>   5005
	  em1             =>   right?
	  em1           =not=> left?)))


(deftest test-0415
  (let [em1 (right 5005)]
    (fact "A Right value prints as Right x"
	  (with-out-str (print em1)) => "Right 5005")))


(deftest test-0420
  (let [em1 (right 12)
	em2 (right "jib-jab")
	em3 (left 100)]
    (fact "An Either value is a functor"
	  (run-right (fmap square em1)) => 144
	  (run-right (fmap upper em2))  => "JIB-JAB"
	  (run-left (fmap square em3))  => 100)))


(deftest test-0425
  (let [ef1 (right square)
	ef2 (right upper)
	ef3 (left square)
	em1 (right 12)
	em2 (right "jib-jab")
	em3 (left 100)
	em4 (<*> (right *) [em1 em1])]
    (fact "An Either value is an applicative functor"
	  (run-right (<*> ef1 em1)) => 144
	  (run-right (<*> ef2 em2)) => "JIB-JAB"
	  (run-left  (<*> ef1 em3)) => 100
	  (<*> ef3 em1)             => left?)
    (fact "An Either function can be applied to multiple args"
	  (run-right em4) => 144)))


(deftest test-0430
  (fact "(monad) chains Either actions."
	(run-right (monad [v1 (right 12)
	                   v2 (right "jib-jab")
	                   v3 (right 100)]
	             (right (+ v1 (count v2) v3)))) => 119))


(deftest test-0435
  (fact "(monad) short-circuits Left actions."
	(run-left (monad [v1 (right 12)
	                  v2 (left "FAILED")
	                  v3 (right 100)]
	         (right (+ v1 (count v2) v3)))) => "FAILED"))


(deftest test-0440
  (let [em1 (left 50)
	em2 (left 12)
	em3 (left 99)
	em4 (left 20)
	zap (right 0)]
    (fact "(lefts) filters Left values"
	  (lefts [em1 zap em2 zap em3 zap em4]) => [50 12 99 20])))


(deftest test-0445
  (let [em1 (right 50)
	em2 (right 12)
	em3 (right 99)
	em4 (right 20)
	zap (left 0)]
    (fact "(rights) filters Right values"
	  (rights [em1 zap em2 zap em3 zap em4]) => [50 12 99 20])))


(deftest test-0450
  (fact "map-either maps f over a collection and collects Right values."
	(map-either right (range 10)) => (range 10)))


(deftest test-0455
  (let [f (fn [n] (if (even? n) (right (- n)) (left n)))]
    (fact "map-either filters out Left values."
	  (map-either f (range 12)) => '(0 -2 -4 -6 -8 -10))))


;; +-------------------------------------------------------------+
;; |                      The Reader Monad.                      |
;; +-------------------------------------------------------------+


(deftest test-0600
  (let [rm1 (reader 60)]
    (fact "(reader) makes a Reader value."
	  (run-reader rm1 "foo") => 60)))


(deftest test-0605
  (let [rm1 (reader 60)]
    (fact "A Reader value prints as Reader"
	  (with-out-str (print rm1)) => #(.startsWith % "Reader@"))))


(deftest test-0610
  (let [rm1 (reader 12)
	rm2 (reader "jib-jab")]
    (fact "A Reader monad is a functor"
	  (run-reader (fmap square rm1) "ENV") => 144
	  (run-reader (fmap upper rm2) "ENV")  => "JIB-JAB")))


(deftest test-0615
  (let [rf1 (reader square)
	rf2 (reader upper)
	rm1 (reader 12)
	rm2 (reader "jib-jab")
	rdr (<*> (reader +) [rm1 rm1 rm1])]
    (fact "A Reader monad is an applicative functor"
	  (run-reader (<*> rf1 rm1) "ENV") => 144
	  (run-reader (<*> rf2 rm2) "ENV") => "JIB-JAB")
    (fact "A Reader can be applied to multiple args"
	  (run-reader rdr "ENV") => 36)))

	    
(deftest test-0620
  (let [rm1 (reader 12)
	rm2 (reader "jib-jab")
	rdr (monad [x rm1 y rm2] (reader (+ x (count y))))]
    (fact "(monad) chains Reader actions"
	  (run-reader rdr "env") => 19)))


(deftest test-0625
  (let [rm1 (reader 12)
	rm2 (reader "jib-jab")
	rdr (monad [x rm1 y ask z rm2] (reader (+ x y (count z))))]
    (fact "Reader ask gets the environment"
	  (run-reader rdr 101) => 120)))


(deftest test-0630
  (let [rm1 (reader 12)
	env {:name "Fernando" :speed 198}
	rdr (monad [x rm1 y (asks :speed)] (reader (+ x y)))]
    (fact "Reader asks applies a selector on the environment"
	  (run-reader rdr env) => 210)))


(deftest test-0635
  (let [rm1 (reader 15)
	rm2 (reader 5)
	rm3 (monad [x rm2 y ask] (reader (+ x y)))
	rdr (monad [x rm1 y ask z (local square rm3)] (reader (+ x y z)))]
    (fact "(local) performs an action uner a modified environment"
	  (run-reader rdr 10) => 130)))


;; +-------------------------------------------------------------+
;; |                      The Writer Monad.                      |
;; +-------------------------------------------------------------+


(deftest test-0800
  (let [wm1 (writer 60 str-id)
	wm2 (writer 777 "foobar")]
    (fact "(writer x) makes a Writer value with mempty string output."
	  (eval-writer wm1) => 60
	  (exec-writer wm1) => empty?)
    (fact "(writer x y) makes a Writer with value x and output y."
	  (eval-writer wm2) => 777
	  (exec-writer wm2) => "foobar")))

(deftest test-0805
  (let [wm1 (writer 60 str-id)
	wm2 (writer 777 "foobar")]
    (fact "A Writer prints as Writer x y"
	  (with-out-str (print wm1)) => "Writer(60,)"
	  (with-out-str (print wm2)) => "Writer(777,foobar)")))


(deftest test-0810
  (let [wm1 (writer 60 "foo")
	wm2 (writer "foobar" "")]
    (fact "A Writer is a functor"
	  (eval-writer (fmap square wm1)) => 3600
	  (eval-writer (fmap upper wm2))  => "FOOBAR")))


(deftest test-0815
  (let [wf1 (writer square "")
	wf2 (writer upper "")
	wm1 (writer 60 "foo")
	wm2 (writer "foobar" "")]
    (fact "A Writer is an applicative functor"
	  (eval-writer (<*> wf1 wm1)) => 3600
	  (eval-writer (<*> wf2 wm2))  => "FOOBAR")
    (fact "A Writer can be applied to multiple args"
	  (eval-writer (<*> (return wm1 +)
			    wm1 wm1 wm1 wm1)) => 240)))


(deftest test-0820
  (let [wm1 (writer 60 "foo")
	wm2 (writer 99 "bar")
	wm3 (monad [x wm1 y wm2] (return wm1 (+ x y)))]
    (fact "(monad) chains writers and combines their output"
	  (eval-writer wm3) => 159
	  (exec-writer wm3) => "foobar")))


(deftest test-0825
  (let [wm1 (writer 60 "foo")
	wm2 (writer 99 "bar")
	wm3 (monad [x wm1 _ (tell " ") y wm2] (return wm1 (+ x y)))]
    (fact "(tell x) makes a writer with output x"
	  (eval-writer wm3) => 159
	  (exec-writer wm3) => "foo bar")
    (fact "(tell x) contributes output"
	  (exec-writer (>> (tell "The result is: ")
			   wm1 (tell "-") wm2)) => "The result is: foo-bar")))


(deftest test-0830
  (let [wm1 (writer 60 "foo")
	wm2 (writer 99 "bar")
	wm3 (writer 21 "baz")
	wm4 (writer 76 "faz")
	out (listen (monad [_ wm1 _ wm2] (return wm1 nil)))
	wrt (monad [x out] (if (= (snd x) "foobar") wm3 wm4))]
    (fact "(listen) provides the current chained output"
	  (eval-writer wrt) => 21
	  (exec-writer wrt) => "foobarbaz")))
   

(deftest test-0835
  (let [wm1 (writer 60 "foo")
	wm2 (writer 99 "bar")
	wf1 (monad [x wm1 y wm2] (return wm1 (->Pair (+ x y) upper)))
	wrt (pass wf1)]
    (fact "(pass) uses an applicative output for transformations"
	  (eval-writer wrt) => 159
	  (exec-writer wrt) => "FOOBAR")))


;; +-------------------------------------------------------------+
;; |                      The State Monad.                       |
;; +-------------------------------------------------------------+

(deftest test-1000
  (let [sm1 (state {:one 1 :two 2})
	sm2 (state "foobar")]
    (fact "(state x) makes a State action"
	  (eval-state sm1 "foo") => {:one 1 :two 2}
	  (exec-state sm1 "foo") => "foo"
	  (eval-state sm2 5005)  => "foobar"
	  (exec-state sm2 5005)  => 5005)))


(deftest test-1005
  (let [sm1 (state 60)
	sm2 (state "foobar")]
    (fact "A Reader value prints as Reader"
	  (with-out-str (print sm1)) => #(.startsWith % "State@")
	  (with-out-str (print sm2)) => #(.startsWith % "State@"))))


(deftest test-1010
  (let [sm1 (state 60)
	sm2 (state "jib-jab")]
    (fact "A State monad is a functor"
	  (eval-state (fmap square sm1) 0) => 3600
	  (eval-state (fmap upper sm2) 0)  => "JIB-JAB")))


(deftest test-1015
  (let [sf1 (state square)
	sf2 (state upper)
	sm1 (state 60)
	sm2 (state "jib-jab")]
    (fact "A State monad is an applicative functor"
	  (eval-state (<*> sf1 sm1) 0) => 3600
	  (eval-state (<*> sf2 sm2) 0)  => "JIB-JAB")
    (fact "A State monad can be applied to multiple args"
	  (eval-state (<*> (state +)
			   sm1 sm1 sm1 sm1) 0) => 240)))


(deftest test-1020
  (let [sm1 (state 60)
	sm2 (state "jib-jab")
	st1 (monad [x sm1 y sm2]
	      (state (- x (count y))))]
    (fact "(monad) chains State actions"
	  (eval-state st1 0) => 53)))


(deftest test-1025
  (let [sm1 (state 60)
	sm2 (state "jib-jab")
	st1 (monad [x sm1 y sm2 z get-state]
	      (state (+ x (count y) z)))]
    (fact "(get-state) provides the state as a value in chained actions"
	  (eval-state st1 100) => 167)))


(deftest test-1030
  (let [sm1 (state 60)
	sm2 (state "jib-jab")
	st1 (monad [x get-state _ (put-state 10) y get-state]
	      (state (+ x y)))]
    (fact "(put-state) replaces the state in chained actions"
	  (eval-state st1 100) => 110)))


(deftest test-1035
  (let [sm1 (state 60)
	sm2 (state "jib-jab")
	st1 (monad [x get-state _ (modify-state * 4) y get-state]
	      (state (+ x y)))]
    (fact "(modify-state) tranforms the state in chained actions"
	  (eval-state st1 25) => 125)))


(deftest test-1040
  (let [sm1 (state 60)
	sm2 (state "jib-jab")
	st1 (monad [x sm1 y sm2 z (gets :total)]
	      (state (+ x (count y) z)))]
    (fact "(gets) provides the result of applying a selector on the state"
	  (eval-state st1 {:id 1 :total 100}) => 167)))


;; +-------------------------------------------------------------+
;; |                    Functions on monads.                     |
;; +-------------------------------------------------------------+


(deftest test-1200
  (let [sm1 (state 60)
	sm2 (put-state [1 2 3])
	sm3 (state "jib-jab")
	sm4 (modify-state conj 4 5 6)
	sm5 (state 5005)
	st1 (>> sm1 sm2 sm3 sm4 sm5)]
    (fact "(>>) keeps the last value and ignores all previous ones"
	  (eval-state st1 []) => 5005
	  (exec-state st1 []) => [1 2 3 4 5 6])))


(deftest test-1205
  (let [sm1 (state 60)
	sm2 (put-state [1 2 3])
	sm3 (state "jib-jab")
	sm4 (modify-state conj 4 5 6)
	sm5 (state 5005)
	st1 (<< sm1 sm2 sm3 sm4 sm5)]
    (fact "(<<) keeps the first value and ignores all the rest"
	  (eval-state st1 []) => 60
	  (exec-state st1 []) => [1 2 3 4 5 6])))


(deftest test-1210
  (let [sm1 (state [1 2])
	sm2 (state [3 8])
	sm3 (state [9 2])
	sm4 (state [4 7])
	st1 (seqm [sm1 sm2 sm3 sm4])]
    (fact "(seqm) transforms [ma] -> m [a]"
	  (eval-state st1 [])    => '([1 2] [3 8] [9 2] [4 7])
	  (exec-state st1 "foo") => "foo")))


(deftest test-1215
  (let [sm1 (state [1 2])
	sm2 (state [3 8])
	sm3 (state [9 2])
	sm4 (state [4 7])
	st1 (seqm_ [sm1 sm2 sm3 sm4])]
    (fact "(seqm_) transforms [ma] -> m []"
	  (eval-state st1 [])    => ()
	  (exec-state st1 "foo") => "foo")))


(deftest test-1220
  (let [sqr (fn [n] (state (square n)))
	st1 (mapm sqr (range 10))]
    (fact "(mapm) maps a -> m b for each [a] and returns m [b]"
	  (eval-state st1 []) => '(0 1 4 9 16 25 36 49 64 81))))


(deftest test-1225
  (let [m1 (state 10)
        m2 (state 99)
	m3 (state 50)
	m4 (state 38)
	fx (fn [a b c d] [[a b] [c d]])]
    (fact "(liftm) applies a regular function on monadic values"
	  (eval-state (liftm square m1) [])       => 100
	  (eval-state (liftm2 * m1 m2) [])        => 990
	  (eval-state (liftm3 + m1 m2 m3) [])     => 159
	  (eval-state (liftm4 fx m1 m2 m3 m4) []) => [[10 99] [50 38]])))
