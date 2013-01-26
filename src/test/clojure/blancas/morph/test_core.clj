;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns blancas.morph.test-core
  (:use [blancas.morph.core]
	[clojure.test]
	[midje.sweet :only (fact)]))


;; +-------------------------------------------------------------+
;; |                         Utilities.                          |
;; +-------------------------------------------------------------+


(defn square [x] (* x x))

(defn to-upper [^String x] (.toUpperCase x))


;; +-------------------------------------------------------------+
;; |                          Monoids.                           |
;; +-------------------------------------------------------------+


(deftest test-0000
  (let [s1 (->Sum 50)
	s2 (->Sum 3.1416)]
    (fact "a Sum is a monoid for sums of numbers"
	  (sum s1)              => 50
	  (sum s2)              => 3.1416
	  (sum (mempty s1))     => 0
	  (sum (mempty s2))     => 0
	  (sum (mappend s1 s2)) => 53.1416)
    (fact "Sum implements Object and Comparable"
	  (= s1 (->Sum 50))     => true
	  (= s1 s2)             => false
	  (sort [s1 s2])        => [s2 s1])))


(deftest test-0005
  (let [s1 (->Sum 50)
	s2 (->Sum 3.1416)]
    (fact "a Sum value prints as Sum x"
	  (with-out-str (print s1)) => "Sum 50"
	  (with-out-str (print s2)) => "Sum 3.1416")))


(deftest test-0010
  (let [s1 (->Sum 50)
	s2 (->Sum 3.1416)]
    (fact "(mconcat) works with Sum monoids"
	  (sum (mconcat [s1 s2 s1 s2])) => 106.2832)))


(deftest test-0015
  (let [p1 (->Product 5)
	p2 (->Product 0.25)]
    (fact "a Product is a monoid for a product of numbers"
	  (product p1)              => 5
	  (product p2)              => 0.25
	  (product (mempty p1))     => 1
	  (product (mempty p2))     => 1
	  (product (mappend p1 p2)) => 1.25)
    (fact "Product implements Object and Comparable"
	  (= p1 (->Product 5))      => true
	  (= p1 p2)                 => false
	  (sort [p1 p2])            => [p2 p1])))


(deftest test-0020
  (let [p1 (->Product 50)
	p2 (->Product 3.1416)]
    (fact "a Product value prints as Product x"
	  (with-out-str (print p1)) => "Product 50"
	  (with-out-str (print p2)) => "Product 3.1416")))


(deftest test-0025
  (let [p1 (->Product 6)
	p2 (->Product 1.5)]
    (fact "(mconcat) works with Product monoids"
	  (product (mconcat [p1 p2 p1 p2])) => 81.0)))


(deftest test-0030
  (let [m1 (->Any false)
	m2 (->Any true)]
    (fact "an Any is a monoid for an OR on booleans"
	  (any m1)              => false
	  (any m2)              => true
	  (any (mempty m1))     => false
	  (any (mempty m2))     => false
	  (any (mappend m1 m2)) => true)
    (fact "Any implements Object and Comparable"
	  (= m1 (->Any false))  => true
	  (= m1 m2)             => false
	  (sort [m1 m2])        => [m1 m2])))


(deftest test-0035
  (let [m1 (->Any false)
	m2 (->Any true)]
    (fact "an Any value prints as Any x"
	  (with-out-str (print m1)) => "Any false"
	  (with-out-str (print m2)) => "Any true")))


(deftest test-0040
  (let [m1 (->Any false)
	m2 (->Any true)]
    (fact "(mconcat) works with Any monoids"
	  (any (mconcat [m1 m2 m1 m2])) => true
	  (any (mconcat [m1 m1 m1 m1])) => false)))


(deftest test-0045
  (let [m1 (->All false)
	m2 (->All true)]
    (fact "an All is a monoid for an AND on booleans"
	  (all m1)              => false
	  (all m2)              => true
	  (all (mempty m1))     => true
	  (all (mempty m2))     => true
	  (all (mappend m1 m2)) => false)
    (fact "All implements Object and Comparable"
	  (= m1 (->All false))  => true
	  (= m1 m2)             => false
	  (sort [m1 m2])        => [m1 m2])))


(deftest test-0050
  (let [m1 (->All false)
	m2 (->All true)]
    (fact "an All value prints as All x"
	  (with-out-str (print m1)) => "All false"
	  (with-out-str (print m2)) => "All true")))


(deftest test-0055
  (let [m1 (->All false)
	m2 (->All true)]
    (fact "(mconcat) works with All monoids"
	  (all (mconcat [m1 m2 m1 m2])) => false
	  (all (mconcat [m2 m2 m2 m2])) => true)))


(deftest test-0060
  (let [m1 (->Pair (->Sum 50) str-id)
	m2 (->Pair (->Sum 15) "jib")
	m3 (->Pair (->Sum 20) "-jab")
	m4 (->Pair (->Sum 20) "zap")]
    (fact "a Pair is a monoid that contains two monoids"
	  (sum (fst m1))              => 50
	  (snd m1)                    => empty?
	  (snd (mempty m1))           => empty?
	  (sum (fst (mempty m2)))     => 0
	  (sum (fst (mappend m2 m3))) => 35
	  (snd (mappend m2 m3))       => "jib-jab")
    (fact "Pair implements Object and Comparable"
	  (= m1 (->Pair (->Sum 50) "")) => true
	  (= m1 m2)                     => false
	  (sort [m1 m2 m4 m3])          => [m2 m3 m4 m1])))


(deftest test-0065
  (let [m1 (->Pair (->Sum 50) 5005)
	m2 (->Pair (->Product 100) "jib-jab")]
    (fact "a Pair value prints as Pair(x,y)"
	  (with-out-str (print m1)) => "Pair(Sum 50,5005)"
	  (with-out-str (print m2)) => "Pair(Product 100,jib-jab)")))


(deftest test-0070
  (let [m1 (->Pair (->Sum 50) "showing ")
	m2 (->Pair (->Sum 15) "jib")
	m3 (->Pair (->Sum 20) "-jab")
	m4 (mconcat [m1 m2 m3])]
    (fact "(mconcat) works with Pair monoids"
	  (sum (fst m4)) => 85
	  (snd m4)       => "showing jib-jab")))


;; +-------------------------------------------------------------+
;; |                          Functors.                          |
;; +-------------------------------------------------------------+


(deftest test-0100
  (fact "a list is a functor"
	(fmap square '(3 7 11 13 20)) => '(9 49 121 169 400)))


(deftest test-0105
  (fact "the empty list is a functor"
	(fmap square ()) => ()))


(deftest test-0110
  (fact "a vector is a functor"
	(fmap square [3 7 11 13 20]) => '(9 49 121 169 400)))


(deftest test-0115
  (fact "a subvector is a functor"
	(fmap square (subvec [1 2 3 7 11 13 20] 2)) => '(9 49 121 169 400)))


(deftest test-0120
  (fact "a hash set is a functor"
	(fmap square #{3 7 11 13 20}) => '(9 49 121 169 400)))


(deftest test-0125
  (fact "a sorted set is a functor"
	(fmap square (sorted-set 8 3 7 12 6 4)) => '(9 16 36 49 64 144)))


(deftest test-0130
  (fact "an array map is a functor"
	(fmap square (array-map :one 20 :two 30)) => '([:one 400] [:two 900])))


(deftest test-0135
  (fact "a hash map is a functor"
	(fmap square (hash-map :one 20 :two 30)) => '([:one 400] [:two 900])))


(deftest test-0140
  (fact "a sorted map is a functor"
	(fmap square (sorted-map :one 20 :two 30)) => '([:one 400] [:two 900])))


(deftest test-0145
  (let [e (clojure.lang.PersistentQueue/EMPTY)
	q (-> e (conj 2) (conj 4) (conj 8) (conj 12))]
  (fact "a queue is a functor"
	(fmap square q) => '(4 16 64 144))))


(deftest test-0150
  (let [e []
	c (->> e (cons 2) (cons 4) (cons 8) (cons 12))]
  (fact "a queue is a functor"
	(fmap square c) => '(144 64 16 4))))


(deftest test-0155
  (let [plus-five #(+ 5 %)]
    (fact "a queue is a functor"
	  ((fmap square plus-five) 7) => 144)))


(deftest test-0160
  (let [lst (map identity '(3 7 11 13 20))]
    (fact "a lazy-seq is a functor"
	  (fmap square lst) => '(9 49 121 169 400))))


(deftest test-0165
  (let [p1 (->Pair 9 12)]
    (fact "a pair is a functor"
	  (fmap square p1) => (->Pair 81 144))))


(deftest test-0170
  (let [p1 (->Pair 9 12)]
    (fact "(<$) substitutes a value in a functor"
	  (<$ 5005 p1) => (->Pair 5005 5005))))


;; +-------------------------------------------------------------+
;; |                           Monads.                           |
;; +-------------------------------------------------------------+

;; In test_monads.clj, Functions on monads.

;; +-------------------------------------------------------------+
;; |                     Curried Functions.                      |
;; +-------------------------------------------------------------+

(defnc f2 "docstring" [x y] (+ x y))
(defnc f3 "doc string" [x y z] (+ x y z))
(defnc f4 "f4 doc" [w x y z] (+ w x y z))
(defnc f5 "f5 doc" [v w x y z] (+ v w x y z))
(defnc f6 "f6 doc" [u v w x y z] (+ u v w x y z))
(defnc f7 "f7 doc" [t u v w x y z] (+ t u v w x y z))
(defnc k2 "k2 doc" [x y] (- x y))
(defnc k3 "k3 doc" [x y z] (* (- x y) z))

(deftest test-0500
  (fact "defnc defines a curried two-arg function"
	(f2 3 4) => 7
	(map (f2 3) (range 5)) => [3 4 5 6 7]))


(deftest test-0505
  (fact "defnc defines a curried three-arg function"
        (f3 1 2 3) => 6
        (map (f3 1 2) (range 5)) => [3 4 5 6 7])
  (fact "a curried function can be called as a total function"
        (map (f3 5) (range 5) (range 5)) => [5 7 9 11 13]
        (let [k2 (f3 5)]
	     (k2 10 20) => 35))
  (fact "a curried function can further return curried functions"
        (let [k2 (f3 5)]
          (map (k2 3) (range 5)) => [8 9 10 11 12])))


(deftest test-0510
  (fact "defnc defines a curried four-arg function"
	(f4 1 2 3 4) => 10
        (map (f4 1 2 3) (range 5)) => [6 7 8 9 10])
  (fact "a curried function can be called as a total function"
	(map (f4 5 3) (range 5) (range 5)) => [8 10 12 14 16])
  (fact "a curried function can further return curried functions"
        (let [k3 (f4 5)
	      k2 (k3 5)]
          (map (k2 5) (range 5))) => [15 16 17 18 19]))


(deftest test-0515
  (fact "defnc defines a curried five-arg function"
	(f5 1 2 3 4 5) => 15
        (map (f5 1 2 3 4) (range 5)) => [10 11 12 13 14])
  (fact "a curried function can be called as a total function"
	(map (f5 5 3 2) (range 5) (range 5)) => [10 12 14 16 18])
  (fact "a curried function can further return curried functions"
        (let [k3 (f5 5 5)
	      k2 (k3 5)]
          (map (k2 5) (range 5))) => [20 21 22 23 24]))


(deftest test-0520
  (fact "defnc defines a curried six-arg function"
	(f6 1 2 3 4 5 6) => 21
        (map (f6 1 2 3 4 5) (range 5)) => [15 16 17 18 19])
  (fact "a curried function can be called as a total function"
	(map (f6 5 3 2 4) (range 5) (range 5)) => [14 16 18 20 22])
  (fact "a curried function can further return curried functions"
        (let [k3 (f6 5 5 3)
	      k2 (k3 5)]
          (map (k2 5) (range 5))) => [23 24 25 26 27]))


(deftest test-0525
  (fact "defnc defines a curried seven-arg function"
	(f7 1 2 3 4 5 6 7) => 28
        (map (f7 1 2 3 4 5 6) (range 5)) => [21 22 23 24 25])
  (fact "a curried function can be called as a total function"
	(map (f7 5 3 2 4 1) (range 5) (range 5)) => [15 17 19 21 23])
  (fact "a curried function can further return curried functions"
        (let [k3 (f7 5 5 3 4)
	      k2 (k3 5)]
          (map (k2 5) (range 5))) => [27 28 29 30 31]))


(deftest test-0550
  (fact "(curry) makes a curried function off a regular one"
	(let [k (curry reduce 3)
	      f (k + 0)]
	  (f (range 10))) => 45
	(let [k (curry map 3)
	      f (k * (range 5))]
	  (f (range 5))) => [0 1 4 9 16]
	(let [k (curry take 2)]
	  (map (k 2) [[1 2 3] [0 1 2] [88 99 100]])) => '((1 2) (0 1) (88 99))))


(deftest test-0580
  (fact "flip swaps the first two args in a function"
	(let [f (fn [x y] (- x y))
	      g (flip f)]
	  (g 4 10) => 6))
  (fact "flip swaps the first two args in a curried function"
	(let [g (flip k2)]
	  (g 4 10) => 6)))


(deftest test-0585
  (fact "flip swaps the first two args in a function"
	(let [f (fn [x y z] (* (- x y) z))
	      g (flip f)]
	  (g 4 10 8) => 48))
  (fact "flip swaps the first two args in a curried function"
	(let [g (flip k3)]
	  (g 4 10 8) => 48)))
