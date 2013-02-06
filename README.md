# Morph

Morph is a library of Haskell-style morphisms: monoids, functors, and monads.
These constructs are helpful for designing programs that are purely functional
and that encapsulate the boilerplate employed by many programming techniques.

## Features

* Implementation based on protocols and data types.
* Predefined monoids and functors; with support for Clojure collections.
* Monads: Identity, Maybe, Either, Reader, Writer, State.
* Monad Transformers: MaybeT, EitherT, ReaderT, WriterT, StateT.
* Support for curried functions.
* Library of generic functions for the above constructs.
* Sample monads in `src/main/resources`.

## Setup

Leiningen:

```clojure
[org.blancas/morph "0.1.0"]
```

Maven:

```xml
<dependency>
  <groupId>org.blancas</groupId>
  <artifactId>morph</artifactId>
  <version>0.1.0</version>
</dependency>
```

## Sample Usage

A simple expression evaluator uses the State monad; it can declare variables and clear the symbol table. Access to the symbol table is direct as if it were global, though all functions are pure.
```clojure
(use 'blancas.morph.core
     'blancas.morph.monads)

(def table {'DEG 57.295779 'E 2.718281 'PI 3.141592})

(declare run)

(defn calc
  "Evaluates and unboxes the operands; then applies the
   operator and returns the result as new state value."
  [op x y]
  (monad [a (run x) b (run y)]
    (state (op a b))))

(defn const
  "Returns a state value with a looked-up value or the given number."
  [x] (if (symbol? x) (gets x) (state x)))

(defn decl
  "Declares a variable as (var = val); value is used in-place."
  [x y] (>> (modify-state assoc x y) (state y)))

(defn clear
  "Clears the symbol table as (val %); value is used in-place."
  [x] (>> (put-state {}) (state x)))

(defn run
  "Dispatches on an expression or a single value."
  [op]
  (if (list? op)
    (case (second op)
      + (calc + (first op) (last op))
      - (calc - (first op) (last op))
      * (calc * (first op) (last op))
      / (calc / (first op) (last op))
      = (decl   (first op) (last op))
      % (clear  (first op)))
    (const op)))
```

Now we evaluate some expressions with `table` as the initial state.
```clojure
(eval-state (run '((9 / 3) + (2 * (PI - E)))) table)
;; 3.846622
(exec-state (run '((180 / (k = 30)) + (k * (PI - E)))) table)
;; {PI 3.141592, E 2.718281, DEG 57.295779, k 30}
(exec-state (run '(((180 %) / (k = 30)) + ((j = 5) * (k - j)))) table)
;; {j 5, k 30}
```

## Documentation

Morph is documented in the [Wiki](https://github.com/blancas/morph/wiki).

To generate the API docs (in the `codox` directory):

    lein doc

## License

Copyright © 2013 Armando Blancas.

Licensed under the [Eclipse Public License](http://www.eclipse.org/legal/epl-v10.html).
