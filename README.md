tiny lisp in rust

## stuff supported

Basic arithmetic operations:
```lisp
(+ 1 1)
// => 2
```

comparisons and equality checks:
```lisp
(= 1 1)
// => true
(< 2 1)
// => true
```

if conditions:
```lisp
(if (= 1 1) 2 1)
// => 2
```

defining variables
```lisp
(def foo 1)
(+ foo foo)
// => 2
```

lambdas:
```lisp
(def inc (fn (x) (+ x 1)))
(inc 2)
// => 3
```

## stuff I could support
- syntactic sugar such as `defn` in clojure
