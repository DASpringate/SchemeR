SchemeR: An implementation of a simple Scheme in R
==================================================

####Copyright (c) David Springate 2013 
#####[Creative Commons Attribution 3.0 Unported License (CC BY 3.0)](http://creativecommons.org/licenses/by/3.0/)

This project is inspired by/ported from/ripped off of Peter Norvig's [Lispy](http://norvig.com/lis.py) and [(How to Write a (Lisp) Interpreter (in Python))](http://norvig.com/lispy.html) with further help from [SICP](http://mitpress.mit.edu/sicp/full-text/book/book.html).

To run the REPL, start up R and:

```R
source(scheme.R)
schemeR()
```

...Then you can start scheming...  e.g.

```scheme
(define cube (lambda (x) (* x (* x x))))
(cube 12)
```

Hit Control-C to get back to R.

To test outside of the REPL, try something like:

```R
procedure <- "(if (< (+ 123 (* 12 12)) 345) (quote yes) (quote no))"
parsed.procedure <- read(procedure)
parsed.procedure        # the procedure parsed into R lists
Eval(parsed.procedure)  # The result of the evaluation of the parsed procedure
global.env              # List of all built-in functions
```

Special forms are:
* `(quote expr)` 
* `(if test conseq alt)`
* `(set! var expr)`
* `(define var expr)`
* `(lambda (vars) expr)`
* `(begin exprs)`

#####Added features:
* The `Eval` function is wrapped in a `tryCatch` so typos will not crash you out
  of the REPL.
* `lambda` function is now fixed and working correctly
* String representations of anonymous functions are now printed on `define`  

#####Issues/bugs:

* Quote is not completely sane

#####To do:
* More built-ins to be added
* Implement features from Peter Norvig's [more complex interpreter](http://norvig.com/lispy2.html)
    - Arbitrary length arguments
    - different data types
    - Macros
    - Reader macros




