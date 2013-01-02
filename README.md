SchemeR: An implementation of a simple Scheme in R
==================================================

####Copyright (c) David Springate 2013

This project is inspired by/ported from/ripped off of Peter Norvig's [Lispy](http://norvig.com/lis.py) and [(How to Write a (Lisp) Interpreter (in Python))](http://norvig.com/lispy.html).

To run the REPL, start up R and:

```R
source(scheme.R)
schemeR()
```

To test outside of the REPL, try something like:

```R

procedure <- "(if (< (+ 123 (* 12 12)) 345) (quote yes) (quote no))"
parsed.procedure <- read(procedure)
parsed.procedure        # the procedure parsed into R lists
Eval(parsed.procedure)  # The result of the evaluation of the parsed procedure
```
Hit Control-C to get back to R.
In R, type `global.env` to see a list of built in functions.

Special forms are:
* `(quote expr)` 
* `(if test conseq alt)`
* `(set! var expr)`
* `(define var expr)`
* `(lambda (vars) expr)`
* `(begin exprs)`

#####Added features:
* The `Eval` function is wrapped in a `tryCatch` which means that every typo will not crash you out
  of the REPL.

#####Issues:

* The Lambda special form does not yet work correctly (at all!). Returns error:
    Error in nchar(val) : 
        cannot coerce type 'closure' to vector of type 'character'
* Quote is not completely sane
* More built-ins to be added

#####To do:
* Implement features from Peter Norvig's [more complex interpreter](http://norvig.com/lispy2.html)



