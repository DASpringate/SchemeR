## SchemeR - an implementation of scheme in R
## Ported from Peter Norvig's Lispy. (http://norvig.com/lis.py)


Env <- function(outer = list()){
    list("outer" = outer)
}

locate <- function(env, var){
    # Find the innermost env where var appears
    if(!is.null(env[[var]])){
        env
    } else if(!is.null(env$outer)){
        locate(env$outer, var)
    } else stop("Symbol not found")
}

globals <- list("+" = function(x,y) x+y, "-" = function(x,y) x-y, "*" = function(x,y) x * y, 
                "/" = function(x,y) x/y, "not" = function(x) !x, ">" = function(x,y) x>y, 
                "<" = function(x,y) x<y, ">=" = function(x,y) x>=y, "<=" = function(x,y) x>y,
                "=" = function(x,y) x==y, "equal?" = function(x,y) x==y,
                "length" = length, "cons" = function(x,y) {x[[length(x)+1]] <- y; x},
                "car" = function(x) x[[1]], "cdr" = function(x) x[2:length(x)],
                "append" = append, "list?" = is.list, 
                "null?" = function(x) is.list(x) & length(x) != 0,
                "symbol?" = is.character, "sin" = sin, "tan" = tan, "cos" = cos,
                "asin" = asin, "sinh" = sinh, "pi" = pi, "sqrt" = sqrt)
    
add.globals <- function(env, global.fns = globals){
    for(i in 1:length(global.fns)){
        env[[names(global.fns)[i]]] <- global.fns[[i]]
    }
    env
}

global.env <- add.globals(Env())

##Eval function

Eval <- function(x, env = global.env){
   # if(!is.null(local.env)){
   #     env <- local.env
   # }
    #Evaluate an expression in an environment
    tryCatch({
        if(is.character(x)){                        # variable reference
            locate(env, x)[[x]]
        } else if(!is.list(x)){                     # constant literal
            return(x)
        } else if(x[[1]] == "quote"){               # (quote exp)
            return(x[[2]])
        } else if(x[[1]] == "if"){
            names(x) <- c("","test","conseq","altern")
            return(ifelse(Eval(x$test, env), Eval(x$conseq, env), Eval(x$altern, env)))
        } else if(x[[1]] == "set!"){                    # don't think this will work!
            names(x) <- c("", "var", "expr")
            if(is.list(locate(env, x$var))){
                new.env <- env
                new.env[[x$var]] = Eval(x$expr, new.env)
                assign(deparse(substitute(env)), new.env, envir = .GlobalEnv)
                return(new.env[[x$var]])
            }
        } else if(x[[1]] == "define"){
            names(x) <- c("", "var", "expr")
            new.env <- env
            new.env[[x$var]] = Eval(x$expr, env)
            assign(deparse(substitute(env)), new.env, envir = .GlobalEnv)
            return(new.env[[x$var]])
        } else if(x[[1]] == "lambda"){                  # (lambda (var*) expr)
            names(x) <- c("", "vars", "expr")
            return(lambda(x$vars, x$expr))
        } else if(x[[1]] == "begin"){
            for(expr in x[2:length(x)]){(
                val <- Eval(expr, env)
            }
            return(val)
        } else {
            exprs <- lapply(x, function(expr) Eval(expr, env))
            proc <- exprs[[1]]
            return(do.call(proc, exprs[2:length(exprs)]))
        }
    }, error = function(e) print(e$message))
}


lambda <- function(vars, expr){
    local.env <- Env(global.env)
    for(v in vars){
        local.env[[v]] <- v
    }
    fn <- function() pass
    formals(fn) <- eval(parse(text=paste("alist(",paste(vars, collapse = "=,"), "=)", sep="")))
    body(fn) <- quote(Eval(expr, local.env))
    return(fn)
    #return(Eval(expr, local.env))
}


lambda(x$vars, x$expr)

##parse, read and UI:

tokenise <- function(s){
    # convert a string to a list of tokens
    tokens <- strsplit(gsub("\\)", " ) ", gsub("\\(", " ( ", s)), split = "[[:space:]]")[[1]]
    tokens[nchar(tokens) >0]
}

atom <- function(token){
    # numbers are numeric, everything else is character string
    ifelse(suppressWarnings(!is.na(as.numeric(token))), as.numeric(token), as.character(token))
}

read <- function(s){
    # Read a Scheme expression from a string.
    read.from(tokenise(s))[[1]]
}
    
read.from <- function(tokens){
    # Read an expression from a sequence of tokens.
    if(is.list(tokens)){
        tokens <- tokens[2]
    }
    if(length(tokens) == 0){
        stop("unexpected EOF while reading")
    }
    token <- tokens[[1]]
    tokens <- tokens[2:length(tokens)]
    if(token == "("){
        L <- list()
        while(tokens[1] != ")"){
            toks <- read.from(tokens)
            L[[length(L)+1]] <- toks[[1]] 
            tokens <- toks[[2]]
        }
        tokens <- tokens[2:length(tokens)]
        return(list(L, tokens))
    } else if(token == ")"){
        stop("unexpected )")
    } else {
        return(list(atom(token), tokens))
    }
}

to.string <- function(expr){
    # Convert an R object back to a Lisp-readable string
    if(is.list(expr)){
        paste("(", paste(sapply(expr, to.string), collapse = " "), ")", sep = "")
    } else {
        as.character(expr)
    }
}

schemeR <- function(prompt = "schemeR>> "){
    # Read-eval-print-loop
    while(1){
        cat(prompt)
        val <- Eval(read(readLines(n = 1)), global.env)
        if(nchar(val)){
            cat(to.string(val), "\n")
        }
    }
}

a = Env()
a$
# program <- "(begin (define r 3) (* 3.141592653 (* r r)))"
# a = read(program)
# Eval(a, global.env)
# 
# a = read("(lambda (x y) (+ x y))")
# s.replace(   '(',  ' ( '   ).replace(')',' ) ').split()
