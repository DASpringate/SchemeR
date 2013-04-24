## SchemeR - an implementation of scheme in R
## Copyright (c) David Springate 2013

schemeR.version <- 1.02

Env <- function(outer = new.env()){
  # defines a new environment
  e <- new.env()
  e$outer <- outer
  e
}

locate <- function(env, var){
  # Find the innermost environment where a variable appears
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
  # Populate an environment with built-in functions
  for(i in 1:length(global.fns)){
    assign(x = names(global.fns)[i], value = global.fns[[i]], envir = env)
  }
}

##Eval function 
Eval <- function(x, env = global.env){
  #Evaluate an expression in an environment
  tryCatch({
    if(is.character(x)){                                # variable reference
      locate(env, x)[[x]]
    } else if(!is.list(x)){                             # constant literal
      return(x)
    } else if(!is.list(x[[1]])){
      if(x[[1]] == "quote"){                          # (quote exp)
        return(x[[2]])
      } else if(x[[1]] == "if"){
        names(x) <- c("","test","conseq","altern")
        return(ifelse(Eval(x$test, env), Eval(x$conseq, env), Eval(x$altern, env)))
      } else if(x[[1]] == "set!"){                    # Now works sanely!
        names(x) <- c("", "var", "expr")
        if(is.environment(locate(env, x$var))){
          assign(x = x$var, value = Eval(x$expr, new.env), envir = env )
          return(env[[x$var]])
        } 
      } else if(x[[1]] == "define"){ # (define var exp)
        names(x) <- c("", "var", "expr")
        assign(x = x$var, value = Eval(x$expr, new.env), envir = env )
        return(env[[x$var]])
      } else if(x[[1]] == "lambda"){                  # (lambda (var*) expr)
        names(x) <- c("", "vars", "expr")
        return(function(..., vars = x$vars, expr = x$expr){
          args <- list(...)
          local.env <- Env(outer = global.env)
          for(v in 1:length(vars)){
            assign(x = vars[[v]], value = args[[v]], envir = local.env)
          }
          Eval(expr, local.env)
        })
      } else if(x[[1]] == "begin"){                   # (begin exprs)
        for(expr in x[2:length(x)]){
          val <- Eval(expr, env)
        }
        return(val)
      } else {                                        # procedure evaluation
        exprs <- lapply(x, function(expr) Eval(expr, env))
        proc <- exprs[[1]]
        return(do.call(proc, exprs[2:length(exprs)]))
      }
    } else {                                # ugly hack to deal with R's vectorised if statements
      exprs <- lapply(x, function(expr) Eval(expr, env))
      proc <- exprs[[1]]
      return(do.call(proc, exprs[2:length(exprs)]))
    }
  }, error = function(e) print(e$message))
}

##parser, reader and UI:

tokenise <- function(s){
  # convert a string to a list of tokens
  tokens <- strsplit(gsub("\\)", " ) ", gsub("\\(", " ( ", s)), split = "[[:space:]]")[[1]]
  tokens[nchar(tokens) >0]
}

atom <- function(token){
  # numbers are numeric, everything else is character string
  ifelse(suppressWarnings(!is.na(as.numeric(token))), 
         as.numeric(token), as.character(token))
}

read <- function(s){
  # Read a lisp expression from a string.
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
  } else if (is.function(expr)){
    paste("Anonymous function: (", 
          paste(sapply(environment(expr)$x, to.string), collapse = " "),
          ")", sep = "") 
  } else {
    as.character(expr)
  }
}

schemeR <- function(prompt = "schemeR>> "){
  # Read-eval-print-loop
  cat("SchemeR version", schemeR.version,"\n")
  while(1){
    cat(prompt)
    val <- Eval(read(readLines(n = 1)), global.env)
    cat(to.string(val), "\n")
  }
}


global.env <- Env()
add.globals(global.env)
