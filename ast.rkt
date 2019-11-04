#lang racket
;; type Prog =
;; | `(begin ,Def ... ,Expr+)
;; | Expr+

;; type Def = `(define (,Variable . ,Formals) ,Expr+)

;; type Expr+ =
;; | Integer
;; | Boolean
;; | Character
;; | String
;; | Variable
;; | `'()
;; | `',Symbol
;; | `(,Prim ,Expr+ ...)
;; | `(if ,Expr+ ,Expr+ ,Expr+)
;; | `(cond ,@Clauses [else ,Expr])
;; | `(let ,Bindings+ ,Expr+)
;; | `(letrec ,Bindings+ ,Expr+)
;; | `(λ ,Formals ,Expr+)
;; | `(,Expr+ ,Expr+ ...)
;; | `(apply ,Expr+ ,Expr+)

;; type Clauses = (Listof `(,Expr+ ,Expr+))
;; type Bindings = (Listof `(,Variable ,Expr+))

;; type Expr =
;; | Integer
;; | Boolean
;; | Character
;; | String
;; | Variable
;; | `'()
;; | `',Symbol
;; | `(,Prim ,Expr ...)
;; | `(if ,Expr ,Expr ,Expr)
;; | `(let ,Bindings ,Expr)
;; | `(letrec ,Bindings ,Expr)
;; | `(λ ,Formals ,Expr)
;; | `(,Expr ,Expr ...)
;; | `(apply ,Expr ,Expr)

;; type Prim =
;; | 'add1 | 'sub1 | 'zero? | '+ | '- | 'abs
;; | 'char=? | 'boolean=? |
;; | '= | '< | '<=
;; | 'char? | 'integer? | 'boolean? | 'empty? | 'string? | 'box? | 'cons?
;; | 'string-length | 'string-ref | 'make-string
;; | 'integer->char | 'char->integer | 'gensym
;; | 'cons | 'car | 'cdr | 'box | 'unbox

;; type Bindings = (Listof `(,Variable ,Expr))

;; type Formals =
;; | '()
;; | Variable
;; | (Cons Variable Formals)

;; type Variable = Symbol (except 'let, 'add1, etc.)
