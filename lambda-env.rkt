#lang racket
; author: Chris GauthierDickey
; Advanced PL

(require "lambda-types.rkt")

; this module defines operations on the context that's being
; passed around with names for the nameless terms in lambda calculus
(provide (all-defined-out))

; creates a new context
(define (new-context) null)

; returns the length of the context
(define (context-length ctx)
  (length ctx))

; helper function to extract pairs and compare them from the context
(define (match-pair a b)
  (let ([name (if (pair? a) b a)]
        [pair (if (pair? a) a b)])
    (match pair
      [(cons n bind) (eq? n name)]
      [_ #f])))

; name-bound? returns true if name is already in the context, false otherwise
(define (name-bound? name context)
  (if (member name context match-pair) #t #f))

; looks up a name in the context
(define (lookup name context)
  (let ([res (member name context match-pair)])
    (cond
      [res (match (first res)
             [(cons _ binding) binding]
             [_ (raise 'UnmatchedBinding)])]
      [else (raise 'NameNotFoundInContext)])))


; adds a binding to the context, should be a symbol
(define (bind name ty ctx)
  (cond
    [(not (symbol? name)) (raise-argument-error 'bind "symbol?" name)]
    [(not (binding? ty)) (raise-argument-error 'bind "binding?" ty)]
    [else
     ; add a pair, of the name and type, to the context
     (cons (cons name ty) ctx)]))


; bind-name takes a name and a context (the list of names) and
; adds this name as the new one to it. If it's not a fresh name,
; we'll keep adding ' marks to it till it is
; name * list -> list
(define (bind-fresh-name name ty ctx)
  (if (name-bound? name ctx)
      (bind-fresh-name (string->symbol (string-append (symbol->string name) "'")) ty ctx)
      (bind name ty ctx)))

; index->name takes a context and an index and then returns that
; element from the context
; list * num -> string
(define (index->name index ctx)
  (car (list-ref ctx index)))

; name->index takes a context and a name, and then returns the index
; of that element, or #f otherwise
(define (name->index name ctx)
  (index-of ctx name match-pair))

; returns the binding of a particular variable location in the context
(define (get-binding index ctx)
  (let ([binding (list-ref ctx index)])
    (cond
      [binding (cdr binding)]
      [else (raise 'IndexNotValid)])))

; gets a type out of the current context (note, we prefer this because we've
; removed all the names, so we need to access the type by index
(define (lookup-type index context)
  (match (get-binding index context)
    [(var-binding ty) ty]
    [_ (raise 'NotAVariableBinding)]))

; structs to handle bindings, which all inherit from a generic binding type
(struct binding () #:transparent)
; a name binding just says, oh, it's a name
(struct name-binding binding () #:transparent)
; a var binding has a type associated with it (we assume these come from lambda-types.rkt)
(struct var-binding binding (type) #:transparent
  #:guard (λ (type ty)
            (cond
              [(type? type) (values type)]
              [else (raise-argument-error ty "type?" type)])))