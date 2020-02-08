#lang racket

(require "lambda-ast.rkt"
         "lambda-env.rkt"
         "lambda-types.rkt")

(provide (all-defined-out))


;;; defines the rules for typechecking an expression
(define (type-of t context)
  (match t
    ; values first
    [(true-term) (bool-type)]
    [(false-term) (bool-type)]
    [(num-term _) (int-type)]

    ; TODO: Fill out the types for conditionals, applications and abstractions

    ; TODO (Graduate required, Undergraduate optional): Fill out the types for numerics
    
    ; lookup variables in the current context
    [(var-term _ i _) (lookup-type i context)]

    [_ (raise-user-error 'type-of
                         "No matching term case for type-of: ~a" t)]))
    