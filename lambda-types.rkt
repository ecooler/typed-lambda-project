#lang racket

(provide (all-defined-out))

(require racket/match
         "lambda-ast.rkt")

; generic struct representing all types
(struct type () #:transparent)

; function type, representing an arg -> ret type
(struct fun-type type (arg ret) #:transparent
  ; a guard function to catch errors in case you try to init it with something not a type
  #:guard (λ (arg ret ty)
            (cond
              [(not (type? arg)) (raise-argument-error ty "type? (first arg)" arg)]
              [(not (type? ret)) (raise-argument-error ty "type? (2nd arg)" ret)]
              [else (values arg ret)])))
; bool type
(struct bool-type type () #:transparent)

; int type
(struct int-type type () #:transparent)

; converts a symbol or something else to its appropriate type
(define (sym->type sym)
  (match sym
    ['Bool (bool-type)]
    ['Int (int-type)]
    [(? type?) sym]))


; converts a type into a string representation (for pretty printing)
(define (type->string ty)
  (match ty
    [(bool-type) "Bool"]
    [(int-type) "Int"]
    ; we add the parenthesis since they may be there in the structure, but we lost them
    ; in the parsing--and note that -> is right-associative, so this will point it out
    [(fun-type arg ret) (string-append "(" (type->string arg) "->" (type->string arg) ")")]))
    
