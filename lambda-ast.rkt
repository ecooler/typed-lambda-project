#lang racket
(require racket/match)

(provide (all-defined-out))

;;; defines the ast nodes of the arith language

; variables will have a name and an index, the name will help with
; debugging so we know which one was attached to it. Finally, for
; checking purposes, each variable with have a length to indicate
; the length of its context--this can then be checked with the index
(struct var-term (name index len) #:transparent)

; abstraction will use the name given, but this is irrelevant once
; we turn it into a de Bruijn representation
(struct abs-term (name type body) #:transparent)

; application has two terms
(struct app-term (term1 term2) #:transparent)

; true term
(struct true-term () #:transparent)

; false term
(struct false-term () #:transparent)

; if-term
(struct if-term (test then else) #:transparent)

; num-term
(struct num-term (val) #:transparent)

; predecessor
(struct pred-term (term) #:transparent)

; successor 
(struct succ-term (term) #:transparent)

; iszero test
(struct iszero-term (term) #:transparent) 

; returns true if the term is a val, false otherwise (only abstractions are values)
(define (isval? ctx t)
  (match t
    [(abs-term _ _ _) #t]
    [(true-term) #t]
    [(false-term) #t]
    [(num-term _) #t]
    [_ #f]))

