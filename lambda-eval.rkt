#lang racket

(require "lambda-ast.rkt"
         "lambda-env.rkt"
         "lambda-parser.rkt"
         "lambda-util.rkt"
         "lambda-typechecker.rkt")

(provide (all-defined-out))


; replaces the names with de Bruijn indices (though in truth,
; we just set the index names on our terms, this results in a new AST)
(define (remove-names t context)
  (match t
    ; true and false terms
    [(true-term) t]
    [(false-term) t]
    
    ; conditionals
    [(if-term test then else)
     (if-term (remove-names test context) (remove-names then context) (remove-names else context))]

    ; numerics
    [(num-term _) t]
    [(succ-term t1)
     (succ-term (remove-names t1 context))]
    [(pred-term t1)
     (pred-term (remove-names t1 context))]
    [(iszero-term t1)
     (iszero-term (remove-names t1 context))]
    
    ; var terms
    [(var-term name index len)
     ; see if it's in the context
     (let ([maybe-idx (name->index name context)])
       (cond
         [maybe-idx
          (var-term name maybe-idx len)]
         [else (error "can't remove the name of a free variable")]))]
    
    ; abstraction
    [(abs-term name ty t1)
     (let ([newctx (bind name (var-binding ty) context)])
       (abs-term name ty (remove-names t1 newctx)))]

    ; application
    [(app-term t1 t2)
     (app-term
      (remove-names t1 context)
      (remove-names t2 context))]))


; shifts the term t by the given distance using a cutoff so we don't go below that
; note that shifting is required for substitution, which follows
; term * int * int -> term (that is shifted)
(define (shift-term t distance [cutoff 0])
  (match t
    ; vars
    [(var-term name k len)
     (cond
       ; if our de Bruijn index is less than the cuttoff, do nothing
       [(< k cutoff) t]
       ; otherwise, shift by the distance
       [else (var-term name (+ k distance) len)])]

    ; shift abstraction by shifting its body, but increment the cutoff
    [(abs-term name ty t1)
     (abs-term name ty (shift-term t1 distance (add1 cutoff)))]

    ; shift application by shifting each of its terms
    [(app-term t1 t2)
     (app-term (shift-term t1 distance cutoff)
               (shift-term t2 distance cutoff))]

    ; shifting mostly ignores the constants
    [(true-term) t]
    [(false-term) t]
    [(num-term _) t]

    ; shifting is applied recursively on subterms
    [(iszero-term t1)
     (iszero-term (shift-term t1 distance cutoff))]
    
    ; shift succ
    [(succ-term t1)
     (succ-term (shift-term t1 distance cutoff))]

    ; shift pred
    [(pred-term t1)
     (pred-term (shift-term t1 distance cutoff))]

    ; shift if
    [(if-term t1 t2 t3)
     (if-term (shift-term t1 distance cutoff)
              (shift-term t2 distance cutoff)
              (shift-term t3 distance cutoff))]
    ))


; substitute [idx -> t1]t, substitutes occurrences of idx with t1 in t.
; note that t1 *must* be a term, or things go badly, but idx is
; definitely a number
(define (substitute idx t1 t)
  (match t
    ; constants, so substitute is mostly ignored
    [(true-term) t]
    [(false-term) t]
    [(num-term _) t]

    ; recursively apply substitute on subterm
    [(succ-term t1prime)
     (succ-term (substitute idx t1 t1prime))]

    ; pred substitute
    [(pred-term t1prime)
     (pred-term (substitute idx t1 t1prime))]

    ; iszero substitute
    [(iszero-term t1prime)
     (iszero-term (substitute idx t1 t1prime))]

    ; if substitute
    [(if-term it1 it2 it3)
     (if-term (substitute idx t1 it1)
              (substitute idx t1 it2)
              (substitute idx t1 it3))]
    ; vars
    [(var-term name k len)
     ; if the name is equal to k, the index, then we substitute t1 into here!
     (if (= idx k)
         t1
         t)]

    [(abs-term name ty body)
     ; build a new term where we substitute on the body, but we shift the term by 1
     ; that we're substituting 
     (abs-term name ty (substitute (add1 idx) (shift-term t1 1) body))]

    [(app-term at1 at2)
     (app-term (substitute idx t1 at1) (substitute idx t1 at2))]))

; does an actual substitution of s in t with de Bruijn index 0
(define (substitute-top s t)
  (shift-term (substitute 0 (shift-term s 1) t) -1))


; one step evaluation--this just advances the term by one step
; term * context -> term
(define (eval-1-step t ctx)
  (match t

    ; if terms
    [(if-term (true-term) t2 t3) t2]    ; (E-IfTrue)
    [(if-term (false-term) t2 t3) t3]   ; (E-IfFalse)
    [(if-term t1 t2 t3)
     (if-term (eval-1-step t1 ctx) t2 t3)]  ; (E-If)

    ; iszero terms
    [(iszero-term (num-term (? number? z))) ; (E-IsZeroZero)
     (if (zero? z) (true-term) (false-term))]

    ; iszero generically on a term
    [(iszero-term t1)                       ; (E-IsZero)
     (iszero-term (eval-1-step t1 ctx))]

    ; succ term on a value
    [(succ-term (num-term n))
     (num-term (add1 n))]

    ; succ on a term
    [(succ-term t1)
     (succ-term (eval-1-step t1 ctx))]

    ; pred terms, where pred is operating on a value
    [(pred-term (num-term n))
     (num-term (if (zero? n) 0 (sub1 n)))]

    ; and where pred is operating on a term
    [(pred-term t1)
     (pred-term (eval-1-step t1 ctx))]
    
    ; both sides are values, so we can substitue 
    [(app-term (abs-term _ ty1 t12) (? (λ (v) (isval? ctx v)) t22))   ; (E-AppAbs)
     (substitute-top t22 t12)]
    
    ; match application, where the left term is an abstraction
    [(app-term (abs-term name ty t12) t2)  ; (E-App2)
     ; in this case, we're just going to continue evaluating the right side
     (app-term (abs-term name ty t12) (eval-1-step t2 ctx))]
    
    ; or match any generic application, so we must evaluate the left side
    [(app-term t1 t2)                    ; (E-App1)
     (let ([t1prime (eval-1-step t1 ctx)])
       (app-term t1prime t2))]
    [_ (raise 'NoRuleApplies)]))


; big-step evaluation, follows 5.3.8 on p73 of the textbook
(define (big-step-eval t ctx)
  (match t
    [(abs-term name _ t1) t]
    [(app-term (abs-term name ty t12) t2)
     (let ([v2 (big-step-eval t2 ctx)])
       (big-step-eval (substitute-top v2 t12) ctx))]
    [(app-term t1 t2)
     (let ([v1 (big-step-eval t1 ctx)])
       (match v1
         [(abs-term name ty t12)
          (let ([v2 (big-step-eval t2 ctx)])
            (big-step-eval (substitute-top v2 t12) ctx))]
         [_ (raise 'T1NotAbs)]))]))
     

; evaluate until we can no longer evaluate
(define (eval t [ctx (new-context)])
  ; we just return the term if we get an exception
  (with-handlers ([(λ (exn) (eq? exn 'NoRuleApplies)) (λ (exn) t)])
      ; evaluate one step
    (let ([t1 (eval-1-step t ctx)])
      (eval t1 ctx))))

; simple eval-in, just pass it to parse-in, which gives us the ast
(define (eval-in in [ctx (new-context)] [nameless? #t] [big-step? #f])  
  (let* ([ast (remove-names (parse in) ctx)]
         [typechecked? (type-of ast ctx)])
    (cond
      [typechecked? 
       (let ([res (if big-step?
                      (big-step-eval ast ctx)
                      (eval ast ctx))])
         (if nameless?
             (print-nameless res)
             res))]
      [else "Typechecking failed."])))

; parse and evaluate a string in the arith language
(define (eval-str str [ctx (new-context)] [nameless? #t] [big-step? #f])
  (let ([in (open-input-string str)])
    (eval-in in ctx nameless? big-step?)))