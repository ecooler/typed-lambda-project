#lang racket

(require "lambda-ast.rkt")
         

(provide (all-defined-out))
(define (print-nameless t)
  (define out (open-output-string))
  (define (print-nameless-helper t)
    (match t
      [(var-term name index len) (fprintf out "~a" index)]
      [(abs-term name ty t1)
       (fprintf out "(λ.")
       (print-nameless-helper t1)
       (fprintf out ")")]
      [(app-term t1 t2)
       (fprintf out "(")
       (print-nameless-helper t1)
       (fprintf out " ")
       (print-nameless-helper t2)
       (fprintf out ")")]
      [(true-term)
       (fprintf out "true")]
      [(false-term)
       (fprintf out "false")]
      [(num-term n)
       (fprintf out "~a" n)]
      [(succ-term t1)
       (fprintf out "succ(")
       (print-nameless-helper t1)
       (fprintf out ")")]
      [(pred-term t1)
       (fprintf out "pred(")
       (print-nameless-helper t1)
       (fprintf out ")")]
      [(iszero-term t1)
       (fprintf out "iszero(")
       (print-nameless-helper t1)
       (fprintf out ")")]
      [(if-term t1 t2 t3)
       (fprintf out "if ")
       (print-nameless-helper t1)
       (fprintf out " then ")
       (print-nameless-helper t2)
       (fprintf out " else ")
       (print-nameless-helper t3)]))
  (print-nameless-helper t)
  (println (get-output-string out)))

(define (print-names t)
  (define out (open-output-string))
  (define (print-nameless-helper t)
    (match t
      [(var-term name index len) (fprintf out "~a" name)]
      [(abs-term name ty t1)
       (fprintf out "(λ~a. " name)
       (print-nameless-helper t1)
       (fprintf out ")")]
      [(app-term t1 t2)
       #;(fprintf out "(")
       (print-nameless-helper t1)
       (fprintf out " ")
       (print-nameless-helper t2)
       #;(fprintf out ")")]
      [(true-term)
       (fprintf out "true")]
      [(false-term)
       (fprintf out "false")]
      [(num-term n)
       (fprintf out "~a" n)]
      [(succ-term t1)
       (fprintf out "succ(")
       (print-nameless-helper t1)
       (fprintf out ")")]
      [(pred-term t1)
       (fprintf out "pred(")
       (print-nameless-helper t1)
       (fprintf out ")")]
      [(iszero-term t1)
       (fprintf out "iszero(")
       (print-nameless-helper t1)
       (fprintf out ")")]
      [(if-term t1 t2 t3)
       (fprintf out "if ")
       (print-nameless-helper t1)
       (fprintf out " then ")
       (print-nameless-helper t2)
       (fprintf out " else ")
       (print-nameless-helper t3)]))
  (print-nameless-helper t)
  (println (get-output-string out)))