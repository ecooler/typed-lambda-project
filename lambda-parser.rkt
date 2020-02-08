#lang racket

;;; a language for simple arithmetic expressions
(require (prefix-in lex: parser-tools/lex)
         parser-tools/yacc)

(require "lambda-lexer.rkt"
         "lambda-ast.rkt"
         "lambda-types.rkt")

(provide (all-defined-out))

(define parse-error (make-parameter #t))

(define lambda-parser
  (parser
   (src-pos)
   (start program)
   (end EOF)
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            ; indicate a parsing error occurred
            (parse-error #t)
            (if (and (eq? tok-ok? #t) (eq? tok-name 'EOF)) '()
                (begin
                  (printf "Parsing error at line ~a, col ~a: token: ~a, value: ~a, tok-ok? ~a\n"
                          (lex:position-line start-pos) (lex:position-col start-pos) tok-name tok-value tok-ok?)
                  (cond
                    [(equal? tok-value (token-PERIOD))
                     (printf "Found . but expected : to specify the type of variable")])))))
   (tokens lambda-empty-tokens lambda-tokens)
   (precs (left ID) (right RARROW))
   ;(debug "grammar-tables.txt")
   (grammar
    (program
     [(term) $1])     

    ; terms--these are the possible terms in the language
    (term     
     ; abstraction term
     [(LAMBDA ID COLON typename PERIOD term) (abs-term $2 $4 $6)]

     ; application term
     [(application) $1]

     ; numerics
     [(numerics) $1]

     ; if statements s
     [(IF term THEN term ELSE term) (prec ELSE) (if-term $2 $4 $6)])

    ; application 
     (application
     [(atomic-term) $1]
     [(application atomic-term) (app-term $1 $2)])
     
     ; successor/predecessor/iszero
     (numerics
      [(SUCC term) (succ-term $2)]
      [(PRED term) (pred-term $2)]
      [(ISZERO term) (iszero-term $2)])
     
    ; atomic terms are simply terms with the highest precedence
    ; so they're wrapped up immediately in an AST node
    (atomic-term
     [(LPAREN term RPAREN) $2]
     [(ID) (var-term $1 #f #f)]
     [(NUM) (num-term $1)]
     [(TRUE) (true-term)]
     [(FALSE) (false-term)])

    ; for handling type declarations
    (typename
     [(ID) (sym->type $1)]
     [(typename RARROW typename) (fun-type (sym->type $1) $3)]
     [(LPAREN typename RPAREN) $2])
    )))
    

(define (parse in)
  (parse-error #f)
  (port-count-lines! in)
  (lambda-parser (get-tokenizer in)))

(define (parse-str str)
  (let ([in (open-input-string str)])
    (parse in)))

(define (parse-file filename)
  (let ([in (open-input-file filename)])
    (parse in)))