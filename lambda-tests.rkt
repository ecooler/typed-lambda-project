#lang racket


(require rackunit
         "lambda-eval.rkt"
         "lambda-ast.rkt"
         "lambda-lexer.rkt"
         "lambda-parser.rkt"
         "lambda-env.rkt"
         "lambda-types.rkt"
         "lambda-typechecker.rkt")


; tests on the lexer to make sure the right tokens are produced
(define lexer-tests
  (test-suite
   "Lexer tests for lambda"
   (check-equal? (lexstr "x") (list (token-ID 'x)))
   (check-equal? (lexstr "foo") (list (token-ID 'foo)))
   (check-equal? (lexstr "FOO") (list (token-ID 'FOO)))
   (check-equal? (lexstr "x  y") (list (token-ID 'x) (token-ID 'y)))
   (check-equal? (lexstr "λ") (list (token-LAMBDA)))
   (check-equal? (lexstr ".") (list (token-PERIOD)))
   (check-equal? (lexstr "(") (list (token-LPAREN)))
   (check-equal? (lexstr ")") (list (token-RPAREN)))
   (check-equal? (lexstr "λx.x") (list (token-LAMBDA) (token-ID 'x)
                                       (token-PERIOD) (token-ID 'x)))
   (check-equal? (lexstr ":") (list (token-COLON)))
   (check-equal? (lexstr "true") (list (token-TRUE)))
   (check-equal? (lexstr "false") (list (token-FALSE)))
   (check-equal? (lexstr "if") (list (token-IF)))
   (check-equal? (lexstr "then") (list (token-THEN)))
   (check-equal? (lexstr "else") (list (token-ELSE)))
   ))

; tests to make sure the parsing is producing the right ASTs
(define parser-tests
  (test-suite
   "Parser tests for lamda"
   (check-equal? (parse-str "x") (var-term 'x #f #f))
   (check-equal? (parse-str "true") (true-term))
   (check-equal? (parse-str "false") (false-term))
   (check-equal? (parse-str "if true then true else false") (if-term (true-term) (true-term) (false-term)))
   (check-equal? (parse-str "λx:Bool.x") (abs-term 'x (bool-type) (var-term 'x #f #f)) "Abstraction with bool type")
   (check-equal? (parse-str "if true then if true then true else true else false")
                 (if-term (true-term) (if-term (true-term) (true-term) (true-term)) (false-term)))
   (check-equal? (parse-str "x y") (app-term (var-term 'x #f #f) (var-term 'y #f #f)))
   (check-equal? (parse-str "(λx:Bool.t )true") (app-term (abs-term 'x (bool-type) (var-term 't #f #f)) (true-term)))
   ; tests precedence
   (check-equal? (parse-str "x y z")
                 (app-term
                  (app-term (var-term 'x #f #f) (var-term 'y #f #f))
                  (var-term 'z #f #f)))
   (check-equal? (parse-str "x (y z)")
                 (app-term (var-term 'x #f #f)
                           (app-term (var-term 'y #f #f)
                                     (var-term 'z #f #f))))
   (check-equal? (parse-str "λx:Bool.y z foo")
                 (abs-term
                  'x
                  (bool-type)
                  (app-term
                   (app-term (var-term 'y #f #f) (var-term 'z #f #f))
                   (var-term 'foo #f #f))))
   ))

(define context-tests
  (test-suite
   "Tests for the context for lambda"

   (test-case
    "Setting up context and testing with it"
    (define ctx (bind
                 'z
                 (var-binding (bool-type))
                 (bind
                  'y
                  (var-binding (bool-type))
                  (bind 'x (var-binding (bool-type)) (new-context)))))
    (check-equal? (new-context) '())
    (check-equal? (context-length (new-context)) 0)
    (check-equal? (bind 'w (var-binding (bool-type)) (new-context))
                  (list `(w . ,(var-binding (bool-type))))
                  "binding a variable to a context")
    (check-equal? (context-length (bind 'x (var-binding (bool-type)) (new-context))) 1)
    (check-equal? (name-bound? 'w ctx) #f "name not in context")
    (check-equal? (name-bound? 'x ctx) #t "name in context")
    (check-equal? (name-bound? 'y ctx) #t "name in context")
    (check-equal? (name-bound? 'z ctx) #t "name in context")
    (check-equal? (bind-fresh-name 'x (var-binding (bool-type)) ctx)
                  (bind (string->symbol "x'") (var-binding (bool-type)) ctx) "binding a fresh name")
    (check-equal? (name->index 'y ctx) 1 "name to index")
    (check-equal? (index->name 2 ctx) 'x "index to name")
    (check-equal? (lookup 'x ctx) (var-binding (bool-type)) "valid lookup (should exist in context)")
    )

   (test-case
    "shifting tests"
    (define ctx (bind 'z (var-binding (bool-type)) (new-context)))
    (check-equal? (shift-term (remove-names (parse-str "λx:Bool.λy:Bool.x(y z)") ctx) 2)
                  (abs-term
                   'x (bool-type)
                   (abs-term
                    'y (bool-type)
                    (app-term (var-term 'x 1 #f)
                              (app-term (var-term 'y 0 #f) (var-term 'z 4 #f))))))
    (check-equal? (shift-term (remove-names (parse-str "λx:Bool.x y(λw:Bool.w x y)")
                                            (bind 'y (var-binding (bool-type)) ctx)) 2)
                  (abs-term
                   'x (bool-type)
                   (app-term
                    (app-term (var-term 'x 0 #f) (var-term 'y 3 #f))
                    (abs-term
                     'w (bool-type)
                     (app-term
                      (app-term (var-term 'w 0 #f) (var-term 'x 1 #f))
                      (var-term 'y 4 #f)))))))
    

   (test-case
    "remove-names tests"
    ; c0
    (check-equal? (remove-names (parse-str "λs:Bool->Bool.λz:Bool.z") (new-context))
                  (abs-term 's (fun-type (bool-type) (bool-type)) (abs-term 'z (bool-type) (var-term 'z 0 #f))))
    ; c2
    (check-equal? (remove-names (parse-str "λs:Bool->Bool.λz:Bool.s(s z)") (new-context))
                  (abs-term
                   's (fun-type (bool-type) (bool-type))
                   (abs-term
                    'z (bool-type)
                    (app-term (var-term 's 1 #f)
                              (app-term (var-term 's 1 #f) (var-term 'z 0 #f))))))

    ; addition
    ; the types aren't right here, just testing remove names
    (check-equal? (remove-names (parse-str "λm:Bool.λn:Bool.λs:Bool.λz:Bool. m s (n s z)") (new-context))
                  (abs-term
                   'm (bool-type)
                   (abs-term
                    'n (bool-type)
                    (abs-term
                     's (bool-type)
                     (abs-term
                      'z (bool-type)
                      (app-term
                       (app-term (var-term 'm 3 #f) (var-term 's 1 #f))
                       (app-term (app-term (var-term 'n 2 #f)
                                           (var-term 's 1 #f))
                                 (var-term 'z 0 #f))))))))

    ; fix point operator
    (check-equal? (remove-names (parse-str "λf:Bool.(λx:Bool.f(λy:Bool.x x y))(λx:Bool.f(λy:Bool.x x y))") (new-context))
                  (abs-term
                   'f (bool-type)
                   (app-term
                    (abs-term
                     'x (bool-type)
                     (app-term
                      (var-term 'f 1 #f)
                      (abs-term
                       'y (bool-type)
                       (app-term (app-term (var-term 'x 1 #f)
                                           (var-term 'x 1 #f))
                                 (var-term 'y 0 #f)))))
                    (abs-term
                     'x (bool-type)
                     (app-term
                      (var-term 'f 1 #f)
                      (abs-term
                       'y (bool-type)
                       (app-term (app-term (var-term 'x 1 #f)
                                           (var-term 'x 1 #f))
                                 (var-term 'y 0 #f))))))))
                  
    )
   ))

; tests on substitution
(define substitution-tests
  (test-suite
   "Substitution tests"
   ; note, we use substitute-top, because generally that's how we do the
   ; substitution--we're doing it because we're applying one thing to another
   ; and therefore have to do some shifting--but this will test substitute just fine.
   ; Also, you'll when we use substitute-top, we're usually substituting into the
   ; *body* of a function, not the function itself (the indices will be wrong then)
   (check-equal? (substitute-top (remove-names (parse-str "λx:Bool.x") (new-context))
                                 (var-term 'y 0 #f))
                 (remove-names (parse-str "λx:Bool.x") (new-context)))

   ; substitution should only affect those vars with the same index (and sub-top affects 0)
   (check-equal? (substitute-top (remove-names (parse-str "λx:Bool.x") (new-context))
                                 (app-term (var-term 'y 0 #f) (var-term 'z 1 #f)))
                 (app-term (abs-term 'x (bool-type) (var-term 'x 0 #f)) (var-term 'z 0 #f)))

   ; substituting in a different (non-zero) index
   (check-equal? (substitute 1 (remove-names (parse-str "λx:Bool.x") (new-context))
                                 (app-term (var-term 'y 0 #f) (var-term 'z 1 #f)))
                 (app-term (var-term 'y 0 #f) (abs-term 'x (bool-type) (var-term 'x 0 #f))))
   ))

; tests on evaluation, your changes should pass all these tests
(define eval-tests
  (test-suite
   "Eval tests for lamda"
   ; test with simple identity
   (check-equal? (eval-str "λx:Bool.x" (new-context) #f #f) (abs-term 'x (bool-type) (var-term 'x 0 #f)))
   ; now pass the identity to an identity
   (check-equal? (eval-str "(λx:Bool->Bool.x)(λy:Bool.y)" (new-context) #f #f)
                 (abs-term 'y (bool-type) (var-term 'y 0 #f)))
   ; true in lambda calc
   (check-equal? (eval-str "(λt:Bool->Bool.λf:Bool->Bool.t)(λx:Bool.x)(λy:Bool.y)" (new-context) #f #f)
                 (abs-term 'x (bool-type) (var-term 'x 0 #f)))

   ; test true in lambda calc
   (check-equal? (eval-str "(λl:(Bool->Bool)->(Bool->Bool)->(Bool->Bool).λm:Bool->Bool.λn:Bool->Bool.l m n)(λt:Bool->Bool.λf:Bool->Bool.t)(λp:Bool.p)(λq:Bool.q)" (new-context) #f #f)
                 (abs-term 'p (bool-type) (var-term 'p 0 #f)))
   ; test false in lambda calc
   (check-equal? (eval-str "(λl:(Bool->Bool)->(Bool->Bool)->(Bool->Bool).λm:Bool->Bool.λn:Bool->Bool.l m n)(λt:Bool->Bool.λf:Bool->Bool.f)(λp:Bool.p)(λq:Bool.q)" (new-context) #f #f)
                 (abs-term 'q (bool-type) (var-term 'q 0 #f)))

   ; testing new constructs
   (check-equal? (eval-str "true" (new-context) #f #f) (true-term))
   (check-equal? (eval-str "false" (new-context) #f #f) (false-term))
   (check-equal? (eval-str "if true then true else false" (new-context) #f #f) (true-term))
   (check-equal? (eval-str "if false then true else false" (new-context) #f #f) (false-term))
   (check-equal? (eval-str "if if true then true else false then true else false" (new-context) #f #f)
                 (true-term))

   (check-equal? (eval-str "0" (new-context) #f #f) (num-term 0))
   (check-equal? (eval-str "succ 0" (new-context) #f #f) (num-term 1))
   (check-equal? (eval-str "pred 1" (new-context) #f #f) (num-term 0))
   (check-equal? (eval-str "pred succ 1" (new-context) #f #f) (num-term 1))
   (check-equal? (eval-str "(λx:Int.succ x) 5" (new-context) #f #f) (num-term 6))
   (check-equal? (eval-str "(λx:Int. succ x) 5" (new-context) #f #f) (num-term 6))
   (check-equal? (eval-str "(λx:Bool->Int.x true)(λy:Bool.if y then 5 else 6)" (new-context) #f #f)
                 (num-term 5))
   (check-equal? (eval-str "(λx:Bool->Int.x false)(λy:Bool.if y then 5 else 6)" (new-context) #f #f)
                 (num-term 6))
   ))

(define eval-big-tests
  (test-suite
   "Eval tests for lamda"
   ; test with simple identity
   (check-equal? (eval-str "λx:Bool.x" (new-context) #f #t) (abs-term 'x (bool-type) (var-term 'x 0 #f)))
   ; now pass the identity to an identity
   (check-equal? (eval-str "(λx:Bool->Bool.x)(λy:Bool.y)" (new-context) #f #t)
                 (abs-term 'y (bool-type) (var-term 'y 0 #f)))
   ; true in lambda calc
   (check-equal? (eval-str "(λt:Bool->Bool.λf:Bool->Bool.t)(λx:Bool.x)(λy:Bool.y)" (new-context) #f #t)
                 (abs-term 'x (bool-type) (var-term 'x 0 #f)))

   ; test true in lambda calc
   (check-equal? (eval-str "(λl:(Bool->Bool)->(Bool->Bool)->(Bool->Bool).λm:Bool->Bool.λn:Bool->Bool.l m n)(λt:Bool->Bool.λf:Bool->Bool.t)(λp:Bool.p)(λq:Bool.q)" (new-context) #f #f)
                 (abs-term 'p (bool-type) (var-term 'p 0 #t)))
   ; test false in lambda calc
   (check-equal? (eval-str "(λl:(Bool->Bool)->(Bool->Bool)->(Bool->Bool).λm:Bool->Bool.λn:Bool->Bool.l m n)(λt:Bool->Bool.λf:Bool->Bool.f)(λp:Bool.p)(λq:Bool.q)" (new-context) #f #f)
                 (abs-term 'q (bool-type) (var-term 'q 0 #t)))

   ; testing new constructs
   (check-equal? (eval-str "true" (new-context) #f #t) (true-term))
   (check-equal? (eval-str "false" (new-context) #f #t) (false-term))
   (check-equal? (eval-str "if true then true else false" (new-context) #f #t) (true-term))
   (check-equal? (eval-str "if false then true else false" (new-context) #f #t) (false-term))
   (check-equal? (eval-str "if if true then true else false then true else false" (new-context) #f #t)
                 (true-term))

   (check-equal? (eval-str "0" (new-context) #f #t) (num-term 0))
   (check-equal? (eval-str "succ 0" (new-context) #f #t) (num-term 1))
   (check-equal? (eval-str "pred 1" (new-context) #f #t) (num-term 0))
   (check-equal? (eval-str "pred succ 1" (new-context) #f #t) (num-term 1))
   (check-equal? (eval-str "(λx:Int.succ x) 5" (new-context) #f #t) (num-term 6))
   (check-equal? (eval-str "(λx:Int. succ x) 5" (new-context) #f #t) (num-term 6))
   (check-equal? (eval-str "(λx:Bool->Int.x true)(λy:Bool.if y then 5 else 6)" (new-context) #f #t)
                 (num-term 5))
   (check-equal? (eval-str "(λx:Bool->Int.x false)(λy:Bool.if y then 5 else 6)" (new-context) #f #t)
                 (num-term 6))
   ))

(define ctx (new-context))
(define typeof-tests
  (test-suite
   "Tests for typechecking"
    (check-equal? (type-of (remove-names (parse-str "0") ctx) ctx) (int-type))
    (check-equal? (type-of (remove-names (parse-str "true") ctx) ctx) (bool-type))
    (check-equal? (type-of (remove-names (parse-str "false") ctx) ctx) (bool-type))
    (check-equal? (type-of (remove-names (parse-str "iszero 0") ctx) ctx) (bool-type))
    (check-equal? (type-of (remove-names (parse-str "iszero succ 1") ctx) ctx) (bool-type))
    (check-equal? (type-of (remove-names (parse-str "iszero if true then 0 else 1") ctx) ctx)
                  (bool-type))
    (check-equal? (type-of (remove-names (parse-str "succ 0") ctx) ctx) (int-type))
    (check-equal? (type-of (remove-names (parse-str "succ succ 0") ctx) ctx) (int-type))    
    (check-equal? (type-of (remove-names (parse-str "pred 0") ctx) ctx) (int-type))
    (check-equal? (type-of (remove-names (parse-str "pred pred 2") ctx) ctx) (int-type))
    (check-equal? (type-of (remove-names (parse-str "if true then 0 else 1") ctx) ctx) (int-type))
    (check-equal? (type-of (remove-names (parse-str "if true then λx:Bool.5 else λy:Bool.6") ctx) ctx)
                  (fun-type (bool-type) (int-type)))
    (check-equal? (type-of (remove-names (parse-str "λx:Bool.if x then λy:Bool.y else λz:Bool.z") ctx) ctx)
                  (fun-type (bool-type) (fun-type (bool-type) (bool-type))))
    (check-equal? (type-of (remove-names (parse-str "(λx:Int.5) (succ 6)") ctx) ctx)
                  (int-type))

    ))

; now execute the tests
(require rackunit/text-ui)
(printf "Executing lexer tests...~n")
(void (run-tests lexer-tests))
(printf "Executing parser tests...~n")
(void (run-tests parser-tests))
(printf "Executing context tests...~n")
(void (run-tests context-tests))
(printf "Executing substitution tests...~n")
(void (run-tests substitution-tests))
(printf "Executing typechecking tests...~n")
(void (run-tests typeof-tests))
(printf "Executing eval tests...~n")
(void (run-tests eval-tests))
(printf "Executing big-step eval tests...~n")
(void (run-tests eval-big-tests))
   
   
   