#lang racket
;;; a language for simple arithmetic expressions

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

(provide (all-defined-out))

; the language is so simple, we can just use tokens for all symbols
(define-empty-tokens lambda-empty-tokens
  (LAMBDA
   PERIOD
   LPAREN
   RPAREN
   TRUE
   FALSE
   IF
   THEN
   ELSE
   COLON
   RARROW
   SUCC
   PRED
   ISZERO
   EOF))

(define-tokens lambda-tokens
  (ID NUM))

(define lambda-lexer
  (lexer-src-pos
   ["λ" (token-LAMBDA)]
   ["." (token-PERIOD)]
   ["(" (token-LPAREN)]
   [")" (token-RPAREN)]
   [":" (token-COLON)]
   ["true" (token-TRUE)]
   ["false" (token-FALSE)]
   ["if" (token-IF)]
   ["then" (token-THEN)]
   ["else" (token-ELSE)]
   ["succ" (token-SUCC)]
   ["pred" (token-PRED)]
   ["iszero" (token-ISZERO)]
   ["->" (token-RARROW)]
   ; variable names
   [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z))) (token-ID (string->symbol lexeme))]
   ; numbers
   [(:+ numeric) (token-NUM (string->number lexeme))]
   ; comments like racket, we'll use ;
   [(:: ";" (:+ (char-complement (:or #\newline #\linefeed)))) (return-without-pos (lambda-lexer input-port))]
   ; ignore whitespace
   [whitespace (return-without-pos (lambda-lexer input-port))]
   [(eof) (token-EOF)]))


; position -> string -> error
; raises a lexing error
(define (raise-lex-error pos lexeme)
  (let* ([linenums? (not (eq? (position-line pos) #f))]
         [loc (if linenums? (position-line pos) (position-offset pos))]
         [col (position-col pos)]
         [partial-msg (string-append (if linenums? "syntax error at line "
                                         "syntax error at offset ") (number->string loc))]
         [msg (string-append partial-msg (if linenums? (string-append ", col " (number->string col)) "")
                             ": '" lexeme "'")])
         (raise-syntax-error 'dbnlexer msg)))


; input port -> thunk
; creates a thunk that when called will return the next token from the input stream
(define (get-tokenizer in)
  (λ () (lambda-lexer in)))


; input port -> list of tokens
; this function takes an input port and returns a list of
; tokens read from it (until it hits eof)
(define (lex in)
  (port-count-lines! in)
  (let ([tokenize (get-tokenizer in)])
    (define (lexfun)
      (let ([tok (tokenize)])
        (cond
          ; test to see if we hit eof as the base case
          [(eq? (position-token-token tok) (token-EOF)) null]
          [else (cons (position-token-token tok) (lexfun))])))
    (lexfun)))


; string -> list of tokens
; this function takes a string and returns a list of
; tokens read from it (until it reaches the end)
(define (lexstr str)
  (lex (open-input-string str)))

; filename -> list of tokens
; this function takes a filename, opens it as an input port,
; and then reads tokens until the end is reached
(define (lexfile filename)
  (lex (open-input-file filename)))