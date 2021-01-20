#lang racket

(provide (all-defined-out))

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define-tokens my-tokens (VARIABLE NUMBER BOOLEAN STRING))
(define-empty-tokens my-empty-tokens (EOF
                                      NULL
                                      WHILE DO IF THEN ELSE END RETURN
                                      SEMICOLON COMMA COLON
                                      ASSIGN
                                      LESS GREATER EQUAL NOT-EQUAL
                                      PLUS MINUS MULT DIV
                                      PARAN-OPEN PARAN-CLOSE
                                      BRACKET-OPEN BRACKET-CLOSE
                                      PRINT PRINT-EXP PRINT-PEXP
                                      SWITCH CASE))
(define project-lexer
  (lexer
   [(eof) (token-EOF)]
   [whitespace (project-lexer input-port)]
   
   ["null"   (token-NULL)]
   
   ["while"  (token-WHILE)]
   ["do"     (token-DO)]
   ["if"     (token-IF)]
   ["then"   (token-THEN)]
   ["else"   (token-ELSE)]
   ["end"    (token-END)]
   ["return" (token-RETURN)]

   ;print & switch-case tokens
   ["print" (token-PRINT)]
   ["switch" (token-SWITCH)]
   ["case" (token-CASE)]
   
   [";" (token-SEMICOLON)]
   [":" (token-COLON)]
   ["," (token-COMMA)]
   
   ["=" (token-ASSIGN)]
   
   ["<" (token-LESS)]
   [">" (token-GREATER)]
   ["==" (token-EQUAL)]
   ["!=" (token-NOT-EQUAL)]
   
   ["+" (token-PLUS)]
   ["-" (token-MINUS)]
   ["*" (token-MULT)]
   ["/" (token-DIV)]

   ["(" (token-PARAN-OPEN)]
   [")" (token-PARAN-CLOSE)]
   ["[" (token-BRACKET-OPEN)]
   ["]" (token-BRACKET-CLOSE)]

   ["true"  (token-BOOLEAN #t)]
   ["false" (token-BOOLEAN #f)]
   [(:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-NUMBER (string->number lexeme))]
   [(:: #\" (complement (:: any-string #\" any-string)) #\") (token-STRING (substring lexeme 1 (- (string-length lexeme) 1)))]
   [(:+ alphabetic) (token-VARIABLE (string->symbol lexeme))]
   )
  )

(define lex-this (lambda (lexer input) (lambda () (lexer input))))