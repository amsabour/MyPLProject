#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)


(require "utils.rkt")



(define-tokens my-tokens (VARIABLE NUMBER BOOLEAN STRING))
(define-empty-tokens my-empty-tokens (EOF
                                      NULL
                                      WHILE DO IF THEN ELSE END RETURN
                                      SEMICOLON COMMA
                                      ASSIGN
                                      LESS GREATER EQUAL NOT-EQUAL
                                      PLUS MINUS MULT DIV
                                      PARAN-OPEN PARAN-CLOSE
                                      BRACKET-OPEN BRACKET-CLOSE))

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

   [";" (token-SEMICOLON)]
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
   [(:: #\" any-string #\") (token-STRING lexeme)]
   [(:+ alphabetic) (token-VARIABLE (string->symbol lexeme))]
   )
  )


(define project-parser
  (parser
   (start command)
   (end EOF)
   (error void)
   (tokens my-tokens my-empty-tokens)
   (grammar
    (command ((command SEMICOLON keyword) (append-last $1 $3))
             ((keyword) (list $1)))
    
    (keyword ((if_statement)     $1)
             ((assign_statement) $1)
             ((while_statement)  $1)
             ((return_statement) $1))
    
    (if_statement ((IF exp THEN command ELSE command END) (list 'IF $2 $4 $6)))
    (assign_statement ((VARIABLE ASSIGN exp) (list 'ASSIGN $1 $3)))
    (while_statement ((WHILE exp DO command END) (list 'WHILE $2 $4)))
    (return_statement ((RETURN exp) (list 'RETURN $2)))

    (exp ((aexp) $1)
         ((aexp GREATER aexp) (list 'GREATER $1 $3))
         ((aexp LESS aexp) (list 'LESS $1 $3))
         ((aexp EQUAL aexp) (list 'EQUAL $1 $3))
         ((aexp NOT-EQUAL aexp) (list 'NOT-EQUAL $1 $3)))

    (aexp ((bexp) $1)
          ((bexp MINUS aexp) (list 'SUB $1 $3))
          ((bexp PLUS aexp) (list 'ADD $1 $3)))

    (bexp ((cexp) $1)
          ((cexp MULT bexp) (list 'MULT $1 $3))
          ((cexp DIV bexp) (list 'DIV $1 $3)))

    (cexp ((MINUS cexp) (list 'NEGATIVE $2))
          ((PARAN-OPEN exp PARAN-CLOSE) ($2))
          ((NUMBER) (list 'DATA-NUMBER $1))
          ((NULL) (list 'DATA-NULL 'NULL))
          ((VARIABLE) (list 'EVAL-VAR $1))
          ((BOOLEAN) (list 'DATA-BOOL $1))
          ((STRING) (list 'DATA-STRING $1))
          ((VARIABLE listMember) (list 'LIST-SELECT $1 $2))
          ((list) $1))

    (list ((BRACKET-OPEN listValues BRACKET-CLOSE) (list 'DATA-LIST $2))
          ((BRACKET-OPEN BRACKET-CLOSE) (list 'DATA-list '())))

    (listValues ((exp) (list $1))
                ((exp COMMA listValues) (cons $1 $3)))

    (listMember ((BRACKET-OPEN exp BRACKET-CLOSE) (list $2))
                ((BRACKET-OPEN exp BRACKET-CLOSE listMember) (cons $2 $4)))
    
    )))

(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this project-lexer (open-input-string "a=b[2][3][4]; return a")))
(let ((parser-res (project-parser my-lexer))) parser-res)
