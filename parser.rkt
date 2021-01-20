#lang racket

(provide (all-defined-out))

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         "lexer.rkt"
         "utils.rkt")


(define project-parser
  (parser
   (start command)
   (end EOF)
   (error void)
   (tokens my-tokens my-empty-tokens)
   (grammar
    (command ((command SEMICOLON keyword) (append-last $1 $3))
             ((keyword) (list $1)))
    
    (keyword
             ((print_statement) $1)
             ((if_statement)     $1)
             ((assign_statement) $1)
             ((while_statement)  $1)
             ((return_statement) $1))
    
    (if_statement ((IF exp THEN command ELSE command END) (list 'IF $2 $4 $6)))
    (assign_statement ((VARIABLE ASSIGN exp) (list 'ASSIGN $1 $3)))
    (while_statement ((WHILE exp DO command END) (list 'WHILE $2 $4)))
    (return_statement ((RETURN exp) (list 'RETURN $2)))
    (print_statement ((PRINT PARAN-OPEN pexp PARAN-CLOSE) (list 'PRINT $3)))

    (pexp ((exp) (list 'PRINT-EXP $1))
          ((exp COMMA pexp) (list 'PRINT-PEXP $1 $3)))
    
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
          ((PARAN-OPEN exp PARAN-CLOSE) $2)
          ((NUMBER) (list 'DATA-NUMBER $1))
          ((NULL) (list 'DATA-NULL 'NULL))
          ((VARIABLE) (list 'EVAL-VAR $1))
          ((BOOLEAN) (list 'DATA-BOOL $1))
          ((STRING) (list 'DATA-STRING $1))
          ((VARIABLE listMember) (list 'LIST-SELECT $1 $2))
          ((list) $1))

    (list ((BRACKET-OPEN listValues BRACKET-CLOSE) (list 'DATA-LIST $2))
          ((BRACKET-OPEN BRACKET-CLOSE) (list 'DATA-LIST '())))

    (listValues ((exp) (list $1))
                ((exp COMMA listValues) (cons $1 $3)))

    (listMember ((BRACKET-OPEN exp BRACKET-CLOSE) (list $2))
                ((BRACKET-OPEN exp BRACKET-CLOSE listMember) (cons $2 $4)))
    
    )))

;(define my-lexer (lex-this project-lexer (open-input-string "print(2, 5, a)")))
;(let ((parser-res (project-parser my-lexer))) parser-res)
;(my-lexer)
