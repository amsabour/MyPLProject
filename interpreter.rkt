#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         "parser.rkt"
         "lexer.rkt")

(define (get-var var-name vars)
  (cond
    [(null? vars) 'VAR-DOESNT-EXIST]
    [(eq? var-name (caar vars)) (cadar vars)]
    [else (get-var var-name (cdr vars))])
  )

; return vars with updated value for var-name
(define (update-var var-name new-val vars)
  (cond
    [(null? vars) (list (list var-name new-val))]
    [(eq? var-name (caar vars)) (cons (list (list var-name new-val)) (cdr vars))]
    [else (cons (car vars) (update-var var-name new-val (cdr vars)))])
  )

(define (run-if-cmd expr then-pt else-pt vars)
  (let ((expr-val (evaluate-expr expr vars)))
    (if expr-val (interpret then-pt vars) (interpret else-pt vars))
    )
  )

(define (run-while-cmd expr pt vars)
  (do ((expr-val (evaluate-expr expr vars) (evaluate-expr expr vars)))
    ((or (and (not (null? vars)) (eq? 'PROGRAM-RETURNED (car vars))) (not expr-val)) vars)
    (set! vars (interpret pt vars))
    )
  )

(define (run-assign-cmd var-name expr vars)
  (let ((expr-val (evaluate-expr expr vars)))
    (update-var var-name expr-val vars)
    )
  )

(define (run-return-cmd expr vars)
  (let ((expr-val (evaluate-expr expr vars))) (list 'PROGRAM-RETURNED expr-val))
  )

; returns new vars or ('PROGRAM-RETURNED return-value)
(define (run-command cmd vars)
  (cond
    [(eq? (car cmd) 'IF) (run-if-cmd (cadr cmd) (caddr cmd) (cadddr cmd) vars)]
    [(eq? (car cmd) 'ASSIGN) (run-assign-cmd (cadr cmd) (caddr cmd) vars)]
    [(eq? (car cmd) 'WHILE) (run-while-cmd (cadr cmd) (caddr cmd) vars)]
    [(eq? (car cmd) 'RETURN) (run-return-cmd (cadr cmd) vars)]
    
    [else (display "SOMETHING IS WRONG") '(PROGRAM-RETURNED -9999999)]
    )
  )

; returns value
(define (evaluate-expr expr vars)
  (let ((a
  (cond
    [(eq? (car expr) 'DATA-NUMBER) (cadr expr)]
    [(eq? (car expr) 'DATA-NULL) (cadr expr)]
    [(eq? (car expr) 'DATA-BOOL) (cadr expr)]
    [(eq? (car expr) 'DATA-STRING) (cadr expr)]
    [(eq? (car expr) 'DATA-LIST) (map (lambda (l) (evaluate-expr l vars)) (cadr expr))]
    
    [(eq? (car expr) 'EVAL-VAR) (get-var (cadr expr) vars)]   ; Implement error handling if var doesn't exist

    [(eq? (car expr) 'LESS)      (evaluate-less-than (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))] ; Implement error handling
    [(eq? (car expr) 'GREATER)   (evaluate-greater-than (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))] ; Implement error handling
    [(eq? (car expr) 'EQUAL)     (evaluate-equal (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))] ; Implement error handling
    [(eq? (car expr) 'NOT-EQUAL) (evaluate-not-equal (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))] ; Implement error handling

    [(eq? (car expr) 'ADD)   (evaluate-add (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))] ; Implement error handling
    [(eq? (car expr) 'SUB)   (evaluate-sub (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))] ; Implement error handling
    [(eq? (car expr) 'MULT)  (evaluate-mult (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))] ; Implement error handling
    [(eq? (car expr) 'DIV)   (evaluate-div (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))] ; Implement error handling

    [(eq? (car expr) 'NEGATIVE)   (evaluate-negative (evaluate-expr (cadr expr) vars))] ; Implement error handling

    [(eq? (car expr) 'LIST-SELECT) (evaluate-list-select (get-var (cadr expr)) (evaluate-expr (caddr expr) vars))] ; Implement error handling
    
    [else (display "WTF IS GOING ON???") (display expr)(newline) 10]
    ))) (display "Expr ") (display expr) (display " with variables ") (display vars) (display " evaluated to ") (display a) (newline) a)
  )

;************************ TODO ************************
;*                                                    *
;*                   PEDRAM & ATRIN                   * 
;*                                                    *
;******************************************************
(define (evaluate-less-than expr1 expr2)       'LESS-THAN-NOT-IMPLEMENTED )
(define (evaluate-greater-than expr1 expr2)    'GREATER-THAN-NOT-IMPLEMENTED )
(define (evaluate-equal expr1 expr2)           'EQUAL-NOT-IMPLEMENTED )
(define (evaluate-not-equal expr1 expr2)       'NOT-EQUAL-NOT-IMPLEMENTED )

(define (evaluate-add expr1 expr2)             'ADD-NOT-IMPLEMENTED )
(define (evaluate-sub expr1 expr2)             'SUB-NOT-IMPLEMENTED )
(define (evaluate-mult expr1 expr2)            'MULT-NOT-IMPLEMENTED )
(define (evaluate-div expr1 expr2)             'DIV-NOT-IMPLEMENTED )

(define (evaluate-negative expr1)              'NEGATIVE-NOT-IMPLEMENTED )

(define (evaluate-list-select list index-list) 'LIST-SELECT-NOT-IMPLEMENTED)
;************************ END TODO ************************
;*                                                        *
;*                   PEDRAM & ATRIN                       * 
;*                                                        *
;**********************************************************

(define (interpret pt vars)
  (do ((pt pt (cdr pt)))
    ((or (null? pt) (and (not (null? vars)) (eq? (car vars) 'PROGRAM-RETURNED)) )
     (cond
       [(eq? (car vars) 'PROGRAM-RETURNED) vars]
       [(null? pt) vars]
       [else (display "Error in (interpret pt vars) function")]
       )
     )
      
    (set! vars (run-command (car pt) vars))
    )
  )

(define (evaluate file)
  (let ((my-lexer (lex-this project-lexer (open-input-file file))))
    (let ((parse-tree (project-parser my-lexer)))
      (display parse-tree)
      (newline)
      (interpret parse-tree '())
      )
    )
  )

(evaluate "sample_program.txt")
