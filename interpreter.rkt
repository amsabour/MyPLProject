#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         "parser.rkt"
         "lexer.rkt"
         "utils.rkt")

(define (get-var var-name vars)
  (cond
    [(null? vars) (display "VAR ") (display var-name) (display " IS NOT DEFINED!") (newline) 'VAR-DOESNT-EXIST]
    
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

(define (run-print-cmd pexpr vars)
  (display "Prints: ")
  (cond
    [(eq? (car pexpr) 'PRINT-EXP) (display (evaluate-expr-for-print (cadr pexpr) vars)) (newline)]
    [(eq? (car pexpr) 'PRINT-PEXP) (display (evaluate-expr-for-print (cadr pexpr) vars)) (display (run-print-cmd (caddr pexpr) vars))]
    [else (display "SOMETHING WENT WRONG!") '(PROGRAM-RETURNED -9999999)]
    ) vars
  )

(define (run-if-cmd expr then-pt else-pt vars)
  (let ((expr-val (evaluate-expr expr vars)))
    (if (symbol? expr-val) (list 'PROGRAM-RETURNED expr-val) (if expr-val (interpret then-pt vars) (interpret else-pt vars)))
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
    (if (symbol? expr-val) (list 'PROGRAM-RETURNED expr-val) (update-var var-name expr-val vars))
    )
  )

(define (run-return-cmd expr vars)
  (let ((expr-val (evaluate-expr expr vars))) (list 'PROGRAM-RETURNED expr-val))
  )


(define (switch-exp-eval-loop cases vars)
  (cond
    [(null? cases) '()]
    [else (let ((expr-val (evaluate-expr (caar cases) vars)) (the-rest (switch-exp-eval-loop (cdr cases) vars)))
            (cond
              [(symbol? expr-val) expr-val]
              [(symbol? the-rest) the-rest]
              [else (cons (list expr-val (cadar cases)) the-rest)]
              ))]
    )
  )

(define (run-switch-cmd expr cases vars)
  (let ((expr-val (evaluate-expr expr vars)) (cases-val (switch-exp-eval-loop cases vars)))
    (cond
      [(symbol? expr-val) (list 'PROGRAM-RETURNED expr-val)]
      [(symbol? cases-val) (list 'PROGRAM-RETURNED cases-val)]
      [else (interpret (search-by-first-elem cases-val expr-val evaluate-equal) vars)]
    )
  )
)

; returns new vars or ('PROGRAM-RETURNED return-value)
(define (run-command cmd vars)
  (cond
    [(eq? (car cmd) 'IF) (run-if-cmd (cadr cmd) (caddr cmd) (cadddr cmd) vars)]
    [(eq? (car cmd) 'ASSIGN) (run-assign-cmd (cadr cmd) (caddr cmd) vars)]
    [(eq? (car cmd) 'WHILE) (run-while-cmd (cadr cmd) (caddr cmd) vars)]
    [(eq? (car cmd) 'RETURN) (run-return-cmd (cadr cmd) vars)]
    [(eq? (car cmd) 'PRINT) (run-print-cmd (cadr cmd) vars)]
    [(eq? (car cmd) 'SWITCH) (run-switch-cmd (cadr cmd) (caddr cmd) vars)]
    
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
    
    [(eq? (car expr) 'EVAL-VAR) (get-var (cadr expr) vars)]

    [(eq? (car expr) 'LESS)      (evaluate-less-than (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))]
    [(eq? (car expr) 'GREATER)   (evaluate-greater-than (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))]
    [(eq? (car expr) 'EQUAL)     (evaluate-equal (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))]
    [(eq? (car expr) 'NOT-EQUAL) (evaluate-not-equal (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))]

    [(eq? (car expr) 'ADD)   (evaluate-add (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))]
    [(eq? (car expr) 'SUB)   (evaluate-sub (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))]
    [(eq? (car expr) 'MULT)  (evaluate-mult (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))]
    [(eq? (car expr) 'DIV)   (evaluate-div (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))]

    [(eq? (car expr) 'NEGATIVE)   (evaluate-negative (evaluate-expr (cadr expr) vars))]

    [(eq? (car expr) 'LIST-SELECT) (evaluate-list-select (get-var (cadr expr) vars) (map (lambda (l) (evaluate-expr l vars)) (caddr expr)))]
    
    [else (display "WTF IS GOING ON???") (display expr)(newline) 10]
    ))) (display "Expr ") (display expr) (display " with variables ") (display vars) (display " evaluated to ") (display a) (newline) a)
  )

(define (evaluate-expr-for-print expr vars)
  (let ((a
  (cond
    [(eq? (car expr) 'DATA-NUMBER) (cadr expr)]
    [(eq? (car expr) 'DATA-NULL) (cadr expr)]
    [(eq? (car expr) 'DATA-BOOL) (cadr expr)]
    [(eq? (car expr) 'DATA-STRING) (substring (cadr expr) 1 (- (string-length  (cadr expr)) 1))]
    [(eq? (car expr) 'DATA-LIST) (map (lambda (l) (evaluate-expr l vars)) (cadr expr))]
    
    [(eq? (car expr) 'EVAL-VAR) (get-var (cadr expr) vars)]

    [(eq? (car expr) 'LESS)      (evaluate-less-than (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))]
    [(eq? (car expr) 'GREATER)   (evaluate-greater-than (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))]
    [(eq? (car expr) 'EQUAL)     (evaluate-equal (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))]
    [(eq? (car expr) 'NOT-EQUAL) (evaluate-not-equal (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))]

    [(eq? (car expr) 'ADD)   (evaluate-add (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))]
    [(eq? (car expr) 'SUB)   (evaluate-sub (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))]
    [(eq? (car expr) 'MULT)  (evaluate-mult (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))]
    [(eq? (car expr) 'DIV)   (evaluate-div (evaluate-expr (cadr expr) vars) (evaluate-expr (caddr expr) vars))]

    [(eq? (car expr) 'NEGATIVE)   (evaluate-negative (evaluate-expr (cadr expr) vars))]

    [(eq? (car expr) 'LIST-SELECT) (evaluate-list-select (get-var (cadr expr)) (evaluate-expr (caddr expr) vars))]
    
    [else (display "WTF IS GOING ON???") (display expr)(newline) 10]
    ))) a)
  )


;******************************************************
(define (evaluate-less-than expr1 expr2 [print-error #t])
  (cond
    [(and (number? expr1) (number? expr2)) (< expr1 expr2)]
    [(and (string? expr1) (string? expr2)) (string<? expr1 expr2)]
    [(and (list? expr1) (number? expr2)) (list-less-than-number expr1 expr2)]
    [(and (list? expr1) (string? expr2)) (list-less-than-string expr1 expr2)]
    [else (when print-error (display expr1) (display " < ") (display expr2) (display " : INVALID OPERATION!") (newline)) 'INVALID-COMPARISON]
    )
  )

(define (evaluate-greater-than expr1 expr2 [print-error #t])
  (cond
    [(and (number? expr1) (number? expr2)) (> expr1 expr2)]
    [(and (string? expr1) (string? expr2)) (string>? expr1 expr2)]
    [(and (list? expr1) (number? expr2)) (list-greater-than-number expr1 expr2)]
    [(and (list? expr1) (string? expr2)) (list-greater-than-string expr1 expr2)]
    [else (when print-error (display expr1) (display " > ") (display expr2) (display " : INVALID OPERATION!") (newline)) 'INVALID-COMPARISON]
    )
  )

(define (evaluate-equal expr1 expr2 [print-error #t])
  (cond
    [(and (number? expr1) (number? expr2)) (equal? expr1 expr2)]
    [(and (string? expr1) (string? expr2)) (equal? expr1 expr2)]
    [(and (symbol? expr1) (symbol? expr2) (equal? expr1 'NULL) (equal? expr2 'NULL)) (#t)]
    [(and (boolean? expr1) (boolean? expr2)) (equal? expr1 expr2)]
    [(and (list? expr1) (list? expr2)) (equal? expr1 expr2)]
    [else (when print-error (display expr1) (display " == ") (display expr2) (display " : INVALID OPERATION!") (newline)) 'INVALID-COMPARISON]
    )
  )

(define (evaluate-not-equal expr1 expr2 [print-error #t])
  (cond
    [(and (number? expr1) (number? expr2)) (not (equal? expr1 expr2))]
    [(and (string? expr1) (string? expr2)) (not (equal? expr1 expr2))]
    [(and (symbol? expr1) (symbol? expr2) (equal? expr1 'NULL) (equal? expr2 'NULL)) (#f)]
    [(and (boolean? expr1) (boolean? expr2)) (not (equal? expr1 expr2))]
    [(and (list? expr1) (list? expr2)) (not (equal? expr1 expr2))]
    [else (when print-error (display expr1) (display " != ") (display expr2) (display " : INVALID OPERATION!") (newline)) 'INVALID-COMPARISON]
    )
  )

(define (evaluate-add expr1 expr2 [print-error #t])
  (cond
    [(and (number? expr1) (number? expr2)) (+ expr1 expr2)]
    [(and (boolean? expr1) (boolean? expr2)) (or expr1 expr2)]
    [(and (list? expr1) (number? expr2)) (list-add-number expr1 expr2)]
    [(and (number? expr1) (list? expr2)) (list-add-number expr2 expr1)]
    [(and (list? expr1) (boolean? expr2)) (list-add-boolean expr1 expr2)]
    [(and (boolean? expr1) (list? expr2)) (list-add-boolean expr2 expr1)]
    [(and (string? expr1) (string? expr2)) (string-append expr1 expr2)]
    [(and (string? expr1) (list? expr2)) (list-add-string-first expr2 expr1)]
    [(and (list? expr1) (string? expr2)) (list-add-string-last expr1 expr2)]
    [(and (list? expr1) (list? expr2)) (append-list expr1 expr2)]
    [else (when print-error (display expr1) (display " + ") (display expr2) (display " : INVALID OPERATION!") (newline)) 'INVALID-ADDITION]
    )
  )

(define (evaluate-sub expr1 expr2 [print-error #t])
  (cond
    [(and (number? expr1) (number? expr2)) (- expr1 expr2)]
    [(and (list? expr1) (number? expr2)) (list-sub-number expr1 expr2)]
    [(and (number? expr1) (list? expr2)) (number-sub-list expr2 expr1)]
    [else (when print-error (display expr1) (display " - ") (display expr2) (display " : INVALID OPERATION!") (newline)) 'INVALID-SUBTRACTION]
    )
  )

(define (evaluate-mult expr1 expr2 [print-error #t])
  (cond
    [(and (number? expr1) (number? expr2)) (* expr1 expr2)]
    [(and (boolean? expr1) (boolean? expr2)) (and expr1 expr2)]
    [(and (list? expr1) (number? expr2)) (list-mult-number expr1 expr2)]
    [(and (number? expr1) (list? expr2)) (list-mult-number expr2 expr1)]
    [(and (list? expr1) (boolean? expr2)) (list-mult-boolean expr1 expr2)]
    [(and (boolean? expr1) (list? expr2)) (list-mult-boolean expr2 expr1)]
    [else (when print-error (display expr1) (display " * ") (display expr2) (display " : INVALID OPERATION!") (newline)) 'INVALID-MULTIPICATION]
    )
  )

(define (evaluate-div expr1 expr2 [print-error #t])
  (cond
    [(and (number? expr1) (number? expr2)) (/ expr1 expr2)]
    [(and (list? expr1) (number? expr2)) (list-div-number expr1 expr2)]
    [(and (number? expr1) (list? expr2)) (number-div-list expr2 expr1)]
    [else (when print-error (display expr1) (display " / ") (display expr2) (display " : INVALID OPERATION!") (newline)) 'INVALID-DIVISION]
    )
  )

(define (evaluate-negative expr1 [print-error #t])
  (cond
    [(number? expr1)  (- 0 expr1)]
    [(boolean? expr1)  (not expr1)]
    [(list? expr1)  (negate-list expr1)]
    [else (when print-error (display "-") (display expr1) (display " : INVALID OPERATION!") (newline)) 'INVALID-NEGATIVE]
    )
  )

(define (evaluate-list-select l index-list)
  (cond
    [(null? index-list) l]
    [else (let ((first-elem (list-select l (car index-list))))
            (cond
              [(symbol? first-elem) first-elem]
              [else (evaluate-list-select first-elem (cdr index-list))]
              ))]
    )
  )

;**********************************************************

(define (interpret pt vars)
  
  (do ((pt pt (cdr pt)))
    ((or (null? pt) (and (not (null? vars)) (eq? (car vars) 'PROGRAM-RETURNED)) )
     (cond
       [(null? pt) vars]
       [(eq? (car vars) 'PROGRAM-RETURNED) vars]
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
