#lang racket

(provide (all-defined-out))

(define (append-last lst elem)
  (cond
    [(null? lst) (list elem)]
    [else (cons (car lst) (append-last (cdr lst) elem))]
    )
  )

(define (append-first lst elem)
  (cons elem lst)
  )


(define (list-less-than-number l num)
  (cond
    [(null? l) #t]
    [(not (number? (car l))) 'INVALID-COMPARISON]
    [(symbol? (list-less-than-number (cdr l) num)) 'INVALID-COMPARISON]
    [else (and (< (car l) num) (list-less-than-number (cdr l) num))]
    )
  )

(define (list-greater-than-number l num)
  (cond
    [(null? l) #t]
    [(not (number? (car l))) 'INVALID-COMPARISON]
    [(symbol? (list-greater-than-number (cdr l) num)) 'INVALID-COMPARISON]
    [else (and (> (car l) num) (list-greater-than-number (cdr l) num))]
    )
  )

(define (list-less-than-string l st)
  (cond
    [(null? l) #t]
    [(not (string? (car l))) 'INVALID-COMPARISON]
    [(symbol? (list-less-than-string (cdr l) st)) 'INVALID-COMPARISON]
    [else (and (string<? (car l) st) (list-less-than-string (cdr l) st))]
    )
  )

(define (list-greater-than-string l st)
  (cond
    [(null? l) #t]
    [(not (string? (car l))) 'INVALID-COMPARISON]
    [(symbol? (list-greater-than-string (cdr l) st)) 'INVALID-COMPARISON]
    [else (and (string>? (car l) st) (list-greater-than-string (cdr l) st))]
    )
  )

(define (list-add-number l num)
  (cond
    [(null? l) '()]
    [(not (number? (car l))) 'INVALID-ADDITION]
    [(symbol? (list-add-number (cdr l) num)) 'INVALID-ADDITION]
    [else (cons (+ (car l) num) (list-add-number (cdr l) num))]
    )
  )

(define (list-add-boolean l b)
  (cond
    [(null? l) '()]
    [(not (boolean? (car l))) 'INVALID-ADDITION]
    [(symbol? (list-add-boolean (cdr l) b)) 'INVALID-ADDITION]
    [else (cons (or (car l) b) (list-add-boolean (cdr l) b))]
    )
  )

(define (list-add-string-first l st)
  (cond
    [(null? l) '()]
    [(not (string? (car l))) 'INVALID-ADDITION]
    [(symbol? (list-add-string-first (cdr l) st)) 'INVALID-ADDITION]
    [else (cons (string-append st (car l)) (list-add-string-first (cdr l) st))]
    )
  )

(define (list-add-string-last l st)
  (cond
    [(null? l) '()]
    [(not (string? (car l))) 'INVALID-ADDITION]
    [(symbol? (list-add-string-last (cdr l) st)) 'INVALID-ADDITION]
    [else (cons (string-append (car l) st) (list-add-string-last (cdr l) st))]
    )
  )

(define (append-list list1 list2)
        (if (null? list1) list2
            (cons (car list1) (append (cdr list1) list2))))

(define (list-sub-number l num)
  (cond
    [(null? l) '()]
    [(not (number? (car l))) 'INVALID-SUBTRACTION]
    [(symbol? (list-sub-number (cdr l) num)) 'INVALID-SUBTRACTION]
    [else (cons (- (car l) num) (list-sub-number (cdr l) num))]
    )
  )

(define (number-sub-list l num)
  (cond
    [(null? l) '()]
    [(not (number? (car l))) 'INVALID-SUBTRACTION]
    [(symbol? (number-sub-list (cdr l) num)) 'INVALID-SUBTRACTION]
    [else (cons (- num (car l)) (number-sub-list (cdr l) num))]
    )
  )

(define (list-mult-number l num)
  (cond
    [(null? l) '()]
    [(not (number? (car l))) 'INVALID-MULTIPICATION]
    [(symbol? (list-mult-number (cdr l) num)) 'INVALID-MULTIPICATION]
    [else (cons (* (car l) num) (list-mult-number (cdr l) num))]
    )
  )

(define (list-mult-boolean l b)
  (cond
    [(null? l) '()]
    [(not (boolean? (car l))) 'INVALID-MULTIPICATION]
    [(symbol? (list-mult-boolean (cdr l) b)) 'INVALID-MULTIPICATION]
    [else (cons (and (car l) b) (list-mult-boolean (cdr l) b))]
    )
  )

(define (list-div-number l num)
  (cond
    [(null? l) '()]
    [(not (number? (car l))) 'INVALID-DIVISION]
    [(symbol? (list-div-number (cdr l) num)) 'INVALID-DIVISION]
    [else (cons (/ (car l) num) (list-div-number (cdr l) num))]
    )
  )

(define (number-div-list l num)
  (cond
    [(null? l) '()]
    [(not (number? (car l))) 'INVALID-DIVISION]
    [(symbol? (number-div-list (cdr l) num)) 'INVALID-DIVISION]
    [else (cons (/ num (car l)) (number-div-list (cdr l) num))]
    )
  )

(define (negate-list l)
  (cond
    [(null? l) '()]
    [(not (or (number? (car l)) (boolean? (car l)))) 'INVALID-NEGATIVE]
    [(symbol? (negate-list (cdr l))) 'INVALID-NEGATIVE]
    [else (cons (if (number? (car l)) (- 0 (car l)) (not (car l))) (negate-list (cdr l)))]
    )
  )

(define (search-by-first-elem lst elem comp)
  (cond
    [(null? lst) '()]
    [(and (comp (caar lst) elem #f) (not (symbol? (comp (caar lst) elem #f))) ) (cadar lst)]
    [else (search-by-first-elem (cdr lst) elem comp)]
    )
  )

(define (list-select l index)
  (cond
    [(and (list? l) (integer? index) (<= 0 index) (< index (length l)))  (list-ref l index)]
    [else 'INVALID-MEMBER]
    )
  )
