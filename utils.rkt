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