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