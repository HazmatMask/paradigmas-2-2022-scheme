#lang racket

(define createPixbit
  (lambda (XPos YPos Value Depth)
    (list XPos YPos Value Depth)))

(define pixbit?
  (lambda PB
        (if (and (not (null? PB))
             (integer? (car PB))
             (> (car PB) -1)
             (not (null? (cdr PB)))
             (integer? (cadr PB))
             (> (cadr PB) -1)
             (not (null? (cddr PB)))
             (integer? (caddr PB))
             (or (= 1 (caddr PB)) (= 0 caddr PB))
             (not (null? (cdddr PB)))
             (integer? (cadddr PB))
             (null? (cddddr PB)))
        #t
        #f)))

  