#lang racket

(define createPixbit
  (lambda (XPos YPos Value Depth)
    (list XPos YPos Value Depth)))

(define pixbit?
  (lambda (PB)
        (if (and (not (null? PB))
             (integer? (car PB))
             (> (car PB) -1)
             (not (null? (cdr PB)))
             (integer? (cadr PB))
             (> (cadr PB) -1)
             (not (null? (cddr PB)))
             (integer? (caddr PB))
             (or (= 1 (caddr PB)) (= 0 (caddr PB)))
             (not (null? (cdddr PB)))
             (integer? (cadddr PB))
             (null? (cddddr PB)))
            #t
        #f)))

(define select_pixbit_x
  (lambda (PB)
    (car PB)))

(define select_pixbit_y
  (lambda (PB)
    (cadr PB)))

(define select_pixbit_value
  (lambda (PB)
    (caddr PB)))

(define select_pixbit_depth
  (lambda (PB)
    (cadddr PB)))

(define mod_pixbit_x
  (lambda (PB newX)
    (cons newX (cdr PB))))

(define mod_pixbit_y
  (lambda (PB newY)
    (cons (car PB) (cons newY (cddr PB)))))

(define mod_pixbit_value
  (lambda (PB newValue)
    (cons (car PB) (cons (cadr PB) (cons newValue (cdddr PB))))))

(define mod_pixbit_depth
  (lambda (PB newDepth)
    (cons (car PB) (cons (cadr PB) (cons (caddr PB) (cons newDepth (cddddr PB)))))))

(provide (all-defined-out))