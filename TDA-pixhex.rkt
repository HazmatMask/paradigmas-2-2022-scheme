#lang racket

(define createPixhex
  (lambda (XPos YPos Value Depth)
    (list XPos YPos Value Depth)))

(define hex?
  (lambda (H)
  (if (or (equal? #\0 H) (equal? #\1 H) (equal? #\2 H)
          (equal? #\3 H) (equal? #\4 H) (equal? #\5 H)
          (equal? #\6 H) (equal? #\7 H) (equal? #\8 H)
          (equal? #\9 H) (equal? #\A H) (equal? #\B H)
          (equal? #\C H) (equal? #\D H) (equal? #\E H)
          (equal? #\F H))
      #t
      #f)))

(define hexList?
  (lambda (HL)
    (if (null? HL)
        #t
        (if (hex? (car HL))
            (hexList? (cdr HL))
            #f))))

(define hexValue?
  (lambda (S)
      (if (and (string? S)
               (= 6 (string-length S))
               (hexList? (string->list S)))
          #t
          #f)))

(define pixhex?
  (lambda (PH)
        (if (and (not (null? PH))
             (integer? (car PH))
             (> (car PH) -1)
             (not (null? (cdr PH)))
             (integer? (cadr PH))
             (> (cadr PH) -1)
             (not (null? (cddr PH)))
             (hexValue? (caddr PH))
             (not (null? (cdddr PH)))
             (integer? (cadddr PH))
             (null? (cddddr PH)))
            #t
        #f)))

(define select_pixhex_x
  (lambda (PH)
    (car PH)))

(define select_pixhex_y
  (lambda (PH)
    (cadr PH)))

(define select_pixhex_value
  (lambda (PH)
    (caddr PH)))

(define select_pixhex_depth
  (lambda (PH)
    (cadddr PH)))

(define mod_pixhex_x
  (lambda (PH newX)
    (cons newX (cdr PH))))

(define mod_pixhex_y
  (lambda (PH newY)
    (cons (car PH) (cons newY (cddr PH)))))

(define mod_pixhex_value
  (lambda (PH newValue)
    (cons (car PH) (cons (cadr PH) (cons newValue (cdddr PH))))))

(define mod_pixhex_depth
  (lambda (PH newDepth)
    (cons (car PH) (cons (cadr PH) (cons (caddr PH) (cons newDepth (cddddr PH)))))))

(provide (all-defined-out))