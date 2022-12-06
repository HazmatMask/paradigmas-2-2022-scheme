#lang racket

(define createPixrgb
  (lambda (XPos YPos Red Green Blue Depth)
    (list XPos YPos Red Green Blue Depth)))

(define pixrgb?
  (lambda (PR)
        (if (and (not (null? PR))
             (integer? (car PR))
             (> (car PR) -1)
             (not (null? (cdr PR)))
             (integer? (cadr PR))
             (> (cadr PR) -1)
             (not (null? (cddr PR)))
             (integer? (caddr PR))
             (and (> 256 (caddr PR)) (< -1 (caddr PR)))
             (not (null? (cdddr PR)))
             (and (> 256 (cadddr PR)) (< -1 (cadddr PR)))
             (not (null? (cddddr PR)))
             (and (> 256 (car (cddddr PR))) (< -1 (car (cddddr PR))))
             (not (null? (cdr (cddddr PR))))
             (integer? (cadr (cddddr PR)))
             (null? (cddr (cddddr PR))))
            #t
            #f)))

(define select_pixrgb_x
  (lambda (PR)
    (car PR)))

(define select_pixrgb_y
  (lambda (PR)
    (cadr PR)))

(define select_pixrgb_value
  (lambda (PR)
    (caddr PR)))

(define select_pixrgb_depth
  (lambda (PR)
    (cadddr PR)))

(define mod_pixrgb_x
  (lambda (PR newX)
    (cons newX (cdr PR))))

(define mod_pixrgb_y
  (lambda (PR newY)
    (cons (car PR) (cons newY (cddr PR)))))

(define mod_pixrgb_value
  (lambda (PR newValue)
    (cons (car PR) (cons (cadr PR) (cons newValue (cdddr PR))))))

(define mod_pixrgb_depth
  (lambda (PR newDepth)
    (cons (car PR) (cons (cadr PR) (cons (caddr PR) (cons newDepth (cddddr PR)))))))

(provide (all-defined-out))