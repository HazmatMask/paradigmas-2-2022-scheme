#lang racket

(require "TDA-pixbit.rkt")
(require "TDA-pixrgb.rkt")
(require "TDA-pixhex.rkt")

(define image
  (lambda (width height . nPixels)
    (list width height nPixels)))

(define image?
  (lambda (I)
    (if (and (not (null? I))
             (integer? (car I))
             (< -1 (car I))
             (not (null? (cdr I)))
             (integer? (cadr I))
             (< -1 (cadr I))
             (not (null? (cddr I)))
             (list? (caddr I)))
        #t
        #f)))

(define bitmap?Rec
  (lambda (L)
    (if (null? L)
        #t
        (if (pixbit? (car L))
            (bitmap?Rec (cdr L))
            #f))))
        
(define bitmap?
  (lambda (I)
    (if (and (image? I)
             (bitmap?Rec (caddr I))
             (null? (cdddr I)))
        #t
        #f)))
             
(define pixmap?Rec
  (lambda (L)
    (if (null? L)
        #t
        (if (pixrgb? (car L))
            (pixmap?Rec (cdr L))
            #f))))
        
(define pixmap?
  (lambda (I)
    (if (and (image? I)
             (pixmap?Rec (caddr I))
             (null? (cdddr I)))
        #t
        #f)))

(define hexmap?Rec
  (lambda (L)
    (if (null? L)
        #t
        (if (pixhex? (car L))
            (hexmap?Rec (cdr L))
            #f))))
        
(define hexmap?
  (lambda (I)
    (if (and (image? I)
             (hexmap?Rec (caddr I))
             (null? (cdddr I)))
        #t
        #f)))

(define compPix?
  (lambda (P)
    (if (and (not (null? P))
             (integer? (car P))
             (< -1 (car P))
             (not (null? (cdr P)))
             (integer? (cadr P))
             (< -1 (cadr P))
             (not (null? (cddr P)))
             (integer? (caddr P))
             (null? (cdddr P)))
        #t
        #f)))

(define compressed?Rec
  (lambda (L)
    (if (null? L)
        #t
        (if (compPix? (car L))
            (compressed?Rec (cdr L))
            #f))))

(define compressed?
  (lambda (I)
    (if (and (image? I)
             (not (null? (cdddr I)))
             (compressed?Rec (cdr (cadddr I))))
        #t
        #f)))