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

(define get_pix_x
  (lambda (P)
    (car P)))

(define get_pix_y
  (lambda (P)
    (cadr P)))

(define get_image_pixels
  (lambda (I)
    (caddr I)))

(define get_image_compress
  (lambda (I)
    (cadddr I)))

(define mod_pix_x
  (lambda (P NewX)
    (cons NewX (cdr P))))

(define mod_pix_y
  (lambda (P NewY)
    (cons (car P) (cons NewY (cddr P)))))

(define flipHList
  (lambda (XSize PixList)
    (if (null? PixList)
        '()
        (cons (mod_pix_x (car PixList) (- (- XSize 1) (get_pix_x (car PixList))))
              (flipHList XSize (cdr PixList))))))

(define flipH
  (lambda (I)
    (list (get_pix_x I) (get_pix_y I) (flipHList (get_pix_x I) (get_image_pixels I)))))

(define flipVList
  (lambda (YSize PixList)
    (if (null? PixList)
        '()
        (cons (mod_pix_y (car PixList) (- (YSize 1) (get_pix_y (car PixList))))
              (flipVList YSize (cdr PixList))))))

(define flipV
  (lambda (I)
    (list (get_pix_x I) (get_pix_y I) (flipVList (get_pix_y I) (get_image_pixels I)))))

(define cropList
  (lambda (X1 Y1 X2 Y2 PixList)
    (if (null? PixList)
        '()
        (if (and (< (- X1 1) (get_pix_x (car PixList))) (> (+ X2 1) (get_pix_x (car PixList)))
                 (< (- Y1 1) (get_pix_y (car PixList))) (> (+ Y2 1) (get_pix_y (car PixList))))
            (cons (car PixList) (cropList X1 Y1 X2 Y2 (cdr PixList)))
            (cropList X1 Y1 X2 Y2 (cdr PixList))))))


(define crop
  (lambda (I X1 Y1 X2 Y2)
    (list (get_pix_x I) (get_pix_y I) (cropList X1 Y1 X2 Y2 (get_image_pixels I)))))

(define rgb->hex
  (lambda (rgb)
    (if (= 0 rgb) "0" (if (= 1 rgb) "1" (if (= 2 rgb) "2" (if (= 3 rgb) "3" (if (= 4 rgb) "4"
    (if (= 5 rgb) "5" (if (= 6 rgb) "6" (if (= 7 rgb) "7" (if (= 8 rgb) "8" (if (= 9 rgb) "9"
    (if (= 10 rgb) "A" (if (= 11 rgb) "B" (if (= 12 rgb) "C" (if (= 13 rgb) "D" (if (= 14 rgb) "E" (if (= 15 rgb) "F" #f))))))))))))))))))      

(define RGBChannel->Hex
  (lambda (N)
    (string-append (rgb->hex (quotient N 16)) (rgb->hex (modulo N 16)))))

(define RGBColor->HEXColor
  (lambda (R G B)
    (string-append (RGBChannel->Hex R) (RGBChannel->Hex G) (RGBChannel->Hex B))))

(define RGBList->HEXList
  (lambda (PixList)
    (if (null? PixList)
        '()
        (cons (createPixhex (select_pixrgb_x (car PixList))
                            (select_pixrgb_y (car PixList))
                            (RGBColor->HEXColor (select_pixrgb_red (car PixList))
                                                (select_pixrgb_green (car PixList))
                                                (select_pixrgb_blue (car PixList)))
                            (select_pixrgb_depth (car PixList)))
              (RGBList->HEXList (cdr PixList))))))

(define imgRGB->imgHex
  (lambda (I)
    (list (get_pix_x I) (get_pix_y I) (RGBList->HEXList (get_image_pixels I)))))