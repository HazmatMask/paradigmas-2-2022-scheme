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
    (cdddr I)))

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

(define hex->rgb
  (lambda (hex)
    (if (equal? #\0 hex) 0 (if (equal? #\1 hex) 1 (if (equal? #\2 hex) 2 (if (equal? #\3 hex) 3 (if (equal? #\4 hex) 4 (if (equal? #\5 hex) 5 (if (equal? #\6 hex) 6
    (if (equal? #\7 hex) 7 (if (equal? #\8 hex) 8 (if (equal? #\9 hex) 9 (if (equal? #\A hex) 10 (if (equal? #\B hex) 11 (if (equal? #\C hex) 12 (if (equal? #\D hex) 13
    (if (equal? #\E hex) 14 (if (equal? #\F hex) 15 #f))))))))))))))))))

(define HexChannel->RGB
  (lambda (A B)
    (+ (* 16 (hex->rgb A)) (hex->rgb B))))

(define HEXColor->RedColor
  (lambda (HEX)
    (HexChannel->RGB (car (string->list HEX)) (cadr (string->list HEX)))))

(define HEXColor->GreenColor
  (lambda (HEX)
    (HexChannel->RGB (caddr (string->list HEX)) (cadddr (string->list HEX)))))

(define HEXColor->BlueColor
  (lambda (HEX)
    (HexChannel->RGB (car (cddddr (string->list HEX))) (cadr (cddddr (string->list HEX))))))

(define HEXList->RGBList
  (lambda (PixList)
    (if (null? PixList)
        '()
        (cons (createPixrgb (select_pixhex_x (car PixList))
                            (select_pixhex_y (car PixList))
                            (HEXColor->RedColor (select_pixhex_value (car PixList))) 
                            (HEXColor->GreenColor (select_pixhex_value (car PixList)))
                            (HEXColor->BlueColor (select_pixhex_value (car PixList)))
                            (select_pixhex_depth (car PixList)))
              (HEXList->RGBList (cdr PixList))))))

(define isBINinList
  (lambda (Bin List)
    (if (null? List)
        #f
        (if (equal? (caar List) Bin)
            #t
            (isBINinList Bin (cdr List))))))

(define addBINValue
  (lambda (BinValue List)
    (cons (list BinValue 1) List)))

(define upBINValue
  (lambda (BinValue List)
    (if (= BinValue (caar List))
        (cons (list (caar List) (+ 1 (cadar List))) (cdr List))
        (cons (car List) (upBINValue BinValue (cdr List))))))

(define runBINList
  (lambda (List AuxList)
    (if (null? List)
        AuxList
        (if (isBINinList (select_pixbit_value (car List)) AuxList)
            (runBINList (cdr List) (upBINValue (select_pixbit_value (car List)) AuxList))
            (runBINList (cdr List) (addBINValue (select_pixbit_value (car List)) AuxList))))))

(define isRGBinList
  (lambda (R G B List)
    (if (null? List)
        #f
        (if (and (= R (car (car List)))
                 (= G (cadr (car List)))
                 (= B (caddr (car List))))
            #t
            (isRGBinList R G B (cdr List))))))

(define addRGBValue
  (lambda (RedValue GreenValue BlueValue List)
    (cons (list RedValue GreenValue BlueValue 1) List)))

(define upRGBValue
  (lambda (RedValue GreenValue BlueValue List)
    (if (and (= RedValue (car (car List)))
             (= GreenValue (cadr (car List)))
             (= BlueValue (caddr (car List))))
        (cons (list (car (car List)) (cadr (car List)) (caddr (car List)) (+ 1 (cadddr (car List)))) (cdr List))
        (cons (car List) (upRGBValue RedValue GreenValue BlueValue (cdr List))))))

(define runRGBList
  (lambda (List AuxList)
    (if (null? List)
        AuxList
        (if (isRGBinList (select_pixrgb_red (car List)) (select_pixrgb_green (car List)) (select_pixrgb_blue (car List)) AuxList)
            (runRGBList (cdr List) (upRGBValue (select_pixrgb_red (car List)) (select_pixrgb_green (car List)) (select_pixrgb_blue (car List)) AuxList))
            (runRGBList (cdr List) (addRGBValue (select_pixrgb_red (car List)) (select_pixrgb_green (car List)) (select_pixrgb_blue (car List)) AuxList))))))
            
(define histogram
  (lambda (I)
    (if (bitmap? I)
        (runBINList (get_image_pixels I) '())
        (if (pixmap? I)
            (runRGBList (get_image_pixels I) '())
            (if (hexmap? I)
                (runRGBList (HEXList->RGBList (get_image_pixels I)) '())
                #f)))))

(define rotate90Rec
  (lambda (YSize PixList)
    (if (null? PixList)
        '()
        (cons (mod_pix_x (mod_pix_y (car PixList) (get_pix_x (car PixList))) (- (- YSize 1) (get_pix_y (car PixList))))
              (rotate90Rec YSize (cdr PixList))))))

(define rotate90
  (lambda (I)
    (list (get_pix_x I) (get_pix_y I) (rotate90Rec (get_pix_y I) (get_image_pixels I)))))

(define createCompressedImage
  (lambda (Image CompressedList ClearList)
    (list (get_pix_x Image) (get_pix_y Image) ClearList (cons CompressedList (get_image_compress Image)))))

(define recMostFrequentBINHisto
  (lambda (Histo Aux)
    (if (null? (cdr Histo))
        (if (> (cadr (car Histo)) (cadr Aux))
            (caar Histo)
            (car Aux))
        (if (> (cadr (car Histo)) (cadr Aux))
            (recMostFrequentBINHisto (cdr Histo) (car Histo))
            (recMostFrequentBINHisto (cdr Histo) Aux)))))

(define mostFrequentBINHisto
  (lambda (Histo)
    (recMostFrequentBINHisto Histo (list 0 0))))

(define recCompressBINList_newList
  (lambda (List Color AuxList)
    (if (null? List)
        AuxList
        (if (equal? Color (select_pixbit_value (car List)))
            (recCompressBINList_newList (cdr List) Color (cons (list
                                                                (select_pixbit_x (car List))
                                                                (select_pixbit_y (car List))
                                                                (select_pixbit_depth (car List)))
                                                               AuxList))
            (recCompressBINList_newList (cdr List) Color AuxList)))))

(define compressBINList_newList
  (lambda (List Color)
    (recCompressBINList_newList List Color '())))

(define recCompressBINList_clear
  (lambda (List Color)
    (if (null? List)
        '()
        (if (equal? Color (select_pixbit_value (car List)))
            (recCompressBINList_clear (cdr List) Color)
            (cons (car List) (recCompressBINList_clear (cdr List) Color))))))

(define compressBINList_clear
  (lambda (List Color)
    (recCompressBINList_clear List Color)))

(define compressBitmap
  (lambda (Image)
    (createCompressedImage Image
                           (cons (mostFrequentBINHisto (histogram Image))
                                       (compressBINList_newList (get_image_pixels Image)
                                                                (mostFrequentBINHisto (histogram Image))))
                           (compressBINList_clear (get_image_pixels Image)
                             (mostFrequentBINHisto (histogram Image))))))

(define recMostFrequentRGBHisto
  (lambda (Histo Aux)
    (if (null? (cdr Histo))
        (if (> (cadddr (car Histo)) (cadddr Aux))
            (list (car (car Histo)) (cadr (car Histo)) (caddr (car Histo)))
            (list (car Aux) (cadr Aux) (caddr Aux)))
        (if (> (cadddr (car Histo)) (cadddr Aux))
            (recMostFrequentRGBHisto (cdr Histo) (car Histo))
            (recMostFrequentRGBHisto (cdr Histo) Aux)))))

(define mostFrequentRGBHisto
  (lambda (Histo)
    (recMostFrequentRGBHisto Histo (list 0 0 0 0))))

(define recCompressRGBList_newList
  (lambda (List Color AuxList)
    (if (null? List)
        AuxList
        (if (and (= (car Color) (select_pixrgb_red (car List)))
                 (= (cadr Color) (select_pixrgb_green (car List)))
                 (= (caddr Color) (select_pixrgb_blue (car List))))
            (recCompressRGBList_newList (cdr List) Color (cons (list
                                                                (select_pixbit_x (car List))
                                                                (select_pixbit_y (car List))
                                                                (select_pixbit_depth (car List)))
                                                               AuxList))
            (recCompressRGBList_newList (cdr List) Color AuxList)))))

(define compressRGBList_newList
  (lambda (List Color)
    (recCompressRGBList_newList List Color '())))

(define recCompressRGBList_clear
  (lambda (List Color)
    (if (null? List)
        '()
        (if (and (= (car Color) (select_pixrgb_red (car List)))
                 (= (cadr Color) (select_pixrgb_green (car List)))
                 (= (caddr Color) (select_pixrgb_blue (car List))))
            (recCompressRGBList_clear (cdr List) Color)
            (cons (car List) (recCompressRGBList_clear (cdr List) Color))))))

(define compressRGBList_clear
  (lambda (List Color)
    (recCompressRGBList_clear List Color)))

(define compressPixmap
  (lambda (Image)
    (createCompressedImage Image
                           (cons (mostFrequentRGBHisto (histogram Image))
                                 (compressRGBList_newList (get_image_pixels Image)
                                                          (mostFrequentRGBHisto (histogram Image))))
                           (compressRGBList_clear (get_image_pixels Image)
                                                  (mostFrequentRGBHisto (histogram Image))))))

(define compressHexmap
  (lambda (Image)
    (compressPixmap (list (get_pix_x Image) (get_pix_y Image) (HEXList->RGBList (get_image_pixels Image))))))

(define compress
  (lambda (Image)
    (if (bitmap? Image)
        (compressBitmap Image)
        (if (pixmap? Image)
            (compressPixmap Image)
            (if (hexmap? Image)
                (compressHexmap Image)
                #f)))))

(define recEdit
  (lambda (function List)
    (if (null? List)
        '()
        (cons (function (car List)) (recEdit function (cdr List))))))

(define edit
  (lambda (function Image)
    (list (get_pix_x Image) (get_pix_y Image) (recEdit function (get_image_pixels Image)))))

