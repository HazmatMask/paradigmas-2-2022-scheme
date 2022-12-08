#lang racket

(require "TDA-pixbit.rkt")
(require "TDA-pixrgb.rkt")
(require "TDA-pixhex.rkt")
(require "TDA-image.rkt")

(image 1 1 '(0 0 "ABBCED" 4))
(image 2 2 '(0 0 0 4) '(1 0 1 4) '(0 1 0 4) '(1 1 0 4))
(image 2 2 '(0 0 10 20 30 4) '(1 0 20 30 40 4) '(0 1 30 40 50 4) '(1 1 40 50 60 4))

(bitmap? (image 1 1 '(0 0 "ABBCED" 4)))
(bitmap? (image 2 2 '(0 0 0 4) '(1 0 1 4) '(0 1 0 4) '(1 1 0 4)))
(bitmap? (image 2 2 '(0 0 10 20 30 4) '(1 0 20 30 40 4) '(0 1 30 40 50 4) '(1 1 40 50 60 4)))

(pixmap? (image 1 1 '(0 0 "ABBCED" 4)))
(pixmap? (image 2 2 '(0 0 0 4) '(1 0 1 4) '(0 1 0 4) '(1 1 0 4)))
(pixmap? (image 2 2 '(0 0 10 20 30 4) '(1 0 20 30 40 4) '(0 1 30 40 50 4) '(1 1 40 50 60 4)))

(hexmap? (image 1 1 '(0 0 "ABBCED" 4)))
(hexmap? (image 2 2 '(0 0 0 4) '(1 0 1 4) '(0 1 0 4) '(1 1 0 4)))
(hexmap? (image 2 2 '(0 0 10 20 30 4) '(1 0 20 30 40 4) '(0 1 30 40 50 4) '(1 1 40 50 60 4)))

(compressed? '(2 2 ((0 0 10 20 30 4) (1 0 20 30 40 4) (0 1 30 40 50 4) (1 1 40 50 60 4))))
(compressed? '(2 2 ((0 0 10 20 30 4) (1 0 20 30 40 4) (0 1 30 40 50 4)) (((40 50 60) (1 1 50)))))
(compressed? (image 1 1 '(0 0 "ABBCED" 4)))

(flipH (image 1 1 '(0 0 "ABBCED" 4)))
(flipH (image 2 2 '(0 0 0 4) '(1 0 1 4) '(0 1 0 4) '(1 1 0 4)))
(flipH (image 2 2 '(0 0 10 20 30 4) '(1 0 20 30 40 4) '(0 1 30 40 50 4) '(1 1 40 50 60 4)))

(flipV (image 1 1 '(0 0 "ABBCED" 4)))
(flipV (image 2 2 '(0 0 0 4) '(1 0 1 4) '(0 1 0 4) '(1 1 0 4)))
(flipV (image 2 2 '(0 0 10 20 30 4) '(1 0 20 30 40 4) '(0 1 30 40 50 4) '(1 1 40 50 60 4)))

(crop (image 1 1 '(0 0 "ABBCED" 4)) 0 0 1 1)
(crop (image 2 2 '(0 0 0 4) '(1 0 1 4) '(0 1 0 4) '(1 1 0 4)) 0 0 1 1)
(crop (image 2 2 '(0 0 10 20 30 4) '(1 0 20 30 40 4) '(0 1 30 40 50 4) '(1 1 40 50 60 4)) 0 0 1 1)

(imgRGB->imgHex (image 1 1 '(0 0 10 20 30 4)))
(imgRGB->imgHex (image 2 2 '(0 0 10 20 30 4) '(1 0 15 25 35 4) '(0 1 20 30 40 4) '(1 1 25 35 45 4)))
(imgRGB->imgHex (image 2 2 '(0 0 10 20 30 4) '(1 0 20 30 40 4) '(0 1 30 40 50 4) '(1 1 40 50 60 4)))

(histogram (image 2 2 '(0 0 1 4) '(1 0 1 4) '(0 1 0 4) '(1 1 1 4)))
(histogram (image 2 2 '(0 0 10 20 30 4) '(1 0 15 25 35 4) '(0 1 10 20 30 4) '(1 1 25 35 45 4)))
(histogram (image 2 2 '(0 0 "ABBCCD" 4) '(1 0 "ABBCCD" 4) '(0 1 "FBEEAC" 4) '(1 1 "ABCDEF" 4)))

(rotate90 (image 1 1 '(0 0 10 20 30 4) '(1 0 10 20 30 4)))
(rotate90 (image 2 2 '(0 0 10 20 30 4) '(1 0 15 25 35 4) '(0 1 20 30 40 4) '(1 1 25 35 45 4)))
(rotate90 (image 2 2 '(0 0 10 20 30 4) '(1 0 20 30 40 4) '(0 1 30 40 50 4) '(1 1 40 50 60 4)))

(compress (image 1 1 '(0 0 10 20 30 4) '(1 0 10 20 30 4)))
(compress (image 2 2 '(0 0 10 20 30 4) '(1 0 15 25 35 4) '(0 1 20 30 40 4) '(1 1 25 35 45 4)))
(compress (image 2 2 '(0 0 10 20 30 4) '(1 0 20 30 40 4) '(0 1 30 40 50 4) '(1 1 40 50 60 4)))

(edit invertColorBit (image 2 2 '(0 0 0 4) '(1 0 1 4) '(0 1 0 4) '(1 1 0 4)))
(edit select_pixbit_value (image 2 2 '(0 0 0 4) '(1 0 1 4) '(0 1 0 4) '(1 1 0 4)))
(edit invertColorRGB (image 2 2 '(0 0 10 20 30 4) '(1 0 20 30 40 4) '(0 1 30 40 50 4) '(1 1 40 50 60 4)))

(image->string (image 2 2 '(0 0 1 4) '(1 0 1 4) '(0 1 0 4) '(1 1 1 4)))
(image->string (image 2 2 '(0 0 10 20 30 4) '(1 0 15 25 35 4) '(0 1 10 20 30 4) '(1 1 25 35 45 4)))
(image->string (image 2 2 '(0 0 "ABBCCD" 4) '(1 0 "ABBCCD" 4) '(0 1 "FBEEAC" 4) '(1 1 "ABCDEF" 4)))

(imageDepthLayers (image 2 2 '(0 0 1 2) '(1 0 1 3) '(0 1 0 4) '(1 1 1 4)))
(imageDepthLayers (image 2 2 '(0 0 10 20 30 3) '(1 0 15 25 35 2) '(0 1 10 20 30 2) '(1 1 25 35 45 4)))
(imageDepthLayers (image 2 2 '(0 0 "ABBCCD" 4) '(1 0 "ABBCCD" 2) '(0 1 "FBEEAC" 1) '(1 1 "ABCDEF" 4)))