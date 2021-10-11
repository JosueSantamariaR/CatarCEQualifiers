
#lang racket
(require racket/gui/base)

; Definir la imagen de fondo
(define the-bitmap
  (make-object bitmap% "background.jpg"))

(define frame (new frame%
                   [label "CatarCEQualifiers"]
                   [width 975]
                   [height 640]))
(new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 1 1)
                (send dc draw-bitmap the-bitmap 0 0)
                (send dc set-text-foreground "blue")
                (send dc draw-text "●" 0 0))
              ])

(send frame show #t)