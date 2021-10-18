
#lang racket
(require graphics/graphics)
(require 2htdp/image)
(require racket/math)


;Se inicializa la libreria encargada del uso de la parte grafica.
(open-graphics)


;Se crea la ventana y el mapa de pixeles con el mismo tamaño de la ventana
(define ventana (open-viewport "CatarCEQualifiers" 1220 530))
(define mapaDePixeles (open-pixmap "CatarCEQualifiers" 1220 530))


#|

--------------------------------------Definicion para dibujar la cancha----------------------------------------

Se dibuja cada parte de la cancha utilizando rectangulos y elipses
|#
(define (dibujarCancha)
  (begin
    ((draw-solid-rectangle mapaDePixeles) (make-posn 0 0) 1220 720 "black") ;;Fondo
    ((draw-pixmap mapaDePixeles) "background.jpg" (make-posn 0 0))
   
   
    ))

;;Borra la pantalla 2 y pega la pantalla 2 en la principal.
;;E: No tiene.
;;S: Borra la pantalla y la pega en la principal.
(define (drawWindow)
  (begin
    (copy-viewport mapaDePixeles ventana)
    ((clear-viewport mapaDePixeles))
    )
  )



(dibujarCancha)
(drawWindow)


