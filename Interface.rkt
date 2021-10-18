
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
    ((draw-solid-rectangle mapaDePixeles) (make-posn 0 0) 920 540 "Cyan") ;;Cesped
    ((draw-solid-rectangle mapaDePixeles) (make-posn 0 0) 5 540 "black")   ;;Raya vertical
    ((draw-solid-rectangle mapaDePixeles) (make-posn 915 0) 5 540 "black") ;;Raya vertical
    ((draw-solid-rectangle mapaDePixeles) (make-posn 0 0) 920 5 "black")   ;;Raya horizontal
    ((draw-solid-rectangle mapaDePixeles) (make-posn 0 535) 920 5 "black") ;;Raya horizontal
    ((draw-solid-rectangle mapaDePixeles) (make-posn 457 0) 5 540 "black") ;;Raya medio campo
    ((draw-solid-ellipse mapaDePixeles) (make-posn 452 262) 15 15 "black") ;;Circulo central
    ((flip-ellipse mapaDePixeles) (make-posn 385 195) 150 150 "black")     ;;Circulo grande central
    ((flip-rectangle mapaDePixeles) (make-posn -90 180) 180 180 "black")   ;;Marco izquierdo
    ((flip-rectangle mapaDePixeles) (make-posn -90 100) 350 350 "black")   ;;Area grande
    ((flip-rectangle mapaDePixeles) (make-posn 830 180) 90 180 "black")   ;;Marco derecho
    ((flip-rectangle mapaDePixeles) (make-posn 650 100) 350 350 "black")   ;;Area grande
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


