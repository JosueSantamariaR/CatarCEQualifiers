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
Def: Se crea un rectangulo de 1220 x 720 de color negro y se asigna la posicion 0,0.
Se coloca la imagen que refleja la cancha.
|#
(define (dibujarCancha)
  (begin
    ((draw-solid-rectangle mapaDePixeles) (make-posn 0 0) 1220 720 "black")
    ((draw-pixmap mapaDePixeles) "background.jpg" (make-posn 0 0)) 
    ))


#|

--------------------------------------Definicion para dibujar en la ventana----------------------------------------
Def: Se copia lo que hay en la ventana y se limpia el mapa de pixeles.
|#
(define (drawWindow)
  (begin
    (copy-viewport mapaDePixeles ventana)
    ((clear-viewport mapaDePixeles))
    )
  )



(dibujarCancha)
(drawWindow)


