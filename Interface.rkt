#lang racket/gui
(require graphics/graphics)
(require 2htdp/image)
(require racket/math)


;Se inicializa la libreria encargada del uso de la parte grafica.
(open-graphics)


;Se crea la ventana y el mapa de pixeles con el mismo tamaño de la ventana
(define mainWindow (open-viewport "CatarCEQualifiers" 1220 600))
(define pixMap (open-pixmap "CatarCEQualifiers" 1220 640))


#|

--------------------------------------Definicion para dibujar la cancha----------------------------------------
Def: Se crea un rectangulo de 1220 x 720 de color negro y se asigna la posicion 0,0.
Se coloca la imagen que refleja la cancha.
|#
(define (drawField)
  (begin
    ((draw-solid-rectangle pixMap) (make-posn 0 0) 1220 720 "white")
    ((draw-pixmap pixMap) "background.jpg" (make-posn 0 0)) 
    ))


#|

--------------------------------------Definicion para dibujar en la ventana----------------------------------------
Def: Se copia lo que hay en la ventana y se limpia el mapa de pixeles.
|#
(define (drawWindow)
  (begin
    (copy-viewport pixMap mainWindow)
    ((clear-viewport pixMap))
    )
  )
#|

--------------------------------------Definiciones de los getters----------------------------------------
Def: Se definen los getters que se utilizan para los jugadores
|#


(define (getXY player);Obtener posición inicial del jugador.
  (cond((or (null? player) (number? player)) '(50 50))
       (else
        (car player))))

(define (getFinaXY player);Obtener posición final del jugador.
  (cond((null? player) '())
       (else
        (cadr player))))

(define (posX player);Obtener posición inicial en coordena X del jugador.
  (car (getXY player)))

(define (posY player);Obtener posición inicial en coordena Y del jugador.
  (cadr (getXY player)))

(define (finalX player);Obtener posición final en coordena X del jugador.
  (car (getFinaXY player)))

(define (finalY player);Obtener posición final en coordena Y del jugador.
  (cadr (getFinaXY player)))

(define (getNextPlayer players);Obtener siguiente jugador de la lista.
  (cond ((null? players) '())
        (else (car players))
        ))

(define (getPlayerNumber player);Obtener el numero para cada jugador de la lista.
  (cond((null? player) '())
       (else
        (getPlayerNumber_aux player 0)
  )))
(define (getPlayerNumber_aux player number)
  (cond((equal? number 4) (car player))
       (else
        (getPlayerNumber_aux (cdr player) (+ number 1)))))


#|

--------------------------------------Definicion para dibujar en la ventana----------------------------------------
Def: Se copia lo que hay en la ventana y se limpia el mapa de pixeles.
|#

(define (drawTeam players number)  
  (cond ((null? players) #t)
        (else (begin
                ((draw-solid-ellipse pixMap)
                 (make-posn (posX (car players))
                            (posY (car players)))30 30
                            (getTeam (car players)))
                ((draw-string pixMap)
                 (make-posn (+ (caaar players) 13)
                            (+ (cadar (car players)) 15))
                 (number->string (getPlayerNumber
                                  (car players))) "white")
                (drawTeam (cdr players)
                                  (+ 1 number))
                ))))
#|

--------------------------------------Definicion para dibujar en la ventana----------------------------------------
Def: Se copia lo que hay en la ventana y se limpia el mapa de pixeles.
|#
(define (getTeam player)
  (caddr (cdddr player)))
(drawTeam)
(drawField)
(drawWindow)


