#lang racket/gui
(require graphics/graphics)
(require 2htdp/image)
(require racket/math)


;Se inicializa la libreria encargada del uso de la parte grafica.
(open-graphics)


(define (move listTGrande)
  (move-aux (cadr (divideteams (car listTGrande))) (car (divideteams (car listTGrande))) (cadr listTGrande))
  )

(define (move-aux playeres playeres_S ball)
  (cond((or (null? playeres) (null? playeres)) '())
       (else
        (list (move_playeres playeres '() ball) (move_playeres_S playeres_S '() ball)))))

(define (move_playeres_S playeres listF ball)
  (cond ((null? playeres) listF)
        (else
         (move_playeres_S (cdr playeres) (append listF (list (move_to_ball_S (car playeres) listF  ball))) ball))))


(define(check_Type_S player posXYf)
  (cond((equal? (getNumType player) 1) (moveGoalkeeper_S player posXYf))
       ((equal? (getNumType player) 2) (moveDefense_S player posXYf))
       ((equal? (getNumType player) 3) (moveMidfielder_S player posXYf))
       ((equal? (getNumType player) 4) (moveForward_S player posXYf))))

(define (moveGoalkeeper_S player posXYf)
  (cond ((not (equal? (car posXYf) 895)) (moveGoalkeeper_S player (cons 895 (cdr posXYf))))
        ((< (cadr posXYf) 180) (moveGoalkeeper_S player (list 895 180)))
        ((> (cadr posXYf) 330) (moveGoalkeeper_S player (list 895 330)))
        (else
         (append (list (car player)) (list posXYf) (cddr player)))))

(define (moveForward_S player posXYf)
  
  (cond 
    ((> (car posXYf) 230) (moveForward_S player (list 230 (cadr posXYf))))
        (else
         {append (list (car player)) (list posXYf) (cddr player)})))


(define (moveMidfielder_S player posXYf)
  (cond ((< (car posXYf) 230) (moveMidfielder_S player (list 230 (cadr posXYf))))
        ((> (car posXYf) 690) (moveMidfielder_S player (list 690 (cadr posXYf))))
        (else
         {append (list (car player)) (list posXYf) (cddr player)})))


(define (moveDefense_S player posXYf)
  (cond ((< (car posXYf) 690) (moveDefense_S player (list 690 (cadr posXYf))) )
        ((> (car posXYf) 900) (moveDefense_S player (list 900 (cadr posXYf))))
        (else
         {append (list (car player)) (list posXYf) (cddr player)})))



(define (move_to_ball_S  player listF ball)
 (cond ((and (<= (abs (- (car(getXY player)) (caar ball))) (+ (* (getAgility player) 15) 150))
             (<= (abs (- (cadr(getXY player)) (cadar ball))) (+ (* (getAgility player) 15) 150))
             (check_other_player (car ball) listF))
        (check_Type_S player  (car ball)))
        
       ((equal? (getNumType player) 1)
        (moveGoalkeeper_S player (list 895 (+ (random 150) 180) )))
       ((equal? (getNumType player) 4)
        (moveForward_S player (list  (+ (random 225) 5) (+ (random 505) 5) )))
       
       ((equal? (getNumType player) 3)
        (moveMidfielder_S player (list (+ (random 460) 230) (+ (random 505) 5) )))
       ((equal? (getNumType player) 2)
        (moveDefense_S player (list (+ (random 210) 690) (+ (random 505) 5) )))
         
       ))


(define (move_playeres playeres listF ball)
  (cond ((null? playeres) listF)
        (else
         (move_playeres (cdr playeres) (append listF (list (move_to_ball (car playeres) listF ball))) ball))))


(define(check_Type player posXYf )
  
  (cond((equal? (getNumType player) 1) (moveGoalkeeper player posXYf))
       ((equal? (getNumType player) 2) (moveDefense player posXYf))
       ((equal? (getNumType player) 3) (moveMidfielder player posXYf))
       ((equal? (getNumType player) 4) (moveForward player posXYf))))
       

(define (moveGoalkeeper player posXYf)
  (cond ((not (equal? (car posXYf) 5)) (moveGoalkeeper player (cons 5(cdr posXYf))))
        ((< (cadr posXYf) 180) (moveGoalkeeper player (list 5 180)))
        ((> (cadr posXYf) 330) (moveGoalkeeper player (list 5 330)))
        (else
         (append (list (car player)) (list posXYf) (cddr player)))))


(define (moveDefense player posXYf)
  
  (cond 
    ((> (car posXYf) 230) (moveDefense player (list 230 (cadr posXYf))))
        (else
         {append (list (car player)) (list posXYf) (cddr player)})))


(define (moveMidfielder player posXYf)
  (cond ((< (car posXYf) 230) (moveMidfielder player (list 230 (cadr posXYf))))
        ((> (car posXYf) 690) (moveMidfielder player (list 690 (cadr posXYf))))
        (else
         {append (list (car player)) (list posXYf) (cddr player)})))


(define (moveForward player posXYf)
  (cond ((< (car posXYf) 690) (moveMidfielder player (list 690 (cadr posXYf))) )
        ((> (car posXYf) 900) (moveMidfielder player (list 900 (cadr posXYf))))
        (else
         {append (list (car player)) (list posXYf) (cddr player)})))



(define (move_to_ball  player listF ball)
 (cond ((and (<= (abs (- (car(getXY player)) (caar ball))) (+ (* (getAgility player) 15) 150))
             (<= (abs (- (cadr(getXY player)) (cadar ball))) (+ (* (getAgility player) 15) 150))
             (check_other_player (car ball) listF))
        (check_Type player  (car ball)))
        
       ((equal? (getNumType player) 1)
        (moveGoalkeeper player (list 5 (+ (random 150) 180) )))
       ((equal? (getNumType player) 2)
        (moveDefense player (list  (+ (random 225) 5) (+ (random 505) 5) )))
         
       ((equal? (getNumType player) 3)
        (moveMidfielder player (list (+ (random 460) 230) (+ (random 505) 5) )))
       ((equal? (getNumType player) 4)
        (moveForward player (list (+ (random 210) 690) (+ (random 505) 5) )))
       ))

        
(define (check_other_player posXYf listF)
  (cond((null? listF) #t)
       ((equal? (getXYf (car listF)) posXYf) #f)
       (else
       
        (check_other_player posXYf (cdr listF)))))


(define (getXY player)
  (cond((or (null? player) (number? player)) '(50 50))
       (else
        (car player))))


(define (getXYf player)
  (cond((null? player) '())
       (else
        (cadr player))))

(define (getEst player)
  (cond ((null? player)
         '())
        (else
         (getEst_aux player 0))))
(define (getEst_aux player num)
  (cond((equal? num 2) (car player))
       (else
        (getEst_aux (cdr player) (+ num 1)))))

(define (getfitness player)
  (cond((null? player) '())
       (else
        (car (getEst player))
  )))


(define (getStrength player)
  (cond((null? player) '())
       (else
        (getStrength_aux (getEst player) 0)
  )))
(define (getStrength_aux estadistica num)
  (cond((equal? num 1) (car estadistica))
       (else
        (getStrength_aux (cdr estadistica) (+ num 1)))))


(define (getAim player)
  (cond((null? player) '())
       (else
        (getAim_aux (getEst player) 0)
  )))
(define (getAim_aux estadistica num)
  (cond((equal? num 2) (car estadistica))
       (else
        (getAim_aux (cdr estadistica) (+ num 1)))))


(define (getSpeed player)
  (cond((null? player) '())
       (else
        (getSpeed_aux (getEst player) 0)
  )))
(define (getSpeed_aux estadistica num)
  (cond((equal? num 3) (car estadistica))
       (else
        (getSpeed_aux (cdr estadistica) (+ num 1)))))


(define (getAgility player)
  (cond((null? player) '())
       (else
        (getAgility_aux (getEst player) 0)
  )))
(define (getAgility_aux estadistica num)
  (cond((equal? num 4) (car estadistica))
       (else
        (getAgility_aux (cdr estadistica) (+ num 1)))))


(define (getNumType player)
  (cond((null? player) '())
       (else
        (getNumType_aux player 0)
  )))

(define (getNumType_aux player num)
  (cond((equal? num 3) (car player))
       (else
        (getNumType_aux (cdr player) (+ num 1)))))

(define (getNum player)
  (cond((null? player) '())
       (else
        (getNum_aux player 0)
  )))

(define (getNum_aux player num)
  (cond((equal? num 4) (car player))
       (else
        (getNum_aux (cdr player) (+ num 1)))))


(define (divideteamsAux playeres num new)
  (cond ((> num 10) (list new playeres))
        (else (divideteamsAux (cdr playeres) (+ num 1) (cons (car playeres) new)) ))
  )

(define (divideteams playeres)
  (begin
    (divideteamsAux playeres 0 '() )
    ))

(define (directionShoot-aux player marco)
  
    (cond ((null? player)
         '())
        (else
          (+ (* (/ (- (getAim player) 10) -0.2222) (expt -1 (random 2))) marco)  )
        )
    )

(define (directionShoot player)
  (cond ((equal? (getteam player) "blue")
         (directionShoot-aux player (getDirection (list (getXY player) '(0 250)))))
        (else
         (directionShoot-aux player (getDirection (list (getXY player) '(920 250))))
         ))
  )


#|
-------------------------------------Aptitud--------------------------------------
Def: Se 

|#

(define (fitness-eq team)
  (cond((null? team) team)
       (else( cons (fitness (car team)) (fitness-eq (cdr team))))))

(define (fitness player)
  (cond((null? player) player)
       (else (list(car player) (cadr player) (fitness-aux (caddr player)) (car(cdddr player)) (cadr(cdddr player))))))

(define (fitness-aux skills)
  (cond((null? skills) skills)
       (else(cons (suma (cdr skills)) (cdr skills)))))

(define(suma listT)
  (cond((null? listT) 0)
       (else(+ (car listT) (suma (cdr listT))))))


#|
-------------------------------------Aux-Formacion--------------------------------------
Def: 

|#

(define (getFormacion playeres  num1 num2 num3)
  (cond((null? playeres) (list num1 num2 num3))
       ((equal? (getNumType (car playeres)) 2) (getFormacion (cdr playeres) (+ num1 1) num2 num3))
       ((equal? (getNumType (car playeres)) 3) (getFormacion (cdr playeres) num1 (+ num2 1) num3))
       ((equal? (getNumType (car playeres)) 4) (getFormacion(cdr playeres)  num1 num2 (+ num3 1)))
       (else
        (getFormacion (cdr playeres) num1 num2 num3))))


#|
-------------------------------------Seleccion--------------------------------------
Def: 

|#

(define (seleccion-reproduction-aux teams)
  (append (seleccion-reproduction (getFormacion (car teams) 0 0 0) (car teams))
          (seleccion-reproduction (getFormacion (cadr teams) 0 0 0) (cadr teams)))
  )

(define (seleccion-reproduction formacion team)
  (cond((null? team) team)
       (else
        (asignar-caracts
         (mutation
          (reproduction
           (crear-hijos (+ 1 (car formacion) (cadr formacion) (caddr formacion)) (mejores(retornar-caracts team)))))
         team))))

;;3
(define (crear-hijos num padres)
  (cond((even? num) (append (crearhijos-aux (quotient num 2) (car padres)) (crearhijos-aux (quotient num 2) (cadr padres))))
       (else(append (crearhijos-aux (+ 1 (quotient num 2)) (car padres)) (crearhijos-aux (quotient num 2) (cadr padres))))))

;;3.1
(define(crearhijos-aux num padre)
  (cond((= num 1) (list padre))
       (else (append (list padre) (crearhijos-aux (- num 1) padre)))))

;;1
(define (retornar-caracts team)
  (cond((null? team) team)
       (else(cons (caddr(car team)) (retornar-caracts (cdr team))))))
;;2
(define (mejores listT)
  (cond((null? listT) listT)
       (else
        (list (mayor (car listT) (cdr listT))
              (mayor (car(eliminar (mayor (car listT) (cdr listT)) listT))
                     (eliminar (mayor (car listT) (cdr listT)) listT))))))
;;2.1
(define (mayor actual listT)
  (cond((null? listT) actual)
       ((> (car actual) (caar listT)) (mayor actual (cdr listT)))
       ((< (car actual) (caar listT)) (mayor (car listT) (cdr listT)))
       (else(mayor actual (cdr listT)))))
;;2.2
(define(eliminar num listT)
  (cond((null? listT) (list))
       ((equal? (car num) (caar listT)) (cdr listT))
       (else (cons (car listT) (eliminar num (cdr listT))))))

;Funciones extra para la seleccion----------------------------
(define(asignar-caracts carac team)
  (cond((null? team) '())
       (else (cons (asignar-aux (car carac) (car team)) (asignar-caracts (cdr carac) (cdr team))))))

(define(asignar-aux hab player)
  (cond((or (null? hab) (null? player)) player)
       (else (list(car player) (cadr player) hab (car(cdddr player)) (cadr(cdddr player)) (getteam player)))))

#|
-------------------------------------Reproduccion--------------------------------------
Def: Obtiene los jugadores y de cada uno de ellos se obtiene las habilidades que se generaron en random
     estas habilidades se utilizan para crear nuevas e ir combinandolas entre el ultimo y el primero
|#

(define(reproduction listT)
  (cond((null? listT) listT)
       ((null? (cdr listT)) listT)
       (else(append (combinar (car listT) (ultimo (car listT) listT)) (reproduction (sin-ultimo(cdr listT)))))))

(define(combinar skills1 skills2)
  (cond((or (null? skills1) (null? skills2)) (list skills1 skills2))
       (else(cons (combinar-aux (+ 1 (random 5)) skills1 skills2) (list (combinar-aux (+ 1 (random 5)) skills1 skills2))))))

(define(combinar-aux num hab1 hab2)
  (cond((or (null? hab1) (= num 0)) hab2)
       (else(cons (car hab1) (combinar-aux (- num 1) (cdr hab1) (cdr hab2))))))

(define(ultimo actual listT)
  (cond((null? listT) actual)
       (else (ultimo (car listT) (cdr listT)))))

(define(sin-ultimo listT)
  (cond((null? (cdr listT)) '())
       (else(cons (car listT) (sin-ultimo (cdr listT))))))


#|
-------------------------------------Mutacion------------------------------------
Def: Comienza a tomar las habilidades del jugador para posteriormente con un random
     hacer diferentes operaciones y obtener un nuevo jugador con estos cambios.
|#

(define(mutation skills)
  (cond((null? skills) skills)
       (else(cons (mutation-aux (car skills)) (mutation (cdr skills))))))

(define (mutation-aux hab)
  (cond((< (random 100) 30) (mutar (+ 1 (random 4)) hab))
       (else hab)))

(define(mutar corte hab)
  (cond((zero? corte) (cons (random 11) (cdr hab)))
       (else (cons (car hab) (mutar (- corte 1) (cdr hab))))))


#|
------------------------------------Alinea los equipos------------------------------------
Def: Esta funcion se encarga de alinear cada jugador en los equipos, asi como segun su tipo
     lo posiciones donde debe de ir
|#

(define(alinear teams)
  (cond((null? teams) teams)
       (else (append (alinear2 (cadr teams)) (alinear1 (car teams))))))

(define(alinear1 team)
  (cond((null? team) team)
       (else (cons (alinear-player1 (car team)) (alinear1 (cdr team))))))

(define(alinear2 team)
  (cond((null? team) team)
       (else (cons (alinear-player2 (car team)) (alinear2 (cdr team))))))

(define(alinear-player1 player)
  (cond((null? player) player)
       ((equal? 1 (car(cdddr player)))
        (list (list 5 (+ 180 (random 180))) (list 0 0) (caddr player) 1 (cadr(cdddr player)) "red"))
       ((equal? 2 (car(cdddr player)))
        (list (list (+ 90 (random 50)) (+ 5 (random 510))) (list 0 0) (caddr player) 2 (cadr(cdddr player)) "red"))
       ((equal? 3 (car(cdddr player)))
        (list (list (+ 260 (random 50)) (+ 5 (random 510))) (list 0 0) (caddr player) 3 (cadr(cdddr player)) "red"))
       (else
        (list (list (+ 400 (random 50)) (+ 5 (random 510))) (list 0 0) (caddr player) 4 (cadr(cdddr player)) "red"))))

(define(alinear-player2 player)
  (cond((null? player) player)
       ((equal? 1 (car(cdddr player)))
        (list (list 895 (+ 180 (random 180))) (list 0 0) (caddr player) 1 (cadr(cdddr player)) "blue"))
       ((equal? 2 (car(cdddr player)))
        (list (list (+ 750 (random 50)) (+ 5 (random 510))) (list 0 0) (caddr player) 2 (cadr(cdddr player)) "blue"))
       ((equal? 3 (car(cdddr player)))
        (list (list (+ 600 (random 50)) (+ 5 (random 510))) (list 0 0) (caddr player) 3 (cadr(cdddr player)) "blue"))
       (else
        (list (list (+ 465 (random 50)) (+ 5 (random 510))) (list 0 0) (caddr player) 4 (cadr(cdddr player)) "blue"))))

#|
------------------------------------Primera generacion para ambos teams------------------------------------
Def: Estas funciones se encargan de crear cada player con sus posicion, equipo y skills.
     Tambien se encargan de crear el equipo segun la formacion requerida por el usuario.
     Se generan por separado cada primera generacion de cada equipo, las skills son random.
|#

;-----------------------Crea skills, posicion y skills random------------------------

(define (player-1er Type num)
  (cond((equal? 1 Type)
        (list (list 5 (+ 180 (random 180))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 1 num "red"))
       ((equal? 2 Type)
        (list (list (+ 90 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 2 num "red"))
       ((equal? 3 Type)
        (list (list (+ 260 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 3 num "red"))
       (else
        (list (list (+ 400 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 4 num "red"))
       ))

(define (player-2do Type num)
  (cond ((equal? 1 Type)
         (list (list 895 (+ 180 (random 180))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 1 num "blue"))
        ((equal? 2 Type)
         (list (list (+ 750 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 2 num "blue"))
        ((equal? 3 Type)
         (list (list (+ 600 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 3 num "blue"))
        (else
         (list (list (+ 465 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 4 num "blue"))
        ))

;--------------------------Verifica formacion y le pone un numero al player---------------------

(define (inic-aux1 num formacion)
  (cond((zero? num) '())
       ((equal? 1 num)
        (cons (player-1er 1 1) (inic-aux1 (- num 1) formacion)))
       ((and (> num 1) (< num (+ 2 (car formacion))))
        (cons (player-1er 2 num) (inic-aux1 (- num 1) formacion)))
       ((and (> num (+ 1 (car formacion))) (< num (+ 2 (car formacion) (cadr formacion))))
        (cons (player-1er 3 num) (inic-aux1 (- num 1) formacion)))
       (else
        (cons (player-1er 4 num) (inic-aux1 (- num 1) formacion)))
       ))

(define (inic-aux2 num formacion)
  (cond ((zero? num) '())
        ((equal? 1 num)
         (cons (player-2do 1 1) (inic-aux2 (- num 1) formacion)))
        ((and (> num 1) (< num (+ 2 (car formacion))))
         (cons (player-2do 2 num) (inic-aux2 (- num 1) formacion)))
        ((and (> num (+ 1 (car formacion))) (< num (+ 2 (car formacion) (cadr formacion))))
         (cons (player-2do 3 num) (inic-aux2 (- num 1) formacion)))
        (else
         (cons (player-2do 4 num) (inic-aux2 (- num 1) formacion)))
        ))

;--------------------------Obtiene la formacion para ir creando playeres---------------------------

(define (inicialitation1 formacion)
  (cond ((not(list? formacion)) '())
        (else
         (inic-aux1 (+ 1 (car formacion) (cadr formacion) (caddr formacion)) formacion))
        ))

(define (inicialitation2 formacion)
  (cond ((not(list? formacion)) '())
        (else
         (inic-aux2 (+ 1 (car formacion) (cadr formacion) (caddr formacion)) formacion))
        ))

#|
--------------------------------------Creacion de las ventanas----------------------------------------
Def: Se definen las funciones para crear una ventana de tipo viewport y una ventana de tipo pixmap, con
las mismas dimensiones.
|#

(define mainWindow (open-viewport "CatarCEQualifiers" 1220 600))
(define pixMap (open-pixmap "CatarCEQualifiers" 1220 640))



#|
--------------------------------------Definicion de elementos iniciales----------------------------------------
Def: Se crea la primera bola con la fuerza, distancia, direccion
|#
(define firstBall '((450 260) 0 45))



#|
--------------------------------------Definicion de la funcion Write----------------------------------------
Def: Esta definicion nos permite escribir en la ventana de pixMap lo que son los marcadores y la generacion
del algoritmo genetico.
|#
(define (writeStrings points generacion)
  (begin
    ((draw-string pixMap) (make-posn 1050 30) "Current Score: " "cyan")
    ((draw-string pixMap) (make-posn 990 60) (number->string (car points)) "cyan")
    ((draw-string pixMap) (make-posn 1190 60) (number->string (cadr points)) "cyan")
    ((draw-string pixMap) (make-posn 1020 150) "Generation: " "cyan")
    ((draw-string pixMap) (make-posn 1150 150) (number->string generacion) "cyan")
    ))




#|
--------------------------------------Definicion para dibujar la cancha----------------------------------------
Def: Se crea un rectangulo de 1220 x 720 de color negro y se asigna la posicion 0,0.
Se coloca la imagen que refleja la cancha.
|#
(define (drawField)
  (begin
    ((draw-solid-rectangle pixMap) (make-posn 0 0) 1220 720 "black")
    ((draw-pixmap pixMap) "background.jpg" (make-posn 0 0)) 
    ))

(define (winner1)
  (begin
    ((draw-solid-rectangle pixMap) (make-posn 0 0) 1220 720 "black")
    (((draw-pixmap-posn "jugador1gana.png") pixMap) (make-posn 290 220)) 
    ))

(define (winner2)
  (begin
    ((draw-solid-rectangle pixMap) (make-posn 0 0) 1220 720 "black")
    (((draw-pixmap-posn "jugador2gana.png") pixMap) (make-posn 290 220))
    
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
Def: Se definen los getters que se utilizan para obtener la posicion inicial,final,movimiento final, la
hipotenusa
|#

(define (posX player)
  (car (getXY player)))

(define (posY player)
  (cadr (getXY player)))

(define (destinationX player)
  (car (getXYf player)))

(define (destinationY player)
  (cadr (getXYf player)))

(define (getteam player)
  (caddr (cdddr player)))

; Se utilizan las posiciones finales del jugadores para obtener el movimiento final
(define (changeMovement player)
  (cons (list (destinationX player) (destinationY player)) (cdr player)))

;Se obtiene la hipotenusa para calcular el movimiento.
(define (hypotenuse player)
  (cond ((null? player) 0)
        (else (sqrt (+ (expt (- (destinationX player) (posX player)) 2)
          (expt (- (destinationY player) (posY player)) 2))))
        ))

;Definicion para el movimiento del jugador en X.
(define (movementX playeres step diagonal direction)
  (cond ((null? playeres) 0)
        (else (+ (posX (car playeres)) (* step (cos (degrees->radians direction))))))
  )

;Definicion para el movimiento del jugador en Y.
(define (movementY playeres step diagonal direction)
  (cond ((null? playeres) 0)
        (else (+ (posY (car playeres)) (* step (sin (degrees->radians direction))))))
  )


;Definicion de la funcion para obtener el jugador siguiente
(define (getNextPlayer playeres)
  (cond ((null? playeres) '())
        (else (car playeres))
        ))


;Pendiente del jugador para la obtencion de los angulos
(define (slope player)
  (cond ((zero? (- (destinationX player) (posX player)))
         (/ (- (destinationY player) (posY player)) 0.5))
        (else (/ (- (destinationY player) (posY player)) (- (destinationX player) (posX player))))
        )
  )

;Funcion Auxiliar para el angulo.
(define (getDirectionAux player)
  (list (- (destinationY player) (posY player)) (- (destinationX player) (posX player))))


;Funcion Auxiliar para el angulo.
(define (getDirection player)
  (cond
    ((null? player) 0)
    ((and (< (car (getDirectionAux player)) 0) (< (cadr (getDirectionAux player)) 0))
     (- (radians->degrees (atan (slope player))) 180)
     )
    ((and (> (car (getDirectionAux player)) 0) (< (cadr (getDirectionAux player)) 0))
     (+ (radians->degrees (atan (slope player))) 180)
     )
    (else (radians->degrees (atan (slope player))))
    )
  )
;Obtener el numero para cada jugador de la lista.
(define (getPlayerNumber player)
  (cond((null? player) '())
       (else
        (getPlayerNumber_aux player 0)
  )))


;Obtener el numero de jugador.
(define (getPlayerNumber_aux player number)
  (cond((equal? number 4) (car player))
       (else
        (getPlayerNumber_aux (cdr player) (+ number 1)))))


#|
--------------------------------------Funcion Draw----------------------------------------
Def: Se define la funcion que nos permite dibujar los jugadores y el numero correspondiente a de cada
jugador.
|#

(define (drawplayeres playeres number)  
  (cond ((null? playeres) #t)
        (else (begin
              (cond 
              ((equal? (getteam (car playeres)) "blue")
                (begin
                    (((draw-pixmap-posn "player2.png") pixMap) (make-posn (posX (car playeres)) (posY (car playeres))) (getteam (car playeres)))
                    ((draw-string pixMap) (make-posn (+ (caaar playeres) 10) (+ (cadar (car playeres)) 10)) (number->string (getNum (car playeres))) "cyan")
                    (drawplayeres (cdr playeres) (+ 1 number))
                  ))
              (else
                (begin
                    (((draw-pixmap-posn "player1.png") pixMap) (make-posn (posX (car playeres)) (posY (car playeres))) (getteam (car playeres)))
                    ((draw-string pixMap) (make-posn (+ (caaar playeres) 10) (+ (cadar (car playeres)) 10)) (number->string (getNum (car playeres))) "cyan")
                    (drawplayeres (cdr playeres) (+ 1 number))
                  ))
                )
                ))))

(define (interseccionAux playeres ball)
  (cond ((null? playeres) ball)
        ((and (< (posX (car playeres)) (+ (posX ball) 20)) (< (posY (car playeres)) (+ (posY ball) 20))
              (> (+ (posX (car playeres)) 30) (posX ball)) (> (+ (posY (car playeres)) 30) (posY ball)))  
                                                        ;;Hay va el direction del player
         (list (car ball) (+ 10 (getStrength (car playeres))) (directionShoot (car playeres)))
         )
        (else (interseccionAux (cdr playeres) ball)) 
        )
  )

(define (interseccion player xm ym ball playeres)
  (cond ((and (< xm (+ (posX ball) 20)) (< ym (+ (posY ball) 20)) (> (+ xm 30) (posX ball)) (> (+ ym 30) (posY ball)))
                                                       ;;Hay va el direction del player 
         (list (car ball) (+ 10 (getStrength player)) (directionShoot player))
         )
        (else (interseccionAux playeres ball))
        ))

(define (StrengthBall ball)
  (cond ((not (list? ball)) 0)
        (else (cadr ball))))

(define (directionBall ball)
  (cond ((number? ball) ball)
        (else (caddr ball)))
  )

(define (verificaLimites ball)
  (cond ((< (posY ball) 1)
         (list (list (posX ball) 1) (StrengthBall ball) (+ (directionBall ball) 240)))
        ((> (posY ball) 519)
         (list (list (posX ball) 519) (StrengthBall ball) (- (directionBall ball) 240)))
        ((and (< (posX ball) 1) (or (< (posY ball) 165) (> (posY ball) 360)))
         (list (list 1 (posY ball)) (StrengthBall ball) (+ (directionBall ball) 240)))
        ((and (> (posX ball) 889) (or (< (posY ball) 165) (> (posY ball) 360)))
         (list (list 889 (posY ball)) (StrengthBall ball) (+ (directionBall ball) 240)))
        (else ball)
    ))

(define (verificarGol ball points playeres generaciones iteraciones)
  (cond ((and (> (posX ball) 900) (or (> (posY ball) 165) (< (posY ball) 360)))
         (playGameGol playeres generaciones (list (+ (car points) 1) (cadr points)) iteraciones))
        ((and (< (posX ball) -5) (or (> (posY ball) 165) (< (posY ball) 360)))
         (playGameGol playeres generaciones (list (car points) (+ (cadr points) 1)) iteraciones))
        (else points)
        )
  )

(define (cambiarStrengthBall ball)
    (cond ((> (StrengthBall ball) 0)
                                              ;;Duracion del Shoot
           (verificaLimites (list
                             (list (+ (posX ball) (* (StrengthBall ball) (cos (degrees->radians (directionBall ball)))))
                                   (+ (posY ball) (* (StrengthBall ball) (sin (degrees->radians (directionBall ball))))))
                             (- (StrengthBall ball) 0.3) (directionBall ball))))
          ((number? ball) (begin
                            (display "coso: ")
                            (display ball) (newline)
                            (cambiarStrengthBall (list (list 450 260) -1 0))
                            ))
          (else (list (car ball) (StrengthBall ball) (directionBall ball)))
          ))


(define (drawBall ball)
  (cond ((or (< (StrengthBall ball) 0) (zero? (StrengthBall ball)))
         (begin
           
           (((draw-pixmap-posn "sprball.png") pixMap) (make-posn (posX ball) (posY ball)))
           )
         )
        (else
         (begin
           (((draw-pixmap-posn "sprball.png") pixMap) (make-posn (posX ball) (posY ball)))
           )
        )
))

(define (remplazarDestino playeres new ball)
  (cond ((null? playeres) '())
        ((equal? (getteam (car playeres)) "blue")
         (append (cons (move_to_ball_S (car playeres) new ball) '()) (cdr playeres)))
        (else
         (append (cons (move_to_ball (car playeres) new ball) '()) (cdr playeres)))
        ))

(define (getResto playeres)
  (cond ((null? playeres) '())
        (else (cdr playeres))
        ))

(define (estela-aux playeres step newplayeres diagonal direction ball points generaciones iteraciones)
  (estela playeres 0
          newplayeres
          (hypotenuse (getNextPlayer playeres))
          (getDirection (getNextPlayer playeres))
          (cambiarStrengthBall (interseccion
                              (getNextPlayer playeres)
                              (movementX playeres step diagonal direction)
                              (movementY playeres step diagonal direction)
                              ball (append newplayeres (getResto playeres))))
          points generaciones iteraciones))



(define (estela playeres step newplayeres diagonal direction ball points generaciones iteraciones)
  (begin
    (drawField)
    (writeStrings points generaciones)
    (cond ((null? playeres)
           (cond
             ((not (< (StrengthBall ball) 0))
               (begin
                 (drawplayeres  newplayeres 1)
                 (drawBall (interseccionAux newplayeres ball))
                 (drawWindow)
                 (estela '() 0 newplayeres 0 0
                                 (cambiarStrengthBall (StrengthBall (interseccionAux newplayeres ball)))
                                 (verificarGol ball points (append newplayeres playeres) generaciones iteraciones) generaciones iteraciones)))
             (else (list newplayeres ball))))
        ((< diagonal step)
         (estela-aux
          (remplazarDestino (cdr playeres) newplayeres ball) 0
          (cons (changeMovement (car playeres)) newplayeres)
          diagonal direction ball (verificarGol ball points (append newplayeres playeres) generaciones iteraciones) generaciones iteraciones))        
        (else
         (begin
            (cond 
              ((equal? (getteam (car playeres)) "blue")
                (begin
                  (((draw-pixmap-posn "player2.png") pixMap)  (make-posn (movementX playeres step diagonal direction)
                                                              (movementY playeres step diagonal direction))
                   (getteam (car playeres)))))
                   (else
                   (((draw-pixmap-posn "player1.png") pixMap)  (make-posn (movementX playeres step diagonal direction)
                                                    (movementY playeres step diagonal direction))
                                          (getteam (car playeres)))
                    ))
           (drawplayeres (append newplayeres (cdr playeres)) 1)
           (drawBall (interseccion (car playeres)
                                      (movementX playeres step diagonal direction)
                                      (movementY playeres step diagonal direction)
                                      ball (append newplayeres (cdr playeres))))
           (drawWindow)
           (estela playeres (+ step 18) newplayeres diagonal direction
                           (cambiarStrengthBall (interseccion (car playeres)
                                                            (movementX playeres step diagonal direction)
                                                            (movementY playeres step diagonal direction) ball
                                                            (append newplayeres (cdr playeres))))
                           (verificarGol ball points (append playeres newplayeres) generaciones iteraciones) generaciones iteraciones))))))

(define (playGameAux players generaciones ball points iteraciones)
  (playGame
   (seleccion-reproduction-aux (move players))
   generaciones ball points iteraciones)
  )

(define (playGameGol players generaciones points iteraciones)
  (playGame (alinear (divideteams players))
              generaciones '((450 260) 0 0)
              points iteraciones))

(define (winner points)
(cond (
       ;Gana el jugador 1
       ((= (car points) (+ 3 (cadr points)))
        (begin
          (winner1)
          (drawWindow))

        )
       ;Gana el jugador 2
       ((= (+ 3 (car points)) (cadr points))
        (begin
          (winner2)
          (drawWindow))
        )

       ))
  )


(define (playGame players generaciones ball points iteraciones)
  (begin
    (drawField)
    (writeStrings points generaciones)
    (display "Generacion: ")
    (display generaciones)
    (newline)
    
    (cond ((or (= (car points) (+ 3 (cadr points))) (= (+ 3 (car points)) (cadr points)))
           (begin
             (writeStrings points generaciones)
             (winner points)
             ))
          ((< generaciones iteraciones)
           (playGameAux
            (estela players 0 '() (hypotenuse (car players)) (getDirection (car players)) ball
                    points generaciones iteraciones)
            (+ generaciones 1) ball points iteraciones))
          (else (writeStrings points generaciones))
          )
    
    
    (copy-viewport pixMap mainWindow)
    ;((clear-viewport pixMap))
    ))

(define (makeOneList teams)
  (append (car teams) (cadr teams))
  )

(define (firstPlayers form1 form2)
  (move-aux (inicialitation1 form1) (inicialitation2 form2) firstBall)
  )


(define (CatarCEQualifiers form1 form2 iteraciones)
  (cond ((or (null? form1) (null? form2)) '())
        (else
         (begin
           (playGame (makeOneList (firstPlayers form1 form2)) 1 firstBall '(0 0) iteraciones)
           ))
  ))


(CatarCEQualifiers '(4 4 2) '(4 3 3) 20)