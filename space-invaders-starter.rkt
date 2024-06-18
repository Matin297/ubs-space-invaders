;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders
;; A simple version of the game

;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 1000)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))


;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))


(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))


(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. represents a list of invaders

(define LOI0 empty)
(define LOI1 (list I1 I2 I3))

#;
(define (fn-for-loinvader loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loinvader (rest loi)))]))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissile is one of:
;; - empty
;; - (cons Missile ListOfMissile)
;; interp. represents a list of missiles

(define LOM0 empty)
(define LOM1 (list M1 M2 M3))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))



;; Functions:

;; Game -> Game
;; start the world with (main (make-game empty empty (make-tank (/ WIDTH 2) 0)))
;; 
(define (main g)
  (big-bang g                ; Game
    (on-tick   next-game)    ; Game -> Game
    (to-draw   render)       ; Game -> Image
    (stop-when game-over)    ; Game -> Boolean
    (on-key    handle-key))) ; Game KeyEvent -> Game

;; Game -> Game
;; produce the next state of the game.
;; - removes missles and invaders that collide

; no hit
(check-random (next-game (make-game (list (make-invader 10 10 1))
                                    (list (make-missile 30 40))
                                    (make-tank 10 1)))
              (make-game (advance-invaders (spawn-invaders (list (make-invader 10 10 1))))
                         (list (make-missile 30 (- 40 MISSILE-SPEED)))
                         (make-tank (+ 10 TANK-SPEED) 1)))

; hit
(check-random (next-game (make-game (list (make-invader (- 100 1) (- 80 INVADER-Y-SPEED) 1))
                                    (list (make-missile 100 (+ 85 MISSILE-SPEED)))
                                    (make-tank WIDTH 1)))
              (make-game (advance-invaders (spawn-invaders empty))
                         empty
                         (make-tank WIDTH 1)))

; (define (next-game s) s) ; stub

;; template comes from Game

(define (next-game s)
  (detect-collisions (make-game (advance-invaders (spawn-invaders (game-invaders s)))
                                (advance-missiles (game-missiles s))
                                (advance-tank (game-tank s)))))


;; Tank -> Tank
;; advances the tank along the x axix by TANK-SPEED in its direction
;; - 1 means tank is going to right
;; - -1 means tank is going to left
;; - 0 means tank is not moving
;; - stays at the left/right edges until direction is changed by user
(check-expect (advance-tank (make-tank 100 -1))
              (make-tank (+ 100 (* TANK-SPEED -1)) -1))

(check-expect (advance-tank (make-tank (- WIDTH (/ TANK-SPEED 2)) 1))
              (make-tank WIDTH 1))

(check-expect (advance-tank (make-tank (+ 0 (/ TANK-SPEED 2)) -1))
              (make-tank 0 -1))

; (define (advance-tank t) t) ; stub

;; template comes from Tank

(define (advance-tank t)
  (cond [(>= (+ (tank-x t) (* TANK-SPEED (tank-dir t))) WIDTH)
         (make-tank WIDTH (tank-dir t))]
        [(<= (+ (tank-x t) (* TANK-SPEED (tank-dir t))) 0)
         (make-tank 0 (tank-dir t))]
        [else
         (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t)))
                    (tank-dir t))]))


;; ListOfInvader -> ListOfInvader
;; adds a new invader randomly at a random position going right on top edge
;; !!!

; (define (spawn-invaders loi) loi) ; stub

;; Template comes from ListOfInvader

(define (spawn-invaders loi)
  (cond [(< (random INVADE-RATE) (sqrt INVADE-RATE))
         (cons (make-invader (random WIDTH)
                             0
                             INVADER-X-SPEED)
               loi)]
        [else loi]))


;; ListOfInvader -> ListOfInvader
;; advances the invaders along x and y axis.
(check-expect (advance-invaders empty) empty)

(check-expect (advance-invaders (list (make-invader  2  9  1)
                                      (make-invader 10 80 -1)))
              (list (make-invader (+ 2   1) (+  9 INVADER-Y-SPEED)  1)
                    (make-invader (+ 10 -1) (+ 80 INVADER-Y-SPEED) -1)))

; (define (advance-invaders loi) loi) ; stub

;; template comes from ListOfInvader

(define (advance-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (advance-invader  (first loi))
               (advance-invaders (rest  loi)))]))

;; Invader -> Invader
;; advances the invader's x, y coordinates.
;; - bounce off the edges
;; - x by dx
;; - y by INVADER-Y-SPEED
(check-expect (advance-invader (make-invader 2 9 1))
              (make-invader (+ 2 1) (+ 9 INVADER-Y-SPEED) 1))

(check-expect (advance-invader (make-invader (- WIDTH 1) 9 1))
              (make-invader WIDTH (+ 9 INVADER-Y-SPEED) -1))

(check-expect (advance-invader (make-invader 1 9 -1))
              (make-invader 0 (+ 9 INVADER-Y-SPEED) 1))

; (define (advance-invader i) i) ; stub

;; template comes from Invader

(define (advance-invader invader)
  (cond [(>= (+ (invader-x invader) (invader-dx invader)) WIDTH)
         (make-invader
          WIDTH
          (+ (invader-y invader) INVADER-Y-SPEED)
          (- (invader-dx invader)))]
        [(<= (+ (invader-x invader) (invader-dx invader)) 0)
         (make-invader
          0
          (+ (invader-y invader) INVADER-Y-SPEED)
          (- (invader-dx invader)))]
        [else
         (make-invader
          (+ (invader-x invader) (invader-dx invader))
          (+ (invader-y invader) INVADER-Y-SPEED)
          (invader-dx invader))]))


;; ListOfMissile -> ListOfMissile
;; advances the missiles along the y axis by MISSILE-SPEED
(check-expect (advance-missiles empty) empty)

(check-expect (advance-missiles (list (make-missile 30 100)
                                      (make-missile 50  50)))
              (list (make-missile 30 (- 100 MISSILE-SPEED))
                    (make-missile 50 (- 50 MISSILE-SPEED))))

(check-expect (advance-missiles (list (make-missile 30 0)
                                      (make-missile 50  50)))
              (list (make-missile 50 (- 50 MISSILE-SPEED))))

; (define (advance-missiles lom) lom) ; stub

;; template comes from ListOfMissile

(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (off-screen? (first lom))
             (advance-missiles (rest  lom))
             (cons (advance-missile  (first lom))
                   (advance-missiles (rest  lom))))]))

;; Missile -> Boolean
;; produces true if  missile's y is <= 0
(check-expect (off-screen? (make-missile 30 40)) false)
(check-expect (off-screen? (make-missile 30  0)) true)
(check-expect (off-screen? (make-missile 30 -1)) true)

; (define (off-screen? m) false) ; stub

;; template comes from Missile

(define (off-screen? m)
  (<= (missile-y m) 0))


;; Missile -> Missile
;; advances the missile's y coordinate by MISSILE-SPEED
(check-expect (advance-missile (make-missile 30 100))
              (make-missile 30 (- 100 MISSILE-SPEED)))

(check-expect (advance-missile (make-missile 30 1))
              (make-missile 30 (- 1 MISSILE-SPEED)))

; (define (advance-missile m) m) ; stub

;; template comes from Missile

(define (advance-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))


;; Game -> Game
;; detects collisions between missles and invaders
;; and removes the missiles and invaders involved.
(check-expect (detect-collisions G2) G2)

(check-expect (detect-collisions G3)
              (make-game (list I2) (list M1) T1))

; (define (detect-collisions s) s) ; stub

;; template comes from Game

(define (detect-collisions s)
  (make-game (filter-invaders (game-invaders s) (game-missiles s))
             (filter-missiles (game-missiles s) (game-invaders s))
             (game-tank s)))

;; ListOfInvader ListOfMissile -> ListOfInvader
;; filters invaders that got hit by a missile
(check-expect (filter-invaders empty (list M1 M2)) empty)
(check-expect (filter-invaders (list I1 I2) empty) (list I1  I2))

(check-expect (filter-invaders (list I1 I2) (list M1 M2)) (list I2))
(check-expect (filter-invaders (list I2) (list M1 M2)) (list I2))

; (define (filter-invaders loi lom) loi) ; stub

;; template comes from ListOfInvader

(define (filter-invaders loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else
         (if (invader-hit? (first loi) lom)
             (rest loi)
             (cons (first loi) (filter-invaders (rest loi) lom)))]))

;; Invader ListOfMissile -> Boolean
;; produces true if there is a missile in the list
;; that collide with the given invader
(check-expect (invader-hit? I1 empty) false)
(check-expect (invader-hit? I1 (list M1 M2)) true)
(check-expect (invader-hit? (make-invader 100 20 -1)
                            (list (make-missile 100 200)
                                  (make-missile 10 25))) false)

; (define (missile-hit? i lom) false) ; stub

;; template comes from ListOfMissile + Invader

(define (invader-hit? i lom)
  (cond [(empty? lom) false]
        [else
         (if (hit? i (first lom))
             true
             (invader-hit? i (rest lom)))]))


;; ListOfMissile ListOfInvader -> ListOfMissile
;; filters missiles that hit invaders
(check-expect (filter-missiles empty (list I1 I2)) empty)
(check-expect (filter-missiles (list M1 M2) empty) (list M1 M2))

(check-expect (filter-missiles (list M1 M2) (list I1 I2)) (list M1))
(check-expect (filter-missiles (list M1) (list I1 I2)) (list M1))

; (define (filter-missiles lom loi) lom) ; stub

;; template comes from ListOfMissile

(define (filter-missiles lom loi)
  (cond [(empty? lom) empty]
        [(empty? loi) lom]
        [else
         (if (missile-hit? (first lom) loi)
             (rest lom)
             (cons (first lom) (filter-missiles (rest lom) loi)))]))

;; Missile ListOfInvader -> Boolean
;; produces true if there is an invader in the list
;; that collides with the given missile
(check-expect (missile-hit? (make-missile 20 100) empty) false)
(check-expect (missile-hit? M2 (list I1 I2)) true)
(check-expect (missile-hit? (make-missile 100 200)
                            (list (make-invader 100 20 -1)
                                  (make-invader 20 200 -1)))
              false)

; (define (missile-hit? m loi) false) ; stub

;; Template comes from ListOfInvader + Missile

(define (missile-hit? m loi)
  (cond [(empty? loi) false]
        [else
         (if (hit? (first loi) m)
             true
             (missile-hit? m (rest loi)))]))


;; Invader Missile -> Boolean
;; produce true if both of the below conditions are true
;; - (missile.y <= invader.y + HIT-RANGE)
;; - (missile.x = invader.x)
(check-expect (hit? I1 M2) true)
(check-expect (hit? (make-invader 100 20 -1) (make-missile 100 200)) false)
(check-expect (hit? (make-invader 100 20 -1) (make-missile 10 25)) false)

; (define (hit? i m) false) ; stub

;; template is a combination of Invader and Missile

(define (hit? i m)
  (and
   (and (<= (missile-x m) (+ (invader-x i) HIT-RANGE))
        (>= (missile-x m) (- (invader-x i) HIT-RANGE)))
   (and (<= (- (missile-y m) (invader-y i))
            HIT-RANGE)
        (>= (- (missile-y m) (invader-y i))
            0))))


;; Game -> Image
;; renders the tank, missiles and invaders at their given positions
(check-expect (render G1)
              (place-image TANK (tank-x T1) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

(check-expect (render G3)
              (place-image
               TANK (tank-x T1) (- HEIGHT TANK-HEIGHT/2)
               (place-image
                INVADER (invader-x I1) (invader-y I1)
                (place-image
                 INVADER (invader-x I2) (invader-y I2)
                 (place-image
                  MISSILE (missile-x M1) (missile-y M1)
                  (place-image
                   MISSILE (missile-x M2) (missile-y M2)
                   BACKGROUND))))))

; (define (render g) BACKGROUND) ;stub

;; template comes from Game

(define (render s)
  (place-tank (game-tank s)
              (place-invaders (game-invaders s)
                              (place-missiles (game-missiles s)))))

;; ListOfMissile -> Image
;; places the list of missiles on the BACKGROUND
(check-expect (place-missiles empty) BACKGROUND)

(check-expect (place-missiles (list M1 M2))
              (place-image
               MISSILE (missile-x M1) (missile-y M1)
               (place-image
                MISSILE (missile-x M2) (missile-y M2)
                BACKGROUND)))

; (define (place-missiles lom) BACKGROUND) ; stub

;; template comes from ListOf Missile

(define (place-missiles lom)
  (cond [(empty? lom) BACKGROUND]
        [else
         (place-missile (first lom) (place-missiles (rest lom)))]))

;; Missile Image -> Image
;; places the given missile on the given image at x, y coordinates
;; - use MISSILE image
(check-expect (place-missile M1 BACKGROUND)
              (place-image MISSILE (missile-x M1) (missile-y M1) BACKGROUND))

; (define (place-missile m img) BACKGROUND) ; stub

;; template comes from Missile + atomic non-distinct

(define (place-missile m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))


;; ListOfInvaders Image -> Image
;; places the list of invaders on the given image
(check-expect (place-invaders empty BACKGROUND) BACKGROUND)
(check-expect (place-invaders (list I1 I2) BACKGROUND)
              (place-image
               INVADER (invader-x I1) (invader-y I1)
               (place-image
                INVADER (invader-x I2) (invader-y I2)
                BACKGROUND)))

; (define (place-invaders loi img) BACKGROUND) ; stub

;; template comes from ListOfInvader + atomic non-distinct

(define (place-invaders loi img)
  (cond [(empty? loi) img]
        [else
         (place-invader (first loi) (place-invaders (rest loi) img))]))


;; Invader Image -> Image
;; places the given invader on the given img at x, y coordinates
;; - use INVADER image
(check-expect (place-invader I1 BACKGROUND)
              (place-image INVADER
                           (invader-x I1)
                           (invader-y I1)
                           BACKGROUND))

; (define (place-invader i img) BACKGROUND) ; stub

;; template comes from Invader + atomic non-distinct

(define (place-invader i img)
  (place-image INVADER
               (invader-x i)
               (invader-y i)
               img))


;; Tank Image -> Image
;; places the tank on the given image
;; - use TANK image
;; - y is constant: HEIGHT - TANK-HEIGHT/2
(check-expect (place-tank T1 BACKGROUND)
              (place-image TANK
                           (tank-x T1)
                           (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))

; (define (place-tank t img) BACKGROUND) ; stub

;; template comes from Tank + atomic non-distinct

(define (place-tank t img)
  (place-image TANK
               (tank-x t)
               (- HEIGHT TANK-HEIGHT/2)
               img))


; Game KeyEvent -> Game
;; when spacebar is pressed add a missile
;; - x coordinate of tank
;; - y coordinate of (HEIGHT - TANK-HEIGHT/2)
;; when left arrow is pressed change tank direction
;; - left: -1
;; - right: 1
(check-expect (handle-key G0 " ")
              (make-game empty
                         (list (make-missile (tank-x (game-tank G0)) (- HEIGHT TANK-HEIGHT/2)))
                         T0))

(check-expect (handle-key G2 "left")
              (make-game (game-invaders G2) (game-missiles G2)
                         (make-tank (tank-x (game-tank G2)) -1)))

(check-expect (handle-key G3 "right")
              (make-game (game-invaders G3) (game-missiles G3)
                         (make-tank (tank-x (game-tank G3)) 1)))

(check-expect (handle-key G1 "up") G1)

; (define (handle-key g ke) g) ; stub

;; template comes from KeyEvent

(define (handle-key g ke)
  (cond [(key=? ke " ")    (make-game
                            (game-invaders g)
                            (cons (make-missile
                                   (tank-x (game-tank g))
                                   (- HEIGHT TANK-HEIGHT/2))
                                  (game-missiles g))
                            (game-tank g))]
        [(key=? ke "left") (make-game
                            (game-invaders g)
                            (game-missiles g)
                            (make-tank (tank-x (game-tank g)) -1))]
        [(key=? ke "right") (make-game
                             (game-invaders g)
                             (game-missiles g)
                             (make-tank (tank-x (game-tank g)) 1))]
        [else g]))


;; Game -> Boolean
;; game is over when one of the invaders land on/pass by the bottom edge (HEIGHT)
(check-expect (game-over (make-game (list I1 I2) (list M1 M2) T1)) false)
(check-expect (game-over (make-game (list I1 I3) (list M1 M2) T1)) true)

; (define (game-over g) false) ; stub

;; template comes from Game

(define (game-over g)
  (any-landed? (game-invaders g)))


;; ListOfInvader -> Boolean
;; produces true if one of the invaders lands on/passes by the bottom edge
(check-expect (any-landed? empty) false)

(check-expect (any-landed? (list (make-invader 30 100 -1)
                                 (make-invader 100 30  1)))
              false)

(check-expect (any-landed? (list (make-invader 30 (+ HEIGHT 3) -1)
                                 (make-invader 100 30  1)))
              true)

; (define (any-landed? loi) false) ; stub

;; template comes from ListOfInvader

(define (any-landed? loi)
  (cond [(empty? loi) false]
        [else
         (if (landed? (first loi))
             true
             (any-landed? (rest loi)))]))


;; Invader -> Boolean
;; produces true if invader's y is >= HEIGHT
(check-expect (landed? (make-invader 30 100    -1)) false)
(check-expect (landed? (make-invader 30 HEIGHT  1)) false)
(check-expect (landed? (make-invader 30 (+ HEIGHT 3)  1)) true)

; (define (landed? i) false) ; stub

;; template comes from Invader

(define (landed? i)
  (> (invader-y i) HEIGHT))
