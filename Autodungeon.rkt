#lang racket

(require racket/gui)
(require racket/draw)
(require racket/mpair)

; Estrutura Posição X e Y
(struct pos (x y))

; Definições do tamanho da tela
(define largura 30)
(define altura 30)
(define tam 16)
(define tela_largura (* largura tam))
(define tela_altura (* altura tam))
(define rnd (make-pseudo-random-generator))

; Definições do jogo
(define vidas 3)
(define score 0)
(define nome "")
(define inimigos (mcons 0 0))
(define spawner 30)
(define count 0)
(define level 1)

; Posição inicial
(define start-pos (pos(/ (* largura tam) 2.5) (- (* altura tam) 90)))
(define char-pos (pos 0 0))
  
; sprites do jogo
(define char (read-bitmap "./knight.png"))
;(define fire (read-bitmap "./fire.png"))
;(define ice (read-bitmap "./ice.png"))
(define sword (read-bitmap "./sword.png"))
(define gameoversprite (read-bitmap "./gameover.png"))
(define background (read-bitmap "./background.png"))
  
; Desenha sprite
(define (desenha-sprite sprite poss)
  (send dc draw-bitmap sprite (pos-x  poss) (pos-y poss))
)

; objeto tela (frame)
(define frame (new frame%
                   [label "Dungeon game"]
                   [width tela_largura]
                   [height tela_altura])
)

; Andar direita
(define (move-dir val)
     (if (< (pos-x char-pos) (-(* largura tam) 55)) 
       (set! char-pos (pos (+ (pos-x char-pos) val) (pos-y char-pos)))
      #f 
  ))

; Andar esquerda
(define (move-esq val)
   (if (> (pos-x char-pos) -10) 
       (set! char-pos (pos (- (pos-x char-pos) val) (pos-y char-pos)))
      #f 
  ))

; Termina o jogo
(define (gameover)
  (send timer stop)
  (send dc set-background "black")
  (send dc clear)
  (desenha-sprite gameoversprite (pos(/ (* largura tam) 4) (/ (* altura tam) 4)))
  (send dc draw-text "Pressione R para começar novamente!" (/ (* largura tam) 6.5) (/ (* altura tam) 1.5) )
 )
  

; Desabilita um inimigo
(define (disable e)
  (set-mcar! e (pos 600 -100))
  (set! score (+ score 1))
 )

; Causa dano ao jogador
(define (take-hit e)
  (display "colidiu com jogador!")
  (set! score (- score 1))
  (disable e)
  (set! vidas (- vidas 1))
  (if (= vidas 0)
      (gameover)
      #f
  )
)

; Checa colisão entre A e B
(define (check-colision a b)
  (if
    (and
      (<= (abs(- (pos-x a) (pos-x b))) 30)
      (<= (abs(- (pos-y a) (pos-y b))) 40)
    )
    #t
    #f
  )
)

; Checa por colisões dos inimigos com o jogador
(define (colidiu-p e )
  (if (check-colision (mcar e) char-pos)
      (take-hit e)
      #f
  ) 
)

; Checa por colisões dos inimigos com o chão
(define (colidiu-c e)
  (if (>= (pos-y (mcar e)) 470)
      (disable e)
      #f
      )
  )

; Spawn iterator
(define (spawn-it e)
  (autoplay (mcar e))
  (if (mpair? (mcdr e))
      (spawn-it (mcdr e))
      (set-mcdr! e (mcons  (pos (random 441) -50) 0))
  )
 )

; Spawn de 1 inimigo em local aleatório
(define (spawn-one)
  (set! count 0)
  (spawn-it inimigos)
)

; Spawn inimigos
(define (spawn)
  (set! count (+ count level))
  (if (> count spawner) 
       (spawn-one)
       #f )
  )

; "Olha para cima"
; Retornos:
; 0 - Seguro
; 1 - Perigo a direita
; 2 - Perigo a esquerda

(define (lookup e)
   (if
      (<= (- (pos-x e) (pos-x char-pos)) 40)
      2
      (if
        (>= (- (pos-x e) (pos-x char-pos)) 40)
        1
        0
        )
  )
  0
)

; Modo autonomo
(define (autoplay e)
  (if (= 1 (lookup e))
          (move-esq 15)
          (if (= 2 (lookup e))
            (move-dir 15)
            (display (lookup e))
          )
      )
)

; Atualiza o sprite e checa por colisões
(define (att-final e)
  (set-mcar! e (pos (pos-x (mcar e)) (+(pos-y (mcar e)) (+ level 5))))
  (desenha-sprite sword (mcar e))
  (colidiu-p e)
  (colidiu-c e)
)

; Atualiza um inimigo
(define (att-one e)
  (if (= (pos-x (mcar e)) 600) ;Inimigos com X = 600 estão desabilitados
      #f
      (att-final e)
      )
 
  (att (mcdr e))
)

; Atualiza inimigos
(define (att lenemies)
    (if (mpair? lenemies)
      (att-one lenemies)
      #f
  )
)

; Aumenta a dificuldade
(define (level-up)
  (if (> score (* level 10))
      (set! level (+ level 1))
      #f
      )
)

; Ações dos botões in-game
(define (canvas-key frame) (class canvas%
                             (define/override (on-char key-event)
                               (cond
                                 [(eq? (send key-event get-key-code) '#\r) (iniciar)]))
                             (super-new [parent frame])))

; Cria o canvas
(define canvas ( new (canvas-key frame)))

; Drawing Context
(define dc (send canvas get-dc))

; Mostrar janela
(send frame show #t)

; Fonte
(send dc set-font (make-object font% 14 'modern))
(send dc set-text-foreground "white")

; Timer
(define timer (new timer%
                   [notify-callback (lambda()
                                      ; Fundo e personagem
                                      (desenha-sprite background (pos 0 0))
                                      (desenha-sprite char char-pos)
                                      
                                      ; Textos na tela
                                      (send dc draw-text "Score: " (- (* largura tam) 100) 5)
                                      (send dc draw-text (number->string score) (- (* largura tam) 35) 5)

                                      (send dc draw-text "Level " (- (* largura tam) 280) 5)
                                      (send dc draw-text (number->string level) (- (* largura tam) 220) 5)
                                      
                                      (send dc draw-text "Vidas: " 5 5)
                                      (send dc draw-text (number->string vidas) 70 5)
                                      
                                      ; Lógica do jogo
                                      (spawn)
                                      (att inimigos)
                                      (level-up)
)]))

; Inicializa o jogo
(define iniciar (lambda()
                  (set! score 0)
                  (set! vidas 3)
                  (set! level 1)
                  (set-mcar! inimigos (pos (random 441) -50))
                  (set! char-pos (struct-copy pos start-pos))
                  (send timer start 70)))

(iniciar)