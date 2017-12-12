#lang racket

;;;; Author: Bruno Cesar, bcesar.g6@gmail.com
;;;; since 3/12/2017

;; Bibliotecas
(require racket/gui)
(require racket/draw)
(require racket/mpair)
(require rackunit)
(require rackunit/text-ui)

;; Definições gerais
(struct pos (x y))
(define rnd (make-pseudo-random-generator))

;; Definições do tamanho da tela
(define largura 30)
(define altura 30)
(define tam 16)
(define tela_largura (* largura tam))
(define tela_altura (* altura tam))

;; Definições do jogo
(define vidas 3)
(define score 0)
(define nome "")
(define inimigos (mcons 0 0))
(define life-potion (pos -10 -10))
(define spd-potion (pos -10 -10))
(define spawner 30)
(define count 0)
(define level 1)
(define direction 0)
(define speed 6)
(define AI #f)
(define char-pos (pos 0 0))
(define start-pos (pos(/ (* largura tam) 2.5) (- (* altura tam) 90))) ;Posição inicial
  
; sprites do jogo
(define char (read-bitmap "./knight.png"))
(define lifep (read-bitmap "./life_potion.png"))
(define speedp (read-bitmap "./spd_potion.png"))
(define sword (read-bitmap "./sword.png"))
(define gameoversprite (read-bitmap "./gameover.png"))
(define background (read-bitmap "./background.png"))

; objeto tela (frame)
(define frame (new frame%
                   [label "Dungeon game"]
                   [width tela_largura]
                   [height tela_altura])
)

;; ***

;; Funções auxiliares

; Desenha sprite na tela
(define (desenha-sprite sprite poss)
  (send dc draw-bitmap sprite (pos-x  poss) (pos-y poss))
)

; Printa uma posição  (usada para debug)
(define (print-pos p)
  (display "Pos [")
  (display (pos-x p))
  (display "/")
  (display (pos-y p))
  (displayln "]")
 )

; Lê o nome do jogador
(define get-nome (lambda ()
                   (display "Seu nome, desafiante: ")
                   (define nome (read))
                   (when (not (string? nome)) (set! nome (symbol->string nome)))
                   (cond [(string=? nome "")
                          (display "Nome inválido")
                          (get-nome)]
                         [else nome]))
)

; Escreve o score do jogador nos scores
(define (grava-score)
  (define file (open-output-file "scores.txt"
                #:exists 'append))
  
  ; Se IA ativada então grava como IA
  (if AI
      (display "IA" file)
      (display nome file)
  )
  
  (display " " file)
  (displayln score file)
  (close-output-port file)
)
;; ***

;; Funções da IA
; Andar direita (IA)
(define (move-dir)
     (if (< (pos-x char-pos) (-(* largura tam) 55)) 
       (set! char-pos (pos (+ (pos-x char-pos) 15) (pos-y char-pos)))
      #f 
  ))

; Andar esquerda (IA)
(define (move-esq)
   (if (> (pos-x char-pos) -10) 
       (set! char-pos (pos (- (pos-x char-pos) 15) (pos-y char-pos)))
      #f 
  ))


;; "Olha para cima"
;; Função auxiliar a IA, compara a posição de um inimigo com a posição do jogador
;; Retorna  um valor que representa um estado de decisão para a IA
;; Tests
;; Pos -> Número
;; 0 - Seguro
;; 1 - Perigo a direita
;; 2 - Perigo a esquerda
(define lookup-tests
         (test-suite
          "lookup tests"
          (check-equal? (lookup (pos 180 20)) 2)
          (check-equal? (lookup (pos 210 20)) 1)
          (check-equal? (lookup (pos 20 20)) 0)
          )
         )

(define (lookup e)
   (let ([dist (- (pos-x e) (pos-x char-pos)) ])
     (if
          (and (>= dist -40) (<= dist 5))
          2
          (if
            (and (<= dist 50) (>= dist -10))
            1
            0
            )
       )
    )
  )

; Modo autonomo (IA)
(define (autoplay e)
  (if (= 1 (lookup e))
          (move-esq) 
          (if (= 2 (lookup e))
            (move-dir)
            #f
          )
      )
)

;; ***

;;; Lógica do Jogo

;; Gerais

; Termina o jogo
(define (gameover)
  (set! inimigos (mcons 0 0))
  (set! life-potion (pos -10 -10))
  (set! spd-potion (pos -10 -10))
  (send timer stop)
  (send dc set-background "black")
  (send dc clear)
  (send dc draw-text "Pontuação final: " (/ (* largura tam) 3.5) 30)
  (send dc draw-text (number->string score) (+(/ (* largura tam) 2) 70) 30)
  (desenha-sprite gameoversprite (pos(/ (* largura tam) 4) (/ (* altura tam) 4)))
  (send dc draw-text "Pressione R para começar novamente!" (/ (* largura tam) 6.5) (/ (* altura tam) 1.5) )
  (grava-score)
 )

; Aumenta a dificuldade
(define (level-up)
  (if (> score (* level 10))
      (set! level (+ level 1))
      #f
      )
)

; Aciona ou cancela a IA
(define (toggleAI)
  (if AI
      (set! AI #f)
      (set! AI #t)
      )

  (if AI
      (displayln "IA ativada")
      (displayln "IA desativada")
   )
  )

;; Auxiliares

; Checa colisão entre A e B
;; Tests
;; Pos -> boolean
;; true - Objeto A e B colidiram
;; false - Objeto A e B não colidiram
(define collision-tests
         (test-suite
          "Collision tests"
          (check-equal? (check-collision (pos 180 20) (pos 120 10)) #f)
          (check-equal? (check-collision (pos 210 20) (pos 200 15)) #t)
          (check-equal? (check-collision (pos 10 10) (pos 100 100)) #f)
          )
         )

(define (check-collision a b)
  (if
    (and
      (<= (abs(- (pos-x a) (pos-x b))) 30)
      (<= (abs(- (pos-y a) (pos-y b))) 40)
    )
    #t
    #f
  )
)

;; Interações do personagem com os objetos
; Efeito life potion
(define (get-life)
  (set! life-potion (pos -10 -10))
  (set! vidas (+ vidas 1))
  )

; Efeito speed potion
(define (inc-spd)
  (set! spd-potion (pos -10 -10))
  (set! speed (+ speed 1))
  )

; Causa dano ao jogador
(define (take-hit e)
  (set! score (- score 1))
  (disable e)
  (set! vidas (- vidas 1))
  (if (= vidas 0)
      (gameover)
      #f
  )
)

; Checa por colisões dos inimigos com o jogador
(define (colidiu-p e )
  (if (check-collision (mcar e) char-pos)
      (take-hit e)
      #f
  ) 
)

;; Interações inimigo
; Desabilita um inimigo
(define (disable e)
  (set-mcar! e (pos 600 -100))
  (set! score (+ score 1))
 )


; Checa por colisões dos inimigos com o chão
(define (colidiu-c e)
  (if (>= (pos-y (mcar e)) 465)
      (disable e)
      #f
      )
) 

;;; Spawn main: inimigos + poções
(define (spawn-one)
  (set! count 0)
  (spawn-it inimigos)
  (spawn-potions) 
)

;; Spawn de inimigos
; Spawn iterator
(define (spawn-it e)
  
  (if (mpair? (mcdr e))
      (spawn-it (mcdr e))
      (set-mcdr! e (mcons  (pos (random 441) -50) 0))
  )
 )

; Spawn inimigos
(define (spawn)
  (set! count (+ count level))
  (if (> count spawner) 
       (spawn-one)
       #f )
  )

;; Spawn de poções
; Se não existir poção de vida cria ela
(define (spawn-life-potion)
  (if (= -10 (pos-x life-potion))
      (set! life-potion (pos (random 441) -30))
      #f)
  )

; Se não exister poção de velocidade no jogo cria ela
(define (spawn-spd-potion)
  (if (= -10 (pos-x spd-potion))
      (set! spd-potion (pos (random 441) -30))
      #f)
  )

; 30% de chance de spawnar uma potion (24% life 6% Speed)
(define (spawn-potions)
  (if (>= (random 100) 70)
          (if (>= (random 10) 8)
              (spawn-spd-potion)
              (spawn-life-potion)
              )
          #f
          )
)
  
;; Atualização dos inimigos

; Atualiza o sprite e checa por colisões
(define (att-final e)
  (set-mcar! e (pos (pos-x (mcar e)) (+(pos-y (mcar e)) (+ level 5))))
  (desenha-sprite sword (mcar e))
  (colidiu-p e)
  (colidiu-c e)
  (if AI
      (autoplay (mcar e))
      #f
      )
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

;; Atualização poções

; Atualiza life potion
(define (att-life-potion)
  (set! life-potion (pos (pos-x life-potion) (+(pos-y life-potion) 5)))
  (desenha-sprite lifep life-potion)

  ; Checa colisão com personagem
  (if (check-collision char-pos life-potion)
      (get-life)
      #f)
        

  ; Checa colisões com o chão
  (if (>= (pos-y life-potion) 465)
      (set! life-potion (pos -10 -10))
      #f)

 ) 

; Atualiza speed potion
(define (att-spd-potion)
  (set! spd-potion (pos (pos-x spd-potion) (+(pos-y spd-potion) 5)))
  (desenha-sprite speedp spd-potion)

  ; Checa colisão com personagem
  (if (check-collision char-pos spd-potion)
      (inc-spd)
      #f)
        

  ; Checa colisões com o chão
  (if (>= (pos-y spd-potion) 465)
      (set! spd-potion (pos -10 -10))
      #f)

 )

; Atualiza poções
(define (att-potions)
  (if (not (= -10 (pos-x spd-potion)))
      (att-spd-potion)
      #f)
  
  (if (not (= -10 (pos-x life-potion)))
      (att-life-potion)
      #f)
  )

;; Ações jogador

; Movimentação jogador
(define (jogador-move)
  (if (= direction 1)

      (if (< (pos-x char-pos) (-(* largura tam) 55))
          (set! char-pos (pos (+ (pos-x char-pos) (* direction speed)) (pos-y char-pos)))
          #f)

      (if (= direction -1)
           (if (> (pos-x char-pos) -10)
               (set! char-pos (pos (+ (pos-x char-pos) (* direction speed)) (pos-y char-pos)))
               #f)
           #f)
      )
  )
     


; Ações dos botões in-game
(define (canvas-key frame) (class canvas%
                             (define/override (on-char key-event)
                               (cond
                                 [(eq? (send key-event get-key-code) 'left) (set! direction -1)]
                                 [(eq? (send key-event get-key-code) 'right) (set! direction 1)]
                                 [(eq? (send key-event get-key-code) 'release) (set! direction 0)]
                                 [(eq? (send key-event get-key-code) '#\a) (toggleAI)]
                                 [(eq? (send key-event get-key-code) '#\r) (iniciar)]))
                             (super-new [parent frame])))

;;; Configurações finais ( Drawing context, timer, etc)

; Cria o canvas
(define canvas ( new (canvas-key frame)))

; Drawing Context
(define dc (send canvas get-dc))

; Fonte
(send dc set-font (make-object font% 14 'modern))
(send dc set-text-foreground "white")

; Mostrar janela
(send frame show #t)

; Timer
(define timer (new timer%
                   [notify-callback (lambda()
                                      ; Movimentação não IA
                                      (jogador-move)
                                      
                                      ; Fundo e personagem
                                      (desenha-sprite background (pos 0 0))
                                      (desenha-sprite char char-pos)
                                      
                                      ; Textos na tela
                                      (send dc draw-text "Score: " (- (* largura tam) 100) 5)
                                      (send dc draw-text (number->string score) (- (* largura tam) 35) 5)

                                      (if AI
                                           (send dc draw-text "(IA playing)" (- (* largura tam) 312) 30)
                                           #f
                                         )
                                      
                                      (send dc draw-text "Level " (- (* largura tam) 280) 5)
                                      (send dc draw-text (number->string level) (- (* largura tam) 220) 5)
                                      
                                      (send dc draw-text "Lifes: " 5 5)
                                      (send dc draw-text (number->string vidas) 70 5)
                                      
                                      ; Lógica do jogo
                                      (spawn)
                                      (att inimigos)
                                      (att-potions)
                                      (level-up)
)]))



; Executa os testes
(define (testes)
  (run-tests (test-suite "Todos os testes" lookup-tests collision-tests))
  (void)
 )

; Inicializa o jogo
(define iniciar (lambda()
                  (set! score 0)
                  (set! vidas 3)
                  (set! level 1)
                  (set! speed 6)
                  (set-mcar! inimigos (pos (random 441) -50))
                  (set! char-pos (struct-copy pos start-pos))
                  (testes)
                  (send timer start 70)))


; Comandos inicias da execução
(set! nome (get-nome))
(iniciar)