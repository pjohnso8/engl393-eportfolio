#lang racket
(provide interp)
(provide interp-env)
(require "ast.rkt")
(require "interp-prim.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void

;; type Env = (Listof (List Id Value))

;; Expr -> Answer
(define (interp e)
  (interp-env e '()))

;; Expr Env -> Answer
(define (interp-env e r)
  (match e
    [(Lit d) d]
    [(Eof)   eof]
    [(Var x) (lookup r x)]
    [(Prim0 p) (interp-prim0 p)]
    [(Prim1 p e)
     (match (interp-env e r)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v1 (match (interp-env e2 r)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    ;; TODO: implement n-ary primitive
    [(PrimN p es)
     (match (interp*-env es r)
       ['err 'err]
       [vs (match p
             ['+  (if (andmap integer? vs)
                      (apply + vs)
                      'err)]
             ['-  (cond
                    [(empty? vs) 'err]
                    [(= (length vs) 1) (- (first vs))]
                    [else (foldr (lambda (x y) (- y x))
                                 (last vs)
                                 (reverse (drop-right vs 1)))])])])]
    [(If e0 e1 e2)
     (match (interp-env e0 r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(Begin e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [_ (interp-env e2 r)])]
    ;; TODO: implement cond
    [(Cond cs el)
     (let loop ([cs cs])
       (match cs
         ['() (interp-env el r)]
         [(cons (Clause e1 e2) rest)
          (match (interp-env e1 r)
            ['err 'err]
            [v (if v
                   (interp-env e2 r)
                   (loop rest))])]))]
    ;; TODO: implement case
    [(Case ev cs el)
     (match (interp-env ev r)
       ['err 'err]
       [v (let loop ([cs cs])
            (match cs
              ['() (interp-env el r)]
              [(cons (Clause dat e) rest)
               (if (member v dat)
                   (interp-env e r)
                   (loop rest))]))])]
    ;; TODO: this works for just a single binding
    ;; but you need to make it work in general
    [(Let xs es e2)
     (match (interp*-env es r)
       ['err 'err]
       [vs (interp-env e2 (append (map list xs vs) r))])]
    ;; TODO: implement let*
    [(Let* xs es e)
     (let loop ([xs xs] [es es] [r r])
       (match* (xs es)
         [('() '()) (interp-env e r)]
         [((cons x xs) (cons e1 es))
          (match (interp-env e1 r)
            ['err 'err]
            [v (loop xs es (ext r x v))])]))]))

;; HINT: this is a function that may come in handy.
;; It takes a list of expressions and environment
;; and evaluates each expression in order.  If any
;; expression produces 'err, the whole thing produces
;; 'err; otherwise it produces a list of values.

;; type Answer* = 'err | [Listof Value]
;; [Listof Expr] Env -> Answer*
(define (interp*-env es r)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r)
       ['err 'err]
       [v (match (interp*-env es r)
            ['err 'err]
            [vs (cons v vs)])])]))

;; Env Id -> Value
(define (lookup r x)
  (match r
    [(cons (list y val) r)
     (if (symbol=? x y)
         val
         (lookup r x))]))


;; Env Id Value -> Env
(define (ext r x v)
  (cons (list x v) r))

