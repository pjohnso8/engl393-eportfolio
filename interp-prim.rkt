#lang racket
(provide interp-prim0 interp-prim1 interp-prim2 interp-primN)

;; Op0 -> Answer
(define (interp-prim0 op)
  (match op
    ['read-byte (read-byte)]
    ['peek-byte (peek-byte)]
    ['void      (void)]))

;; Op1 Value -> Answer
(define (interp-prim1 op v)
  (match (list op v)
    [(list 'add1 (? integer?))            (add1 v)]
    [(list 'sub1 (? integer?))            (sub1 v)]
    [(list 'zero? (? integer?))           (zero? v)]
    [(list 'char? v)                      (char? v)]
    [(list 'integer->char (? codepoint?)) (integer->char v)]
    [(list 'char->integer (? char?))      (char->integer v)]
    [(list 'write-byte    (? byte?))      (write-byte v)]
    [(list 'eof-object? v)                (eof-object? v)]
    ;; TODO: handle -, abs, integer?, etc.
    [(list '- (? integer?))               (- v)]
    [(list 'abs (? integer?))             (abs v)]
    [(list 'integer? v)                   (integer? v)]
    [(list 'boolean? v)                   (boolean? v)]
    [_ 'err]))

;; Op2 Value Value -> Answer
(define (interp-prim2 op v1 v2)
  (match (list op v1 v2)
    [(list '- (? integer?) (? integer?)) (- v1 v2)]
    [(list '< (? integer?) (? integer?)) (< v1 v2)]
    [(list '= (? integer?) (? integer?)) (= v1 v2)]
    ;; TODO: Make + take any number of arguments, see hint below.
    ;; Once that works, you can remove this code:
    [(list '+ (? integer?) (? integer?)) (+ v1 v2)]
    [_ 'err]))

;; HINT: You could use a function like the following and call it from interp.

;; OpN [Listof Value] -> Answer
(define (interp-primN op vs)
  (match op
    ['+ (match vs
          ['() 0]  ; Empty case - return identity for addition
          [(cons v rest)
           (if (integer? v)
               (let loop ([remaining rest]
                          [acc v])
                 (match remaining
                   ['() acc]
                   [(cons next more)
                    (if (integer? next)
                        (loop more (+ acc next))
                        'err)]))
               'err)])]
    ['- (match vs
          ['() 'err]  ; No arguments is an error for subtraction
          [(list v) (if (integer? v) (- v) 'err)]  ; Single argument - unary negation
          [(cons v rest)  ; Multiple arguments - subtract from left to right
           (if (andmap integer? (cons v rest))
               (foldr (λ (x acc) (- acc x))
                      (last (cons v rest))
                      (reverse (drop-right (cons v rest) 1)))
               'err)])]
    [_ 'err]))

;; Any -> Boolean
(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))

