#lang racket
(provide (all-defined-out))
(require "ast.rkt")
(require "compile-ops.rkt")
(require "types.rkt")
(require a86/ast)

(define rax 'rax)
(define rsp 'rsp) ; stack
(define r15 'r15) ; stack pad (non-volatile)

;; Expr -> Asm
(define (compile e)
  (prog (Global 'entry)
        (Extern 'peek_byte)
        (Extern 'read_byte)
        (Extern 'write_byte)
        (Extern 'raise_error)
        (Label 'entry)
        ;; save callee-saved register
        (Push r15)
        (compile-e e '())
        ;; restore callee-save register
        (Pop r15)
        (Ret)
        ;; Error handler
        (Label 'err)
        pad-stack
        (Call 'raise_error)))

;; type CEnv = (Listof [Maybe Id])
;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Lit d) (compile-value d)]
    [(Eof) (compile-value eof)]
    [(Var x) (compile-variable x c)]
    [(Prim0 p) (compile-prim0 p)]
    [(Prim1 p e) (compile-prim1 p e c)]
    [(Prim2 p e1 e2) (compile-prim2 p e1 e2 c)]
    [(PrimN p es) (seq (compile-e* es c)
                       (compile-primn p (length es)))]
    [(If e1 e2 e3) (compile-if e1 e2 e3 c)]
    [(Begin e1 e2) (compile-begin e1 e2 c)]
    [(Let xs es e2) (compile-let xs es e2 c)]
    [(Let* xs es e) (compile-let* xs es e c)]
    [(Case ev cs el) (compile-case ev cs el c)]
    [(Cond cs el) (compile-cond cs el c)]))


;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

;; Op0 -> Asm
(define (compile-prim0 p)
  (compile-op0 p))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c)
       (compile-op1 p)))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (compile-op2 p)))

;; [Listof Expr] CEnv -> Asm
(define (compile-e* es c)
  (match es
    ['() (seq)]
    [(cons e es)
     (seq (compile-e e c)
          (Push rax)
          (compile-e* es (cons #f c)))]))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp rax (value->bits #f))
         (Je l1)
         (compile-e e2 c)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c)
         (Label l2))))

;; Expr Expr CEnv -> Asm
(define (compile-begin e1 e2 c)
  (seq (compile-e e1 c)
       (compile-e e2 c)))

;; Id Expr Expr CEnv -> Asm
;; NOTE: this is specialized for a single variable binding
;; You should write another function for the general case
(define (compile-let xs es e2 c)
  (seq (compile-e* (reverse es) c)     ; Evaluate bindings in reverse order
       (compile-e e2 (append xs c))     ; Evaluate body with extended environment
       (Add rsp (* 8 (length xs)))))

(define (compile-let* xs es e c)
  (match* (xs es)
    [('() '())
     (compile-e e c)]
    [((cons x xs) (cons e1 es))
     (seq (compile-e e1 c)
          (Push rax)
          (compile-let* xs es e (cons x c))
          (Add rsp 8))]))

(define (compile-case ev cs el c)
  (let ((end-label (gensym 'case)))
    (seq (compile-e ev c)
         (compile-case-clauses cs el c end-label)
         (Label end-label))))

(define (compile-cond cs el c)
  (match cs
    ['() (compile-e el c)]
    [(cons (Clause e1 e2) rest)
     (let ((l1 (gensym 'cond))
           (l2 (gensym 'cond)))
       (seq (compile-e e1 c)
            (Cmp rax (value->bits #f))
            (Je l1)
            (compile-e e2 c)
            (Jmp l2)
            (Label l1)
            (compile-cond rest el c)
            (Label l2)))]))

(define (compile-case-clauses cs el c end-label)
  (match cs
    ['() (compile-e el c)]
    [(cons (Clause dat e) rest)
     (let ((match-label (gensym 'match))
           (next-label (gensym 'next)))
       (seq (let loop ([vals dat])
              (match vals
                ['() (seq)]
                [(cons v rest)
                 (seq (Cmp rax (value->bits v))
                      (Je match-label)
                      (loop rest))]))
            (Jmp next-label)
            (Label match-label)
            (compile-e e c)
            (Jmp end-label)
            (Label next-label)
            (compile-case-clauses rest el c end-label)))]))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

(define (compile-primn p n)
  (match p
    ['+ (if (= n 0)
            (seq (Mov rax (value->bits 0)))
            (seq (Mov rax (Offset rsp (* 8 (sub1 n))))
                 ;; Validate first operand is a number
                 (Mov r15 rax)
                 (And r15 1)
                 (Cmp r15 0)
                 (Jne 'err)
                 (let loop ([i (- n 2)])
                   (if (>= i 0)
                       (seq ;; Load and validate each subsequent operand
                        (Mov r15 (Offset rsp (* 8 i)))
                        ;; Check if number
                        (Mov rax r15)
                        (And rax 1)
                        (Cmp rax 0)
                        (Jne 'err)
                        ;; Do addition and check overflow
                        (Mov rax r15)
                        (Add rax (Offset rsp (* 8 (sub1 n))))
                        (Jo 'err)
                        (Mov (Offset rsp (* 8 (sub1 n))) rax)
                        (loop (sub1 i)))
                       (seq)))
                 ;; Load final result
                 (Mov rax (Offset rsp (* 8 (sub1 n))))
                 (Add rsp (* 8 n))))]
    ['- (if (= n 1)
            (seq (Mov rax (Offset rsp 0))
                 ;; Validate operand is a number
                 (Mov r15 rax)
                 (And r15 1)
                 (Cmp r15 0)
                 (Jne 'err)
                 ;; Negate the number
                 (Mov r15 rax)
                 (Sub rax r15)
                 (Sub rax r15)
                 (Jo 'err)
                 (Add rsp 8))
            (seq (Mov rax (Offset rsp (* 8 (sub1 n))))
                 ;; Validate first operand is a number
                 (Mov r15 rax)
                 (And r15 1)
                 (Cmp r15 0)
                 (Jne 'err)
                 (let loop ([i (- n 2)])
                   (if (>= i 0)
                       (seq ;; Load and validate each subsequent operand
                        (Mov r15 (Offset rsp (* 8 i)))
                        ;; Check if number
                        (Mov rax r15)
                        (And rax 1)
                        (Cmp rax 0)
                        (Jne 'err)
                        ;; Do subtraction and check overflow
                        (Mov rax (Offset rsp (* 8 (sub1 n))))
                        (Sub rax r15)
                        (Jo 'err)
                        (Mov (Offset rsp (* 8 (sub1 n))) rax)
                        (loop (sub1 i)))
                       (seq)))
                 ;; Load final result
                 (Mov rax (Offset rsp (* 8 (sub1 n))))
                 (Add rsp (* 8 n))))]))

