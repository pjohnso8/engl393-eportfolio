#lang racket
(provide Lit Prim0 Prim1 Prim2 PrimN If Eof Begin Var Let Let* Cond Case Clause)

;; type Expr = (Lit Datum)
;;           | (Eof)
;;           | (Prim0 Op0)
;;           | (Prim1 Op1 Expr)
;;           | (Prim2 Op2 Expr Expr)
;;           | (PrimN OpN [Listof Expr])
;;           | (If Expr Expr Expr)
;;           | (Begin Expr Expr)
;;           | (Var Id)
;;           | (Let  [Listof Id] [Listof Expr] Expr)  ; lengths must be equal
;;           | (Let* [Listof Id] [Listof Expr] Expr)  ; lengths must be equal
;;           | (Cond [Listof CondClause] Expr)
;;           | (Case Expr [Listof CaseClause] Expr)

;; type Id  = Symbol
;; type Datum = Integer
;;            | Boolean
;;            | Character
;; type Op0 = 'read-byte | 'peek-byte | 'void
;; type Op1 = 'add1 | 'sub1
;;          | 'zero?
;;          | 'char? | 'integer->char | 'char->integer
;;          | 'write-byte | 'eof-object?
;;          | '- | 'not | 'abs
;; type Op2 = '- | '< | '=
;; type OpN = '+
;; type CondClause = (Clause Expr Expr)
;; type CaseClause = (Clause [Listof Datum] Expr)

(struct Eof () #:prefab)
(struct Lit (d) #:prefab)
(struct Prim0 (p) #:prefab)
(struct Prim1 (p e) #:prefab)
(struct Prim2 (p e1 e2)  #:prefab)
(struct PrimN (p es) #:prefab)
(struct If (e1 e2 e3) #:prefab)
(struct Begin (e1 e2) #:prefab)
(struct Var (x) #:prefab)
(struct Let (xs es e) #:prefab)
(struct Let* (xs es e) #:prefab)
(struct Cond (cs e) #:prefab)
(struct Case (e cs el) #:prefab)
(struct Clause (p body) #:prefab)
