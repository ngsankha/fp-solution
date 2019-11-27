#lang racket
(provide (all-defined-out))
(require "syntax.rkt")

;; An immediate is anything ending in #b000
;; All other tags in mask #b111 are pointers

(define result-shift     3)
(define result-type-mask (sub1 (arithmetic-shift 1 result-shift)))
(define type-imm         #b000)
(define type-box         #b001)
(define type-pair        #b010)
(define type-string      #b011)
(define type-proc        #b100)
(define type-symbol      #b101)

(define imm-count-bits 3)
(define imm-shift        (+ imm-count-bits result-shift))
(define imm-type-mask    (sub1 (arithmetic-shift 1 imm-shift)))
(define imm-type-int     (arithmetic-shift #b000 result-shift))
(define imm-type-bool    (arithmetic-shift #b001 result-shift))
(define imm-type-char    (arithmetic-shift #b010 result-shift))
(define imm-type-empty   (arithmetic-shift #b011 result-shift))
(define imm-type-eof     (arithmetic-shift #b100 result-shift))
(define imm-type-void    (arithmetic-shift #b101 result-shift))
(define imm-val-false    imm-type-bool)
(define imm-val-true
  (bitwise-ior (arithmetic-shift 1 (add1 imm-shift)) imm-type-bool))

;; Allocate in 64-bit (8-byte) increments, so pointers
;; end in #b000 and we tag with #b001 for boxes, etc.

;; type CEnv = (Listof (Maybe Variable))
;; type Imm = Integer | Boolean | Char | ''()

;; type Formals =
;; | '()
;; | Variable
;; | (Cons Variable Formals)

;; Expr+ -> Asm
;; Compile e as the entry point
(define (compile e)
  (let ((le (label-λ (intern-symbols (desugar (stdlib e))))))
    `(entry
      ,@(compile-tail-e le '())
      ret
      ,@(compile-λ-definitions (λs le))
      err
      (mov rbx rsp) ; align stack at 16-byte boundary
      (and rbx 8)   ; 0 if 16-byte aligned, 8 if 16-byte align
      (sub rsp rbx)
      (call error))))

;; type Compiler = LExpr CEnv -> Asm

;; LExpr CEnv -> Asm
(define (compile-e e c)
  ((compile-e/k compile-nontail-forms) e c))

;; LExpr CEnv -> Asm
(define (compile-tail-e e c)
  ((compile-e/k compile-tail-forms) e c))

;; Compiler -> (LExpr CEnv -> Asm)
(define (compile-e/k compile-tail-forms)
  (λ (e c)
    (match e
      [(? imm? i)                  (compile-imm i)]
      [(? symbol? x)               (compile-var x c)]
      [(? string? s)               (compile-string s)]
      [`',x                        (compile-symbol x)]
      [`(read-char)                (compile-read-char c)]
      [`(write-char ,e0)           (compile-write-char e0 c)]
      [`(void)                     (compile-void c)]
      [`(gensym)                   (compile-gensym)]
      [`(symbol->string ,e0)       (compile-symbol->string e0 c)]
      [`(box ,e0)                  (compile-box e0 c)]
      [`(unbox ,e0)                (compile-unbox e0 c)]
      [`(set-box! ,e0 ,e1)         (compile-set-box! e0 e1 c)]
      [`(cons ,e0 ,e1)             (compile-cons e0 e1 c)]
      [`(car ,e0)                  (compile-car e0 c)]
      [`(cdr ,e0)                  (compile-cdr e0 c)]
      [`(add1 ,e0)                 (compile-add1 e0 c)]
      [`(sub1 ,e0)                 (compile-sub1 e0 c)]
      [`(zero? ,e0)                (compile-zero? e0 c)]
      [`(+ ,e0 ,e1)                (compile-+ e0 e1 c)]
      [`(- ,e0 ,e1)                (compile-- e0 e1 c)]
      [`(char=? ,e0 ,e1)           (compile-char=? e0 e1 c)]
      [`(boolean=? ,e0 ,e1)        (compile-boolean=? e0 e1 c)]
      [`(= ,e0 ,e1)                (compile-= e0 e1 c)]
      [`(< ,e0 ,e1)                (compile-< e0 e1 c)]
      [`(<= ,e0 ,e1)               (compile-<= e0 e1 c)]
      [`(eq? ,e0 ,e1)              (compile-eq? e0 e1 c)]
      [`(string-length ,e0)        (compile-string-length e0 c)]
      [`(string-ref ,e0 ,e1)       (compile-string-ref e0 e1 c)]
      [`(make-string ,e0 ,e1)      (compile-make-string e0 e1 c)]
      [`(string=? ,e0 ,e1)         (compile-string=? e0 e1 c)]
      [`(λ ,xs ',l ,e0)            (compile-λ xs l (fvs e) c)]
      [`(,(? type-pred? p) ,e0)    (compile-type-pred p e0 c)]
      [`(char->integer ,e0)        (compile-char->integer e0 c)]
      [`(integer->char ,e0)        (compile-integer->char e0 c)]
      [`(abs ,e0)                  (compile-abs e0 c)]
      [`(list->string ,e0)         (compile-list->string e0 c)]
      [`(arithmetic-shift ,e0 ,e1) (compile-arithmetic-shift e0 e1 c)]
      [`(bitwise-ior ,e0 ,e1)      (compile-bitwise-ior e0 e1 c)]
      [_                           (compile-tail-forms e c)])))

;; CEnv -> Asm
(define (compile-void c)
  `((mov rax ,imm-type-void)))

;; CEnv -> Asm
(define (compile-read-char c)
  (let ((i (length c)))
    `((sub rsp ,(arithmetic-shift i 3))
      (mov rbx rsp)
      (and rbx 8)      ; 0 if 16-byte aligned, 8 if 16-byte align
      (sub rsp rbx)
      (push rbx)       ; now pushed 8 bytes
      (push rdi)       ; pushed 16 bytes
      (call read_char)
      (pop rdi)
      (pop rbx)
      (add rsp rbx)
      (add rsp ,(arithmetic-shift i 3)))))

;; Expr CEnv -> Asm
(define (compile-write-char e0 c)
  (let ((c0 (compile-e e0 c))
        (i (length c)))
    `(,@c0
      ,@assert-char
      (sub rsp ,(arithmetic-shift i 3))
      (mov rbx rsp)
      (and rbx 8)      ; 0 if 16-byte aligned, 8 if 16-byte align
      (sub rsp rbx)
      (push rbx)       ; now pushed 8 bytes
      (push rdi)       ; pushed 16 bytes
      (mov rdi rax)    ; rdi holds first argument
      (call write_char)
      (pop rdi)
      (pop rbx)
      (add rsp rbx)
      (add rsp ,(arithmetic-shift i 3))
      (mov rax ,imm-type-void))))

;; LExpr CEnv -> Asm
;; Compile a form that has a tail position,
;; but which is not itself in tail position
(define (compile-nontail-forms e c)
  (match e
    [`(if ,e0 ,e1 ,e2) (compile-if e0 e1 e2 c)]
    [`(let ,bs ,e )    (compile-let bs e c)]
    [`(letrec ,bs ,e)  (compile-letrec (map first bs) (map second bs) e c)]
    [`(apply ,e0 ,e1)  (compile-apply e0 e1 c)]
    [`(,e0 . ,es)      (compile-call e0 es c)]))

;; LExpr CEnv -> Asm
;; Compile a form that has a tail position,
;; which is itself in tail position
(define (compile-tail-forms e c)
  (match e
    [`(if ,e0 ,e1 ,e2) (compile-tail-if e0 e1 e2 c)]
    [`(let ,bs ,e)     (compile-tail-let bs e c)]
    [`(letrec ,bs ,e)  (compile-tail-letrec (map first bs) (map second bs) e c)]
    [`(apply ,e0 ,e1)  (compile-tail-apply e0 e1 c)]
    [`(,e0 . ,es)      (compile-tail-call e0 es c)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function definitions

;; (Listof Lambda) -> Asm
(define (compile-λ-definitions ls)
  (apply append (map compile-λ-definition ls)))

;; Lambda -> Asm
(define (compile-λ-definition l)
  (match l
    [`(λ ,(list xs ...) ',f ,e0)
     (let ((c0 (compile-tail-e e0 (reverse (append xs (fvs l))))))
       `(,f
         ,@(assert-arity (length xs))
         ,@c0
         ret))]
    [`(λ ,(list-rest xs ... r) ',f ,e0)
     (let ((ys (fvs l)))
       (let ((c0 (compile-tail-e e0 (reverse (append xs (list r) ys))))
             (l1 (gensym 'nonempty_vararg))
             (done (gensym)))
         `(,f
           ,@(assert-arity-at-least (length xs))
           (mov rbx r8)
           (sub rbx ,(length xs))
           (cmp rbx 0)
           (jne ,l1)
           ;; r is empty, so move closure env further out by 1,
           ;; and put '() in new stack slot
           ,@(move-args-back-one (length xs) (length ys))
           (mov rbx ,imm-type-empty)
           (mov (offset rsp ,(- (add1 (length xs)))) rbx)
           (jmp ,done)
           ,l1
           ;; r is non-empty, so allocate a list and move closure env up
           (mov rax rsp)
           (sub rax ,(arithmetic-shift (+ 1 (length xs)) 3))
           ,@(copy-stack-to-list)
           ; update stack to point to list
           (mov (offset rsp ,(- (+ 1 (length xs)))) rdx)

           ; move closure env arguments up on stack
           (sal r8 3)
           (mov r9 rsp)
           (sub r9 ,(arithmetic-shift (add1 (length xs)) 3))
           (sub r9 r8)
           ,@(move-args* ys 0 (+ 2 (length xs)))
           ,done

           ,@c0
           ret)))]))

;; Natural Natural -> Asm
;; Move i arguments at given offset further down stack by 1
(define (move-args-back-one off i)
  (match i
    [0 '()]
    [_
     `((mov rbx (offset rsp ,(- (+ off i))))
       (mov (offset rsp ,(- (+ off (add1 i)))) rbx)
       ,@(move-args-back-one off (sub1 i)))]))

;; (Listof Any) Natural Natural -> Asm
(define (move-args* ys i j)
  (match ys
    ['() '()]
    [(cons y ys)
     `(,@(move-args* ys (add1 i) j)
         (mov rbx (offset r9 ,(- i)))
         (mov (offset rsp ,(- (+ i j))) rbx))]))

;; Natural Natural -> Asm
;; Move i arguments upward on stack by offset off
(define (move-args i off)
  (match i
    [0 '()]
    [_ `(,@(move-args (sub1 i) off)
         (mov rbx (offset rsp ,(- off i)))
         (mov (offset rsp ,(- i)) rbx))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tail forms

;; Compiler -> (LExpr LExpr LExpr CEnv -> Asm)
(define (compile-if/k compile-tail-e)
  (λ (e0 e1 e2 c)
    (let ((c0 (compile-e e0 c))
          (c1 (compile-tail-e e1 c))
          (c2 (compile-tail-e e2 c))
          (l0 (gensym))
          (l1 (gensym)))
      `(,@c0
        (cmp rax ,imm-val-false)
        (je ,l0)
        ,@c1
        (jmp ,l1)
        ,l0
        ,@c2
        ,l1))))

;; LExpr LExpr LExpr CEnv -> Asm
(define compile-if
  (compile-if/k compile-e))
(define compile-tail-if
  (compile-if/k compile-tail-e))

;; Compiler -> ((Listof (List Variable LExpr)) LExpr CEnv -> Asm)
(define (compile-let/k compile-tail-e)
  (λ (bs e1 c)
    (let ((c0 (compile-let-rhs (map second bs) c))
          (c1 (compile-tail-e e1 (append (reverse (map first bs)) c))))
      `(,@c0
        ,@c1))))

;; (Listof (List Variable LExpr)) LExpr CEnv -> Asm
(define compile-let
  (compile-let/k compile-e))
(define compile-tail-let
  (compile-let/k compile-tail-e))

;; (Listof LExpr) CEnv -> Asm
;; Compile the RHSs of a let
(define (compile-let-rhs  es c)
  (match es
    ['() '()]
    [(cons e es)
     (let ((c0 (compile-e e c)))
       `(,@c0
         (mov (offset rsp ,(- (add1 (length c)))) rax)
         ,@(compile-let-rhs es (cons (gensym) c))))]))

;; Compiler -> ((Listof Variable) (Listof Lambda) LExpr CEnv -> Asm)
(define (compile-letrec/k compile-tail-e)
  (λ (fs ls e c)
    (let ((c0 (compile-letrec-λs ls c))
          (c1 (compile-letrec-init fs ls (append (reverse fs) c)))
          (c2 (compile-tail-e e (append (reverse fs) c))))
    `(,@c0
      ,@c1
      ,@c2))))

;; ((Listof Variable) (Listof Lambda) LExpr CEnv -> Asm)
(define compile-letrec
  (compile-letrec/k compile-e))
(define compile-tail-letrec
  (compile-letrec/k compile-tail-e))

;; (Listof Lambda) CEnv -> Asm
;; Create a bunch of uninitialized closures and push them on the stack
(define (compile-letrec-λs ls c)
  (match ls
    ['() '()]
    [(cons l ls)
     (let ((cs (compile-letrec-λs ls (cons #f c)))
           (ys (fvs l)))
       `((lea rax (offset ,(second (third l)) 0))
         (mov (offset rdi 0) rax)
         (mov rax ,(length ys))
         (mov (offset rdi 1) rax)
         (mov rax rdi)
         (or rax ,type-proc)
         (add rdi ,(arithmetic-shift (+ 2 (length ys)) 3))
         (mov (offset rsp ,(- (add1 (length c)))) rax)
         ,@cs))]))

;; (Listof Variable) (Listof Lambda) CEnv -> Asm
(define (compile-letrec-init fs ls c)
  (match fs
    ['() '()]
    [(cons f fs)
     (let ((ys (fvs (first ls)))
           (cs (compile-letrec-init fs (rest ls) c)))
       `((mov r9 (offset rsp ,(- (add1 (lookup f c)))))
         (xor r9 ,type-proc)
         (add r9 16) ; move past label and length
         ,@(copy-env-to-heap ys c 0)
         ,@cs))]))

;; LExpr (Listof LExpr) CEnv -> Asm
;; Common call code
(define (compile-call-common e0 es c)
  (let ((cs (compile-es es (cons #f c)))
        (c0 (compile-e e0 c))
        (i (- (add1 (length c)))))
    `(,@c0
      (mov (offset rsp ,i) rax)
      ,@cs
      (mov rax (offset rsp ,i))
      ,@assert-proc
      (xor rax ,type-proc)
      (mov r8 ,(length es)))))

;; LExpr (Listof LExpr) CEnv -> Asm
(define (compile-call e0 es c)
  (let ((stack-size (arithmetic-shift (length c) 3)))
    `(,@(compile-call-common e0 es c)
      ;; Non-tail call: adjust rsp and call
      (mov rcx rsp) ; start of stack in rcx
      (sub rcx ,stack-size)
      (sub rcx ,(arithmetic-shift (+ 2 (length es)) 3))
      ,@(copy-closure-env-to-stack)
      (sub rsp ,stack-size)
      (call (offset rax 0))
      (add rsp ,stack-size))))

;; LExpr (Listof LExpr) CEnv -> Asm
(define (compile-tail-call e0 es c)
  (let ((i (- (add1 (length c)))))
    `(,@(compile-call-common e0 es c)
      ;; Tail-call: move args down and jump
      ,@(move-args (length es) i)
      (mov rcx rsp) ; start of stack in rcx
      (sub rcx ,(arithmetic-shift (+ 1 (length es)) 3))
      ,@(copy-closure-env-to-stack)
      (jmp (offset rax 0)))))

;; (Listof LExpr) CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (let ((c0 (compile-e e c))
           (cs (compile-es es (cons #f c))))
       `(,@c0
         (mov (offset rsp ,(- (add1 (length c)))) rax)
         ,@cs))]))

;; TODO (bonus): implement propert tail calls for apply
(define (compile-tail-apply e0 e1 c)
  (compile-apply e0 e1 c))

;; LExpr LExpr CEnv -> Asm
(define (compile-apply e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c)))
        (i (- (add1 (length c))))
        (stack-size (arithmetic-shift (length c) 3)))
    `(,@c0
      (mov (offset rsp ,i) rax)

      ,@c1
      ;; rax holds a list, copy to the stack
      (mov rbx rsp)
      (add rbx ,(- (+ stack-size 16))) ; past rp, c, & fp
      ,@(copy-list-to-stack)

      (mov rax (offset rsp ,i))
      ,@assert-proc
      (xor rax ,type-proc)

      ;; setup closure environment arguments
      (mov rcx rsp)                ; start of stack
      (sub rcx ,(+ stack-size 16)) ; skip past rp, c, function
      (mov r8 rdx)                 ; rdx holds length of list
      (sal r8 3)                   ; mult by 8
      (sub rcx r8)                 ; skip past args
      ,@(copy-closure-env-to-stack)
      (sub rsp ,stack-size)

      (mov r8 rdx)                 ; communicate arity
      (call (offset rax 0))
      (add rsp ,stack-size))))

;; assume: rbx points to next stack frame
;;         rax holds list to copy
;;         leaves length in rdx
(define (copy-list-to-stack)
  (let ((loop (gensym 'apply_loop))
        (done (gensym 'apply_done)))
    `((mov rdx 0) ; length initialized to 0.
      ,loop
      ;; if empty, done
      (mov rcx rax)
      (and rcx ,imm-type-mask)
      (cmp rcx ,imm-type-empty)
      (je ,done)

      ;; if not cons, err
      (mov rcx rax)
      (and rcx ,result-type-mask)
      (cmp rcx ,type-pair)
      (jne err)

      ;; otherwise copy car, mov cdr to rax, and loop
      (add rdx 1)
      (xor rax ,type-pair)
      (mov rcx (offset rax 0))
      (mov (offset rbx 0) rcx)
      (mov rax (offset rax 1))
      (sub rbx 8)
      (jmp ,loop)

      ,done)))

;; assume: rbx holds length of list
;;         rax pointer to stack start of list
;; leaves result value in rdx
(define (copy-stack-to-list)
  (let ((loop (gensym 'vararity_loop))
        (last (gensym 'vararity_last))
        (done (gensym 'vararity_done)))

    `(;; if len = 0, done
      (cmp rbx 0)
      (mov rdx ,imm-type-empty)
      (je ,done)

      (mov rdx rdi)
      (or rdx ,type-pair)
      ,loop
      ;; if len = 1, last
      (cmp rbx 1)
      (je ,last)

      ;; else
      (mov r9 (offset rax 0))
      (mov (offset rdi 0) r9)

      (mov r9 rdi)
      (add r9 ,(+ 16 type-pair))
      (mov (offset rdi 1) r9)
      (add rdi 16)
      (sub rax 8)
      (sub rbx 1)
      (jmp ,loop)

      ,last
      (mov r9 (offset rax 0))
      (mov (offset rdi 0) r9)

      (mov r9 ,imm-type-empty)
      (mov (offset rdi 1) r9)
      (add rdi 16)

      ,done)))

;; Natural -> Asm
;; Arity info store in r8
(define (assert-arity n)
  `((cmp r8 ,n)
    (jne err)))

;; Natural -> Asm
;; Arity info store in r8
(define (assert-arity-at-least n)
  `((cmp r8 ,n)
    (jl err)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other forms

;; (Listof Variable) Label (Listof Variable) CEnv -> Asm
(define (compile-λ xs f ys c)
  `(;; Save label address
    (lea rax (offset ,f 0))
    (mov (offset rdi 0) rax)

    ;; Save the environment
    (mov r8 ,(length ys))
    (mov (offset rdi 1) r8)
    (mov r9 rdi)
    (add r9 16)
    ,@(copy-env-to-heap ys c 0)

    ;; Return a pointer to the closure
    (mov rax rdi)
    (or rax ,type-proc)
    (add rdi ,(arithmetic-shift (+ 2 (length ys)) 3))))

;; (Listof Variable) CEnv Natural -> Asm
;; Pointer to beginning of environment in r9
(define (copy-env-to-heap fvs c i)
  (match fvs
    ['() '()]
    [(cons x fvs)
     `((mov r8 (offset rsp ,(- (add1 (lookup x c)))))
       (mov (offset r9 ,i) r8)
       ,@(copy-env-to-heap fvs c (add1 i)))]))

;; -> Asm
;; Copy closure's (in rax) env to stack in rcx
(define (copy-closure-env-to-stack)
  (let ((copy-loop (gensym 'copy_closure))
        (copy-done (gensym 'copy_done)))
    `((mov r9 (offset rax 1)) ; length
      (mov r10 rax)
      (add r10 16)             ; start of env
      ,copy-loop
      (cmp r9 0)
      (je ,copy-done)
      (mov rbx (offset r10 0))
      (mov (offset rcx 0) rbx)
      (sub r9 1)
      (add r10 8)
      (sub rcx 8)
      (jmp ,copy-loop)
      ,copy-done)))


;; Any -> Boolean
(define (imm? x)
  (or (integer? x)
      (boolean? x)
      (char? x)
      (equal? ''() x)))

;; Any -> Boolean
(define (type-pred? x)
  (memq x '(integer?
            char?
            empty?
            string?
            boolean?
            box?
            cons?
            symbol?
            eof-object?)))

;; Imm -> Asm
(define (compile-imm i)
  `((mov rax ,(imm->bits i))))

;; Imm -> Integer
(define (imm->bits i)
  (match i
    [(? integer? i) (arithmetic-shift i imm-shift)]
    [(? char? c)    (+ (arithmetic-shift (char->integer c) imm-shift) imm-type-char)]
    [(? boolean? b) (if b imm-val-true imm-val-false)]
    [''()           imm-type-empty]))

;; LExpr LExpr CEnv -> Asm
(define (compile-char=? e0 e1 c)
  (compile-type-equiv 'char? 'je e0 e1 c))
(define (compile-boolean=? e0 e1 c)
  (compile-type-equiv 'boolean? 'je e0 e1 c))
(define (compile-= e0 e1 c)
  (compile-type-equiv 'integer? 'je e0 e1 c))
(define (compile-< e0 e1 c)
  (compile-type-equiv 'integer? 'jg e0 e1 c))
(define (compile-<= e0 e1 c)
  (compile-type-equiv 'integer? 'jge e0 e1 c))

;; TypePred Op LExpr LExpr CEnv -> Asm
(define (compile-type-equiv t j e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c)))
        (i  (- (add1 (length c))))
        (l0 (gensym)))
    `(,@c0
      ,@(assert-type t)
      (mov (offset rsp ,i) rax)
      ,@c1
      ,@(assert-type t)
      (cmp rax (offset rsp ,i))
      (mov rax ,imm-val-true)
      (,j ,l0)
      (mov rax ,imm-val-false)
      ,l0)))

;; LExpr LExpr CEnv -> Asm
(define (compile-eq? e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c)))
        (i  (- (add1 (length c))))
        (l0 (gensym)))
    `(,@c0
      (mov (offset rsp ,i) rax)
      ,@c1
      (cmp rax (offset rsp ,i))
      (mov rax ,imm-val-true)
      (je ,l0)
      (mov rax ,imm-val-false)
      ,l0)))

;; LExpr CEnv -> Asm
(define (compile-string-length e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-string
      (xor rax ,type-string)
      (mov rax (offset rax 0)))))

;; LExpr LExpr CEnv -> Asm
(define (compile-string-ref e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c)))
        (i (- (add1 (length c)))))
    `(,@c0
      ,@assert-string
      (xor rax ,type-string)
      (mov (offset rsp ,i) rax)
      ,@c1
      ,@assert-integer
      ,@(assert-valid-index i)
      (sar rax ,imm-count-bits) ;; hold i * 8
      (add rax 8) ;; skip past length
      (add rax (offset rsp ,(- (add1 (length c)))))
      (mov rax (offset rax 0)))))

;; LExpr LExpr CEnv -> Asm
(define (compile-make-string e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c)))
        (done (gensym 'make_string_done))
        (loop (gensym 'make_string_loop))
        (i (- (add1 (length c)))))
    `(,@c0
      ,@assert-natural
      (mov (offset rsp ,i) rax)
      ,@c1
      ,@assert-char
      (mov rbx (offset rsp ,i))
      (mov (offset rdi 0) rbx)
      (mov rcx rdi)
      (sar rbx ,(- imm-shift 3)) ; rbx = len*8
      (add rdi 8)
      (add rbx rdi)
      ,loop
      (cmp rbx rdi)
      (je ,done)
      (mov (offset rdi 0) rax)
      (add rdi 8)
      (jmp ,loop)
      ,done
      (mov rax rcx)
      (or rax ,type-string))))

;; LExpr LExpr CEnv -> Asm
(define (compile-string=? e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c)))
        (i (- (add1 (length c))))
        (rf   (gensym 'retf))
        (rt   (gensym 'rett))
        (done (gensym 'done))
        (loop (gensym 'loop)))

    `(,@c0
      ,@assert-string
      (xor rax ,type-string)
      (mov (offset rsp ,i) rax)
      ,@c1
      ,@assert-string
      (xor rax ,type-string)
      (mov rbx (offset rsp ,i))

      ;; check lengths are equal
      (mov rcx (offset rax 0))
      (cmp rcx (offset rbx 0))
      (jne ,rf)

      ;; loop through characters and compare each
      (sar rcx ,imm-shift)
      ,loop
      (cmp rcx 0)
      (je ,rt)
      (sub rcx 1)
      (add rax 8)
      (add rbx 8)
      (mov r9  (offset rax 0))
      (mov r10 (offset rbx 0))
      (cmp r9 r10)
      (jne ,rf)
      (jmp ,loop)
      ,rt
      (mov rax ,imm-val-true)
      (jmp ,done)
      ,rf
      (mov rax ,imm-val-false)
      ,done)))

;; Symbol -> Asm
(define (compile-symbol s)
  (let ((s (symbol->string s)))
    (let ((c (compile-string-chars (string->list s) 1)))
      `(,@c
        (mov rax ,(imm->bits (string-length s)))
        (mov (offset rdi 0) rax)
        (mov rax rdi)
        (add rax ,type-symbol)
        (add rdi ,(arithmetic-shift (add1 (string-length s)) 3))))))

;; -> Asm
(define (compile-gensym)
  (compile-symbol 'gensym))

;; LExpr CEnv -> Asm
(define (compile-symbol->string e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-symbol
      (xor rax ,type-symbol)
      (or rax ,type-string))))

;; String -> Asm
(define (compile-string s)
  (let ((c (compile-string-chars (string->list s) 1)))
    `(,@c
      (mov rax ,(imm->bits (string-length s)))
      (mov (offset rdi 0) rax)
      (mov rax rdi)
      (add rax ,type-string)
      (add rdi ,(arithmetic-shift (add1 (string-length s)) 3)))))

;; (Listof Char) Natural -> Asm
(define (compile-string-chars cs i)
  (match cs
    ['() '()]
    [(cons c cs)
     `((mov rax ,(imm->bits c))
       (mov (offset rdi ,i) rax)
       ,@(compile-string-chars cs (add1 i)))]))

;; Variable CEnv -> Asm
(define (compile-var x c)
  (let ((i (lookup x c)))
    `((mov rax (offset rsp ,(- (add1 i)))))))

;; LExpr CEnv -> Asm
(define (compile-box e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      (mov (offset rdi 0) rax)
      (mov rax rdi)
      (or rax ,type-box)
      (add rdi 8)))) ; allocate 8 bytes

;; LExpr CEnv -> Asm
(define (compile-unbox e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-box
      (xor rax ,type-box)
      (mov rax (offset rax 0)))))

;; LExpr CEnv -> Asm
(define (compile-set-box! e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c)))
        (i (- (add1 (length c)))))
    `(,@c0
      ,@assert-box
      (xor rax ,type-box)
      (mov (offset rsp ,i) rax)
      ,@c1
      (mov rbx (offset rsp ,i))
      (mov (offset rbx 0) rax)
      (mov rax ,imm-type-void))))

;; LExpr LExpr CEnv -> Asm
(define (compile-cons e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c))))
    `(,@c0
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c1
      (mov (offset rdi 1) rax)
      (mov rax (offset rsp ,(- (add1 (length c)))))
      (mov (offset rdi 0) rax)
      (mov rax rdi)
      (or rax ,type-pair)
      (add rdi 16))))

;; LExpr CEnv -> Asm
(define (compile-car e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-pair
      (xor rax ,type-pair) ; untag
      (mov rax (offset rax 0)))))

;; LExpr CEnv -> Asm
(define (compile-cdr e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-pair
      (xor rax ,type-pair) ; untag
      (mov rax (offset rax 1)))))

;; LExpr CEnv -> Asm
(define (compile-add1 e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer
      (add rax ,(arithmetic-shift 1 imm-shift)))))

;; LExpr CEnv -> Asm
(define (compile-sub1 e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer
      (sub rax ,(arithmetic-shift 1 imm-shift)))))

;; LExpr CEnv -> Asm
(define (compile-zero? e0 c)
  (let ((c0 (compile-e e0 c))
        (l0 (gensym))
        (l1 (gensym)))
    `(,@c0
      ,@assert-integer
      (cmp rax 0)
      (mov rax ,imm-val-false)
      (jne ,l0)
      (mov rax ,imm-val-true)
      ,l0)))

;; LExpr LExpr CEnv -> Asm
(define (compile-+ e0 e1 c)
  (let ((c1 (compile-e e1 c))
        (c0 (compile-e e0 (cons #f c))))
    `(,@c1
      ,@assert-integer
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c0
      ,@assert-integer
      (add rax (offset rsp ,(- (add1 (length c))))))))

;; LExpr LExpr CEnv -> Asm
(define (compile-- e0 e1 c)
  (let ((c1 (compile-e e1 c))
        (c0 (compile-e e0 (cons #f c))))
    `(,@c1
      ,@assert-integer
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c0
      ,@assert-integer
      (sub rax (offset rsp ,(- (add1 (length c))))))))

;; TypePred LExpr CEnv -> Asm
(define (compile-type-pred p e0 c)
  (let ((c0 (compile-e e0 c))
        (l0 (gensym)))
    `(,@c0
      (and rax ,(type-pred->mask p))
      (cmp rax ,(type-pred->tag p))
      (mov rax ,imm-val-false)
      (jne ,l0)
      (mov rax ,imm-val-true)
      ,l0)))

;; TypePred -> Integer
(define (type-pred->mask p)
  (match p
    [(or 'box? 'cons? 'string? 'procedure? 'symbol?) result-type-mask]
    [_ imm-type-mask]))

;; TypePred -> Integer
(define (type-pred->tag p)
  (match p
    ['box?       type-box]
    ['cons?      type-pair]
    ['string?    type-string]
    ['procedure? type-proc]
    ['symbol?     type-symbol]
    ['integer?   imm-type-int]
    ['empty?     imm-type-empty]
    ['char?      imm-type-char]
    ['boolean?   imm-type-bool]
    ['eof-object? imm-type-eof]))

;; LExpr CEnv -> Asm
(define (compile-char->integer e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-char
      (xor rax ,imm-type-char))))

;; LExpr CEnv -> Asm
(define (compile-integer->char e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer-codepoint
      (xor rax ,imm-type-char))))

;; LExpr CEnv -> Asm
(define (compile-abs e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer
      (mov rbx rax)
      (neg rax)
      (cmovl rax rbx))))

;; LExpr CEnv -> Asm
(define (compile-list->string e0 c)
  (let ((c0 (compile-e e0 c))
        (mt (gensym 'mt))
        (done (gensym 'done))
        (loop (gensym 'loop))
        (finish (gensym 'finish)))
    `(,@c0
      (cmp rax ,imm-type-empty)
      (je ,mt)

      ,@assert-pair
      (xor rax ,type-pair)
      (mov r10 0)
      (mov r9 rdi)
      (add r9 8)
      (mov rcx rax)

      ,loop
      (mov rax (offset rcx 0))
      ,@assert-char
      (add r10 1)
      (mov (offset r9 0) rax)
      (mov rax (offset rcx 1))
      (cmp rax ,imm-type-empty)
      (je ,finish)
      ,@assert-pair
      (xor rax ,type-pair)
      (mov rcx rax)
      (add r9 8)
      (jmp ,loop)

      ,finish
      (sal r10 ,imm-shift)
      (mov (offset rdi 0) r10)
      (mov rax rdi)
      (mov rdi r9)
      (add rdi 8)
      (or rax ,type-string)
      (jmp ,done)

      ,mt
      (mov rax 0)
      (mov (offset rdi 0) rax)
      (mov rax rdi)
      (or rax ,type-string)
      (add rdi 8)
      ,done)))

;; LExpr LExpr CEnv -> Asm
(define (compile-arithmetic-shift e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c)))
        (i  (- (add1 (length c))))
        (neg (gensym 'neg))
        (done (gensym 'done)))
    `(,@c0
      ,@assert-integer
      (mov (offset rsp ,i) rax)
      ,@c1
      ,@assert-integer
      (mov rcx rax)
      (sar rcx ,imm-shift)
      (mov rax (offset rsp ,i))
      (cmp rcx 0)
      (jle ,neg)
      (sal rax cl)
      (jmp ,done)
      ,neg
      (neg rcx)
      (sar rax ,imm-shift)
      (sar rax cl)
      (sal rax ,imm-shift)
      ,done)))

;; LExpr LExpr CEnv -> Asm
(define (compile-bitwise-ior e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c)))
        (i  (- (add1 (length c)))))
    `(,@c0
      ,@assert-integer
      (mov (offset rsp ,i) rax)
      ,@c1
      ,@assert-integer
      (mov rbx (offset rsp ,i))
      (or rax rbx))))


;; Variable CEnv -> Natural
(define (lookup x cenv)
  (match cenv
    ['() (error "unbound variable" x)]
    [(cons y cenv)
     (match (eq? x y)
       [#t (length cenv)]
       [#f (lookup x cenv)])]))

;; TypePred -> Asm
(define (assert-type p)
  `((mov rbx rax)
    (and rbx ,(type-pred->mask p))
    (cmp rbx ,(type-pred->tag p))
    (jne err)))

(define assert-integer (assert-type 'integer?))
(define assert-box     (assert-type 'box?))
(define assert-pair    (assert-type 'cons?))
(define assert-string  (assert-type 'string?))
(define assert-symbol  (assert-type 'symbol?))
(define assert-char    (assert-type 'char?))
(define assert-proc    (assert-type 'procedure?))

;; Asm
(define assert-natural
  `(,@assert-integer
    (cmp rax -1)
    (jle err)))

;; Asm
(define assert-integer-codepoint
  `((mov rbx rax)
    (and rbx ,imm-type-mask)
    (cmp rbx 0)
    (jne err)
    (cmp rax ,(arithmetic-shift -1 imm-shift))
    (jle err)
    (cmp rax ,(arithmetic-shift #x10FFFF imm-shift))
    (mov rbx rax)
    (sar rbx ,(+ 11 imm-shift))
    (cmp rbx #b11011)
    (je err)))

;; Asm
(define (assert-valid-index i)
  `((cmp rax ,(arithmetic-shift -1 imm-shift))
    (jle err)
    (mov rbx (offset rsp ,i))
    (mov rbx (offset rbx 0))
    (cmp rbx rax)
    (jle err)))

;; LExpr -> LExpr
;; Intern symbols
(define (intern-symbols e)
  (let ((se (symbol-env e)))
    (let ((e0 (replace-symbols e se)))
      `(let ,(symbol-env->bindings se)
         ,e0))))

;; LExpr -> SEnv
(define (symbol-env e)
  (map (λ (s) (list s (gensym "symb")))
       (symbols e)))

;; SE -> Bindings
(define (symbol-env->bindings se)
  (map (λ (l) (list (second l) (list 'quote (first l)))) se))

;; LExpr -> (Listof Symbol)
;; Unique list of all symbols occuring in program
(define (symbols e)
  (define (symbols e)
    (match e
      [(? variable? x)        '()]
      [(? imm? i)             '()]
      [(? string? s)          '()]
      [`',x                   (list x)]
      [`(,(? prim0?))         '()]
      [`(,(? prim1?) ,e0)     (symbols e0)]
      [`(,(? prim2?) ,e0 ,e1) (append (symbols e0) (symbols e1))]
      [`(if ,e0 ,e1 ,e2)      (append (symbols e0) (symbols e1) (symbols e2))]
      [`(apply ,e0 ,e1)       (append (symbols e0) (symbols e1))]
      [`(let ,bs ,e0)
       (apply append (symbols e0) (map (compose symbols second) bs))]
      [`(letrec ,bs ,e0)
       (apply append (symbols e0) (map (compose symbols second) bs))]
      [`(λ ,xs ,e0)
       (symbols e0)]
      [`(,e . ,es)
       (append (symbols e) (apply append (map symbols es)))]))
  (remove-duplicates (symbols e)))

;; LExpr SEnv -> LExpr
;; Replace symbols according to given symbol environment
(define (replace-symbols e se)
  (match e
    [(? variable? x)          x]
    [(? imm? i)               i]
    [(? string? s)            s]
    [`',x                     (second (assq x se))]
    [`(,(? prim0? p))         `(,p)]
    [`(,(? prim1? p) ,e0)     `(,p ,(replace-symbols e0 se))]
    [`(,(? prim2? p) ,e0 ,e1)
     `(,p ,(replace-symbols e0 se) ,(replace-symbols e1 se))]
    [`(if ,e0 ,e1 ,e2)
     `(if ,(replace-symbols e0 se)
          ,(replace-symbols e1 se)
          ,(replace-symbols e2 se))]
    [`(apply ,e0 ,e1)
     `(apply ,(replace-symbols e0 se)
             ,(replace-symbols e1 se))]
    [`(let ,bs ,e0)
     `(let ,(zip (map first bs)
                 (map (compose (λ (e) (replace-symbols e se)) second) bs))
        ,(replace-symbols e0 se))]
    [`(letrec ,bs ,e0)
     `(letrec ,(zip (map first bs)
                    (map (compose (λ (e) (replace-symbols e se)) second) bs))
        ,(replace-symbols e0 se))]
    [`(λ ,xs ,e0)
     `(λ ,xs ,(replace-symbols e0 se))]
    [`(,e . ,es)
     `(,(replace-symbols e se) . ,(map (λ (e) (replace-symbols e se)) es))]))

(define (zip xs ys)
  (map list xs ys))

;; Expr -> Expr
;; Implement the standard library functions
(define (stdlib e)
  `(letrec ((append
             (λ ls
               (if (empty? ls)
                   '()
                   (if (empty? (car ls))
                       (apply append (cdr ls))
                       (if (empty? (cdr ls))
                           (car ls)
                           (cons (car (car ls))
                                 (append (cdr (car ls))
                                         (apply append (cdr ls)))))))))
            (list
             (λ ls ls))

            (list?
             (λ (x)
               (if (empty? x)
                   #t
                   (if (cons? x)
                       (list? (cdr x))
                       #f))))

            (first
             (λ (x)
               (if (list? x)
                   (car x)
                   (car '()))))

            (second
             (λ (x)
               (if (list? x)
                   (car (cdr x))
                   (car '()))))

            (rest
             (λ (x)
               (if (list? x)
                   (cdr x)
                   (car '()))))

            (reverse
             (λ (x)
               (letrec ((rev/acc
                         (λ (x a)
                           (if (empty? x)
                               a
                               (rev/acc (cdr x) (cons (car x) a))))))
                 (rev/acc x '()))))

            (not
             (λ (x)
               (if x #f #t)))

            (compose
             (λ (f g)
               (λ (x)
                 (f (g x)))))

            (map
             ;; NOTE: only works for a single list
             (λ (f xs)
               (if (empty? xs)
                   '()
                   (cons (f (car xs)) (map f (cdr xs))))))

            (symbol=?
             (λ (s1 s2)
               (if (and (symbol? s1)
                        (symbol? s2))
                   (eq? s1 s2)
                   (car '()))))

            (memq
             (λ (x xs)
               (if (empty? xs)
                   #f
                   (if (eq? x (car xs))
                       xs
                       (memq x (cdr xs))))))

            (length
             (λ (xs)
               (if (empty? xs)
                   0
                   (add1 (length (cdr xs))))))

            (remove-duplicates
             (λ (xs)
               (if (empty? xs)
                   '()
                   (cons (car xs)
                         (remove (car xs)
                                 (remove-duplicates (cdr xs)))))))

            (remove
             (λ (x xs)
               (if (empty? xs)
                   '()
                   (if (equal? x (car xs))
                       (cdr xs)
                       (cons (car xs) (remove x (cdr xs)))))))

            (member
             (λ (x xs)
               (if (empty? xs)
                   #f
                   (if (equal? (car xs) x)
                       xs
                       (member x (cdr xs))))))

            (string->list
             (λ (s)
               (letrec ((loop (λ (i)
                                (if (= i (string-length s))
                                    '()
                                    (cons (string-ref s i)
                                          (loop (add1 i)))))))
                 (loop 0))))

            (string-append
             (λ (s1 s2)
               (list->string
                (append (string->list s1) (string->list s2)))))

            (foldr
             (λ (f b xs)
               (if (empty? xs)
                   b
                   (f (car xs) (foldr f b (cdr xs))))))

            (for-each
             (λ (f xs)
               (if (empty? xs)
                   (void)
                   (let ((_ (f (car xs))))
                     (for-each f (cdr xs))))))

            (display ;; only works on strings
             (λ (s)
               (for-each (λ (c) (write-char c))
                         (string->list s))))

            (equal? ; loops on circular values
             (λ (x y)
               (or (eq? x y)
                   (cond [(and (string? x) (string? y)) (string=? x y)]
                         [(and (box? x) (box y))
                          (equal? (unbox x) (unbox y))]
                         [(and (cons? x) (cons? y))
                          (and (equal? (car x) (car y))
                               (equal? (cdr x) (cdr y)))]
                         [else #f])))))
     ,e))
