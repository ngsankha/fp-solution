#lang racket
(provide (all-defined-out))
(require "printer.rkt" racket/runtime-path)
(define-runtime-path dir "..")

;; Asm -> String
;; Interpret (by assemblying, linking, and exec'ing) x86-64 code
;; Assume: starts with entry point run-time expects
(define (asm-interp a)
  (with-input-from-string (asm-interp/io a "") read))

;; Asm String -> String
;; Interpret (by assemblying, linking, and exec'ing) x86-64 code
;; Assume: starts with entry point run-time expects
(define (asm-interp/io a str)
  (let* ((t.s (make-temporary-file "nasm~a.s"))
         (t.run (path-replace-extension t.s #".run")))
    (with-output-to-file t.s
      #:exists 'truncate
      (λ ()
        (asm-display a)))
    (system (format "(cd ~a && make -s ~a) 2>&1 >/dev/null" dir t.run))
    (delete-file t.s)
    (match 
        (process/ports #f
                       (open-input-string str)
                       (current-error-port)
                       (path->string t.run))
      [(list in out pid err status)
       (begin
         (status 'wait)
         (delete-file t.run)
         (port->string in #:close? #t))])))
