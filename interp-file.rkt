#lang racket
(provide (all-defined-out))
(require (only-in "interp.rkt" interp) "syntax.rkt")

;; String -> Void
;; Parse and interpret contents of given filename,
;; print result on stdout
(define (main fn)
  (with-input-from-file fn
    (λ ()
      (let ((p (read-program)))
        (unless (and (prog? p) (closed? p))
          (error "syntax error" p))
        (writeln (interp p))))))

(define (read-program)
  (regexp-match "^#lang racket" (current-input-port))
  (read))
