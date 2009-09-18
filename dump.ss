#!/usr/bin/env mzscheme
;; Disassemble ABC file
;;
;; -abc option allows you to see the abc file structure directly.
;;
;; ./dump.ss filename.abc
;; ./dump.ss -abc filename.abc

#lang scheme
(include "instruction.k")
(include "abc.k")

(define (print-abc-file infile is-abc)
  (call-with-input-file infile
    (if is-abc
	(lambda (port) (pretty-print (read-abc port)))
	(lambda (port) (pretty-print (read-asm port))))))

(let ((args (vector->list (current-command-line-arguments))))
  (cond
   ((< (length args) 1) (error "Usage: ./dump [-abc] filename.abc"))
   ((equal? (car args) "-abc") (print-abc-file (cadr args) #t))
   (else (print-abc-file (car args) #f))))
