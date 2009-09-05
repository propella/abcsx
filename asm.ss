#!/usr/bin/env mzscheme
;; ./asm.ss filename.sx

#lang scheme
(require "abc.ss")

;;; read a S-expression file
(define (read-file infile)
  (call-with-input-file infile
    (lambda (port) (read port))))

;;; write a ABC file
(define (write-file asm outfile)
  (call-with-output-file outfile
    (lambda (port)
      (write-asm asm port))
    #:exists 'replace))

;;; main
(define (asm infile outfile)
  (write-file (read-file infile) outfile))

(let*
    ((infile (vector-ref (current-command-line-arguments) 0))
     (outfile (string-append infile ".abc")))
  (asm infile outfile))
