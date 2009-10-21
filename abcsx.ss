#!/usr/bin/env mzscheme

;; Copyright (c) 2009 Takashi Yamamiya
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

#lang scheme
(require srfi/78) ; Lightweight testing
(require srfi/1) ; List library

(include "instruction.k")
(include "abc.k")
(include "test.scm")

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

(define (test _ __)
  (check-set-mode! 'report-failed)
  (run-test)
  )

(define (asm infile is-abc)
  (if (string? infile)
      (let ((outfile (string-append infile ".abc")))
	(write-file (read-file infile) outfile))
      (usage)))

(define (dump infile is-abc)
  (if (string? infile)
      (call-with-input-file infile
	(if is-abc
	    (lambda (port) (pretty-print (read-abc port)))
	    (lambda (port) (pretty-print (read-asm port)))))
      (usage)))

(define usage
  (lambda ()
    (display "Usage: abcsx [-asm | -dump] [-abc] filename\n")
    (display "Usage: abcsx -test\n")
    (exit 1)))
  
(define main
  (lambda (args)
    (let ((infile '())
	  (command asm)
	  (is-abc #f))
      (for-each
       (lambda (opt)
	 (cond
	  ((equal? opt "-abc") (set! is-abc #t))
	  ((equal? opt "-test") (set! command test))
	  ((equal? opt "-asm") (set! command asm))
	  ((equal? opt "-dump") (set! command dump))
	  (#t (set! infile opt))))
       args)
      (command infile is-abc))))

(main (vector->list (current-command-line-arguments)))
