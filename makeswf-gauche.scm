#!/usr/bin/env gosh

;; Copyright (c) 2010 Takashi Yamamiya
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

;; ./abcsx-gauche.scm examples/textField.sx
;; ./makeswf-gauche.scm > test.swf

(use srfi-1)
(use srfi-4)
(use binary.io)
(use file.util)

(define SPEC
  '((frame-size . (3000 4000))
    (frame-rate . 30)
    (frame-count . 1)
    (tags
     (file-attribute)
     (do-abc . "examples/textField.sx.abc")
     (symbol-class . "Hello")
     (set-background-color . (#xfa #x64 #x64))
     (show-frame)
     (end-tag))))

(define call-with-output-bytes
  (lambda (proc)
    (string->u8vector (call-with-output-string proc))))

(define bytes u8vector)
(define bytes-length u8vector-length)
(define write-bytes write-block)

(define int
  (lambda (n) (inexact->exact (truncate n))))

(define nbits-unsigned
  (lambda (lst)
    (let ((max (fold max 0 lst)))
      (+ (int (/ (log max) (log 2))) 1))))
(define nbits-signed
  (lambda (lst) (+ (nbits-unsigned lst) 1)))

(define push-bits
  (lambda (num nbits rest)
    (if (eq? nbits 0)
        rest
        (push-bits (ash num -1)
                     (- nbits 1)
                     (cons (logand num 1) rest)))))

;; Make a integer from the first nbits of list
(define bits->u8
  (lambda (lst)
    (let ((byte (take lst 8)))
      (fold (lambda (x n) (+ (* n 2) x)) 0 byte))))

;; Write a list of bit as binary data postfix 8 bits padding.
(define *write-bits
  (lambda (lst out)
    (if (pair? lst)
        (begin 
          (write-u8 (bits->u8 lst) out)
          (*write-bits (drop lst 8) out)))))

(define write-bits
  (lambda (bits out)
    (let* ((padding (- 8 (modulo (length bits) 8)))
           (filled (append bits (take '(0 0 0 0 0 0 0 0) padding))))
      (*write-bits filled out))))

(define write-fixed8dot8
  (lambda (n out)
    (let* ((integer-part (inexact->exact (truncate n)))
           (small-part (inexact->exact (truncate (* (/ (- n integer-part) 1) 256)))))
      (write-u8 small-part out)
      (write-u8 integer-part out))))

(define write-rectangle
  (lambda (xmin xmax ymin ymax out)
    (let ((nbits (nbits-signed (list xmin xmax ymin ymax))))
      (write-bits
        (push-bits nbits 5
          (push-bits xmin nbits
            (push-bits xmax nbits
              (push-bits ymin nbits
                (push-bits ymax nbits ())))))
        out))))

(define record-header-body
  (lambda (type body out)
    (let ((size (bytes-length body)))
      (if (< size 63)
          (let ((code-length (logior (ash type 6) size)))
            (write-u16 code-length out 'little-endian)
            (write-bytes body out))
          (let ((code-length (logior (ash type 6) 63)))
            (write-u16 code-length out 'little-endian)
            (write-u32 size out 'little-endian)
            (write-bytes body out))))))

(define write-rgb
  (lambda (rgb out)
    (write-u8 (car rgb) out)
    (write-u8 (cadr rgb) out)
    (write-u8 (caddr rgb) out)))

(define write-string-type
  (lambda (str out)
    (display str out)
    (write-u8 0 out)))

(define write-symbol-class
  (lambda (name1 out)
    (let ((num-symbols 1)
          (tag1 0))
      (record-header-body 76
                          (call-with-output-bytes
                           (lambda (p)
                             (write-u16 num-symbols p 'little-endian)
                             (write-u16 tag1 p 'little-endian)
                             (write-string-type name1 p)))
                          out))))

;; Make a byte array from file
(define read-bytes-from-file
  (lambda (infile)
    (let ((size (file-size infile)))
      (call-with-input-file infile
        (lambda (input)
          (string->u8vector (read-block size input)))))))

(define write-do-abc
  (lambda (infile out)
    (let ((abc-data (read-bytes-from-file infile))
          (flags 0))
      (record-header-body 82
                          (call-with-output-bytes
                           (lambda (p)
                             (write-u32 flags p 'little-endian)
                             (write-string-type infile p)
                             (write-bytes abc-data p)))
                           out))))

      (define write-tag
  (lambda (tag out)
    (let ((name (car tag)))
      (cond
       ((eq? name 'end-tag) (record-header-body 0 (bytes) out))
       ((eq? name 'show-frame) (record-header-body 1 (bytes) out))
       ((eq? name 'set-background-color)
        (record-header-body 9
                            (call-with-output-bytes
                             (lambda (p) (write-rgb (cdr tag) p)))
                            out))
       ((eq? name 'file-attribute) (record-header-body 69 (bytes #b00001000 0 0 0) out))
       ((eq? name 'symbol-class) (write-symbol-class (cdr tag) out))
       ((eq? name 'do-abc) (write-do-abc (cdr tag) out))
       ('else (error "unknown tag:" name))))))

(define write-tags
  (lambda (tags out) (map (lambda (t) (write-tag t out)) tags)))

(define write-body
  (lambda (spec out)
    (let* ((frame-size (cdr (assoc 'frame-size spec)))
           (frame-rate (cdr (assoc 'frame-rate spec)))
           (frame-count (cdr (assoc 'frame-count spec)))
           (tags (cdr (assoc 'tags spec)))
           (tags-data (call-with-output-bytes
                       (lambda (p) (write-tags tags p)))))
      (write-rectangle 0 (car frame-size) 0 (cadr frame-size) out)
      (write-fixed8dot8 frame-rate out)
      (write-u16 frame-count out 'little-endian)
      (write-bytes tags-data out))))

(define write-swf
  (lambda (spec out)
    (let* ((version 10)
           (body (call-with-output-bytes
                  (lambda (p) (write-body spec p))))
           (body-length (+ (bytes-length body) 8)))
      (display "FWS" out)
      (write-u8 version out 'little-endian)
      (write-u32 body-length out 'little-endian)
      (write-bytes body out)
      )))

(set! *load-path* (cons "." *load-path*))
(load "check.scm") ;; srfi-78 Lightweight testing

;;;; Test cases

;; proc is (proc obj port)
(define to-bytes
  (lambda (proc obj) (call-with-output-bytes (lambda (port) (proc obj port)))))

(check (push-bits 1 1 ()) => '(1))
(check (push-bits 7 3 '(0 0 0)) => '(1 1 1 0 0 0))

(check (to-bytes write-fixed8dot8 1.0) => (bytes #x00 #x01))
(check (to-bytes write-fixed8dot8 1.5) => (bytes #x80 #x01))
(check (bits->u8 '(0 0 0 0  0 0 0 1)) => #x01)
(check (bits->u8 '(1 0 0 0  0 0 0 0)) => #x80)
(check (to-bytes *write-bits '(0 0 0 1 0 0 0 0)) => (bytes #x10))
(check (to-bytes *write-bits '(0 0 0 0 1 1 1 1  0 0 0 1 0 0 0 0)) => (bytes #x0f #x10))
(check (to-bytes write-bits '(0 0 0 0 1 1 1 1  0 0 0 1)) => (bytes #x0f #x10))
(check (to-bytes write-rgb '(#x12 #x34 #x56)) => (bytes #x12 #x34 #x56))

(check (call-with-output-bytes
        (lambda (out) (record-header-body 0 (bytes) out))) => (bytes 0 0))
(check (call-with-output-bytes
        (lambda (out) (record-header-body 1 (bytes 1 2 3) out))) => (bytes #x43 00 1 2 3))

(check (nbits-unsigned '(7)) => 3)

;;;; Main

(write-swf SPEC (current-output-port))
