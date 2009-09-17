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

(require "abc.ss")
(require "instruction.ss")

(check-set-mode! 'report-failed)

;;; Test utilities

; proc is (proc port)
(define (from-bytes proc bstr)
  (proc (open-input-bytes bstr)))

; proc is (proc obj port)
(define (to-bytes proc obj)
  (call-with-output-bytes (lambda (port) (proc obj port))))

; round trip
; write-proc is (write-proc obj port)
; read-proc is (read-proc port)
(define (roundtrip-check write-proc read-proc obj)
  (check
   (let ((bstr (to-bytes write-proc obj)))
     (from-bytes read-proc bstr)) => obj))

;;; s24 reader writer

(check (from-bytes read-s24 (bytes #x02 #x00 #x00)) => 2)
(check (from-bytes read-s24 (bytes #xfe #xff #xff)) => -2)
(check (from-bytes read-s24 (bytes #xff #xff #x7f)) => 8388607)
(check (from-bytes read-s24 (bytes #x00 #x00 #x80)) => -8388608)

(check (to-bytes write-s24 2) => (bytes #x02 #x00 #x00))
(check (to-bytes write-s24 -2) => (bytes #xfe #xff #xff))
(check (to-bytes write-s24 8388607) => (bytes #xff #xff #x7f))
(check (to-bytes write-s24 -8388608) => (bytes #x00 #x00 #x80))

;;; s32 reader writer

(check (from-bytes read-s32 (bytes #x02)) => 2)
(check (from-bytes read-s32 (bytes #xfe #xff #xff #xff #x0f)) => -2)
(check (from-bytes read-s32 (bytes #x80 #x80 #x80 #x80 #x08)) => #x-80000000)
(check (from-bytes read-s32 (bytes #xff #xff #xff #xff #x07)) => #x7fffffff)

(check (to-bytes write-s32 2) => (bytes #x02))
(check (to-bytes write-s32 -2) => (bytes #xfe #xff #xff #xff #x0F))
(check (to-bytes write-s32 #x-80000000) => (bytes #x80 #x80 #x80 #x80 #x08))
(check (to-bytes write-s32 #x7fffffff) => (bytes #xff #xff #xff #xff #x07))


(check (from-bytes read-u32 (bytes 2)) => 2)
(check (from-bytes read-u32 (bytes #xff #x07)) => #x3ff)
(check (from-bytes read-u32 (bytes #xff #xff #x07)) => #x1ffff)
(check (from-bytes read-u32 (bytes #x80 #x80 #x80 #x80 #xf8)) => #x80000000)

;;; u32 writer

(check (to-bytes write-u32 #x7f) => (bytes #x7f))
(check (to-bytes write-u32 #x3fff) => (bytes #xff #x7f))
(check (to-bytes write-u32 #x1fffff) => (bytes #xff #xff #x7f))
(check (to-bytes write-u32 #xffffffff) => (bytes #xff #xff #xff #xff #x0f))
;(check (to-bytes write-u32 #x80000000) => (bytes #x80 #x80 #x80 #x80 #xf8))

;;; Round trip

(roundtrip-check (lambda (o p) (write-list1 write-u30 o p))
		 (lambda (p) (read-list1 read-u30 p))
		 '(1 2 3))

(roundtrip-check write-u30 read-u30 1)
(roundtrip-check write-u30 read-u30 2)
(roundtrip-check write-u30 read-u30 3)
(roundtrip-check write-s32 read-s32 3652147)
(roundtrip-check write-u32 read-u32 #x80000000)

(roundtrip-check write-d64 read-d64 0.1)
(roundtrip-check write-d64 read-d64 0.2)
(roundtrip-check write-d64 read-d64 0.3)
(roundtrip-check write-d64 read-d64 9.391)

(roundtrip-check write-string_info read-string_info "")
(roundtrip-check write-string_info read-string_info "hello")
(roundtrip-check write-string_info read-string_info "world")

(roundtrip-check write-namespace_info read-namespace_info '(package (string 1)))
(roundtrip-check write-namespace_info read-namespace_info '(private (string 2)))

(roundtrip-check write-ns_set_info read-ns_set_info '(ns_set (namespace 1) (namespace 2)))

(roundtrip-check write-multiname_info read-multiname_info '((namespace 1) (string 2)))
(roundtrip-check write-multiname_info read-multiname_info '((ns_set 3) (string 4)))

;;; Instruction

(check (read-arguments '(pushstring string) 10 (open-input-bytes (bytes 42)))
       => '(10 pushstring (string 42)))

(check (read-arguments '(callproperty multiname u30) 20 (open-input-bytes (bytes 1 2)))
       => '(20 callproperty (multiname 1) 2))

; Round trip instructions

(check
 (call-with-output-bytes
  (lambda (p)
    (write-arguments '((multiname 2) 1) '(multiname u30) p)))
 => (bytes 2 1))

(check
 (call-with-output-bytes
  (lambda (p)
    (write-instruction '(7 callproperty (multiname 2) 1)
			p))) => (bytes #x46 2 1))

(roundtrip-check (lambda (x p) (write-instructions x p))
		 read-instructions
		 '((0 getlocal 0)
		   (2 pushscope)
		   (3 findpropstrict (multiname 1))
		   (5 pushstring (string 4))
		   (7 callproperty (multiname 2) 1)
		   (10 returnvoid))
		 )

(check
 (call-with-output-bytes
  (lambda (p)
    (write-instruction '(0 getlocal_0)
			p))) => (bytes #xd0))

(roundtrip-check (lambda (x p) (write-instructions x p))
		 read-instructions
		 '((0 getlocal_0)
;		   (1 debugfile (string 1))
;		   (3 pushscope)
;		   (4 getlocal_0)
;		   (5 debugline 9)
;		   (7 finddef (multiname 1))
;		   (9 pushstring (string 2))
		 ))

;;; decode-id

(define test-abc-data
  '((constant_pool(
		   (integer (-10 0 10))
		   (uinteger (10 20 30))
		   (double (0.1 0.2 0.3))
		   (string ("hello" "world" "flash.display"))
		   (namespace ((private (string 1))
			       (package (string 3))))
		   (ns_set ((ns_set (namespace 1))
			    (ns_set (namespace 1) (namespace 2))))
		   (multiname (((namespace 2) (string 1))
			       ((namespace 1) (string 2))))
		   ))))

(check (decode-id '(integer 1) test-abc-data) => -10)
(check (decode-id '(integer 0) test-abc-data) => 0)
(check (decode-id '(uinteger 3) test-abc-data) => 30)
(check (decode-id '(uinteger 0) test-abc-data) => 0)
(check (decode-id '(double 3) test-abc-data) => 0.3)
(check (decode-id '(double 0) test-abc-data) => 0.0)
(check (decode-id '(string 2) test-abc-data) => "world")
(check (decode-id '(string 0) test-abc-data) => '*)
(check (decode-id '(namespace 0) test-abc-data) => '*)
(check (decode-id '(namespace 2) test-abc-data) => '(package "flash.display"))
(check (decode-id '(ns_set 2) test-abc-data) => '(ns_set 2)) ; no convertion
(check (decode-id '(multiname 1) test-abc-data) => '((package "flash.display") "hello"))
(check (decode-id '(multiname 0) test-abc-data) => '*)

;;; Round trip of cpool_info
(roundtrip-check write-cpool_info read-cpool_info
		 (cadr (assoc 'constant_pool test-abc-data)))

;;; Round trip of method_info
(roundtrip-check write-method_info read-method_info
		 '((return_type (multiname 0))
		   (param_type ((multiname 1)))
		   (name (string 1))
		   (flags 0)
		   (options ())
		   (param_names ())))

;;; Round trip of metadata_info
(roundtrip-check write-metadata_info read-metadata_info
		 '((name (string 1)) (items (((key (string 2)) (value (string 3)))
					     ((key (string 4)) (value (string 5)))))))

(roundtrip-check write-traits_info read-traits_info
		 '((kind slot)
		   (name (multiname 3))
		   (slot_id 1)
		   (type_name (multiname 0))
		   (vindex 0)
		   (vkind 0)
		   (metadata ())))

;;; Jump reader

(check (find-label-new 10 '()) => '(10))
(check (find-label-new 10 '(5)) => '(10 5))

(check (*find-label '(23 jump (offset 9)) '(5)) => '(9 5))

(check (find-label '((_ jump (offset 9))
		     (_ ifgt (offset 13))
		     (_ jump (offset 9)))) => '(13 9))

(check (insert-label '((0 jump (offset 8))
		       (4 ifgt (offset 4))
		       (8 jump (offset 8)))
		     '(4 8))
       => '((0 jump L1)
	    L2
	    (4 ifgt L2)
	    L1
	    (8 jump L1)))

;;; Jump writer

(check (instruction-length '(_ pop)) => 1)
(check (instruction-length '(_ pushint (integer 1))) => 2)
(check (instruction-length '(_ callproperty (multiname 1) 1)) => 3)

(check (instruction-arg-length 'integer '(integer 1) 0) => 1)
(check (instruction-arg-length 'integer '(integer #x7f) 0) => 1)
(check (instruction-arg-length 'integer '(integer #x80) 0) => 2)
(check (instruction-arg-length 'integer '(integer #x3fff) 0) => 2)
(check (instruction-arg-length 'integer '(integer #x4000) 0) => 3)
(check (instruction-arg-length 'integer '(integer #x1fffff) 0) => 3)
(check (instruction-arg-length 'integer '(integer #x200000) 0) => 4)
(check (instruction-arg-length 'integer '(integer #xfffffff) 0) => 4)
(check (instruction-arg-length 'integer '(integer #x10000000) 0) => 5)

;; Set position where label appeared.
(check 
 (let ((dst-labels (make-hasheq)))
   (*write-label-extract 'L1 0 dst-labels)
   (hash-ref dst-labels 'L1)) => 0)

;; Set position and placeholder.
(let ((dst-labels 
       (write-label-extract
	'(L1
	  (_ jump L1)
	  (_ jump L2)
	  (_ jump L2)
	  L2
	  ))))
  (check (hash-ref dst-labels 'L1) => 0)
  (check (hash-ref dst-labels 'L2) => 12))

;; Set position and placeholder.
(check (write-label-replace
	'(L1
	  (_ jump L1)
	  (_ jump L2)
	  (_ jump L2)
	  L2
	  ))
       => '(
	  (_ jump (offset 0))
	  (_ jump (offset 12))
	  (_ jump (offset 12)
	  )))
  
;;; ABC form

(define hello-world-abc
  '(abc (minor_version 16) (major_version 46)
    (constant_pool
     ((integer (1 2 3))
      (uinteger (10 20 30))
      (double (0.1 0.2 0.3))
      (string ("hello" "" "print" "Hello, World!!"))
      (namespace ((package (string 2)) (private (string 2))))
      (ns_set ((ns_set (namespace 1) (namespace 2))))
      (multiname (((ns_set 1) (string 3)) ((namespace 1) (string 3))))))
    (method
     (((return_type (multiname 0)) (param_type ()) (name (string 1)) (flags 0) (options ()) (param_names ()))))
    (metadata ())
    (instance ())
    (class ())
    (script (((init (method 0)) (trait ()))))
    (method_body
     (((method 0)
       (max_stack 2) (local_count 1) (init_scope_depth 0) (max_scope_depth 1)
       (code
	((0 getlocal 0)
	 (2 pushscope)
	 (3 findpropstrict (multiname 1))
	 (5 pushstring (string 4))
	 (7 callproperty (multiname 2) 1)
	 (10 returnvoid)))
       (exception ())
       (trait ()))))))

;; (call-with-output-file "test.abc"
;;   (lambda (port)
;;     (write-abc hello-world-abc port))
;;   #:exists 'replace)

(roundtrip-check write-abc read-abc hello-world-abc)

;;; ASM form

(define hello-world-asm
  '(asm
    (ns_set ((ns_set (package "") (private ""))))
    (method
     (((signature
	((return_type *) (param_type ()) (name "hello") (flags 0) (options ()) (param_names ())))
       (hint ((max_stack 2) (local_count 1) (init_scope_depth 0) (max_scope_depth 1)))
       (code
	((0 getlocal 0)
	 (2 pushscope)
	 (3 findpropstrict ((ns_set 1) "print"))
	 (5 pushstring "Hello, World!!")
	 (7 callproperty ((package "") "print") 1)
	 (10 returnvoid)))
       (trait ())
       (exception ()))))
    (instance ())
    (class ())
    (script (((init (method 0)) (trait ()))))))

(define hello-world-asm-short
  '(asm
    (ns_set ((ns_set (package "") (private ""))))
    (method
     (((signature
	((return_type *) (param_type ()) (name "hello") (flags 0) (options ()) (param_names ())))
       (code
	((0 getlocal 0)
	 (2 pushscope)
	 (3 findpropstrict ((ns_set 1) "print"))
	 (5 pushstring "Hello, World!!")
	 (7 callproperty ((package "") "print") 1)
	 (10 returnvoid)))
       (trait ())
       (exception ()))))
    (instance ())
    (class ())
    (script (((init (method 0)) (trait ()))))))

;;; Encode-id test

;;; string

(check (encode-id "hello" NEW-CONSTANT-DICT (lambda (x dict) x))  => '(string 1))

(encode-id "hello" NEW-CONSTANT-DICT
  (lambda (x dict) (encode-id "hello" dict
    (lambda (x dict2) (encode-id "world" dict2
      (lambda (x dict3)
	(check x => '(string 2))
	(check dict3 => '((string "world" "hello")
			  (integer) (uinteger) (double) (namespace) (multiname))
	       )))))))

;;; list
(check (encode-id-map encode-id '("hello" "world") NEW-CONSTANT-DICT (lambda (x dict) x))
       => '((string 1) (string 2)))

;;; namespace

(check (encode-id '(package "ok") NEW-CONSTANT-DICT (lambda (x dict) x))  => '(namespace 1))


(encode-id '(package "ok") NEW-CONSTANT-DICT
  (lambda (x dict) 
    (check x => '(namespace 1))
    (check dict => '((namespace (package (string 1)))
		     (string "ok")
		     (integer) (uinteger) (double) (multiname)))))

(encode-id '((package "") "print") NEW-CONSTANT-DICT
  (lambda (x dict) 
    (check x => '(multiname 1))
    (check dict => '((multiname ((namespace 1) (string 2)))
		     (string "print" "")
		     (namespace (package (string 1)))
		     (integer) (uinteger) (double)))))

(encode-id '((ns_set 1) "print") NEW-CONSTANT-DICT
  (lambda (x dict) 
    (check x => '(multiname 1))
    (check dict => '((multiname ((ns_set 1) (string 1)))
		     (string "print")
		     (integer) (uinteger) (double) (namespace)))))

;;; signature

(check 
 (encode-id '((return_type *) (param_type ()) (name "hello") (flags 0) (options ()) (param_names ())) NEW-CONSTANT-DICT
	     (lambda (x dict) x))
 => '((return_type (multiname 0)) (param_type ()) (name (string 1)) (flags 0) (options ()) (param_names ())))
  
;;; code without numbers

(encode-id
 '(code ((0 getlocal 0)
	 (2 pushscope)
	 (3 findpropstrict ((ns_set 1) "print"))
	 (5 pushstring "Hello, World!!")
	 (7 callproperty ((package "") "print") 1)
	 (10 returnvoid)))
 NEW-CONSTANT-DICT
 (lambda (x dict)
   (check x => '(code ((0 getlocal 0)
		       (2 pushscope)
		       (3 findpropstrict (multiname 1))
		       (5 pushstring (string 2))
		       (7 callproperty (multiname 2) 1)
		       (10 returnvoid))))
   (check dict => '((multiname ((namespace 1) (string 1)) ((ns_set 1) (string 1)))
		    (namespace (package (string 3)))
		    (string "" "Hello, World!!" "print")
		    (integer)
		    (uinteger)
		    (double)))))

;;; no-conversion

(check 
 (encode-id '((0 getlocal 0) (2 pushscope)) NEW-CONSTANT-DICT
	     (lambda (x dict) x))
 => '((0 getlocal 0) (2 pushscope)))

;;; ns_set

(encode-id '(ns_set (package "ok") (private "ok")) NEW-CONSTANT-DICT
	    (lambda (x dict)
	      (check x => '(ns_set (namespace 1) (namespace 2)))
	      (check dict => '((namespace (private (string 1)) (package (string 1)))
			       (string "ok") (integer) (uinteger) (double) (multiname)))))

;;; code with constant numbers

(encode-id '(code (
		   (5 pushstring "byte 9 + short 938=")
		   (7 pushbyte 9)
		   (9 pushshort 938)
		   (21 pushint 3652147)
		   (23 pushuint 2147483648)
		   (34 pushdouble 9.391)
		   )) NEW-CONSTANT-DICT
		      (lambda (x dict) (check x => '(code (
							   (5 pushstring (string 1))
							   (7 pushbyte 9)
							   (9 pushshort 938)
							   (21 pushint (integer 1))
							   (23 pushuint (uinteger 1))
							   (34 pushdouble (double 1))
							   )))))

;;; From ASM

(check (from-asm-constant NEW-CONSTANT-DICT '())
       => '((integer ()) (uinteger ()) (double ()) (string ()) (namespace ()) (ns_set ()) (multiname ())))

(check 
 (let* ((ns_set '(((package "") (private ""))))
	(method (ref 'method (cdr hello-world-asm)))
	(dict (encode-id method NEW-CONSTANT-DICT (lambda (x dict) dict)))
	(constant (from-asm-constant dict ns_set)))
   constant)
 => '((integer ())
      (uinteger ())
      (double ())
      (string ("hello" "print" "Hello, World!!" ""))
      (namespace ((package (string 4))))
      (ns_set (((package "") (private ""))))
      (multiname (((ns_set 1) (string 2)) ((namespace 1) (string 2))))))

(roundtrip-check write-asm read-asm hello-world-asm)

;;; Extract hint from code

(check (hash-ref hint-instruction-map 'pushscope) => '((stack -1) (scope 1)))

(check (*from-asm-make-hint-eval '(1 2) 1) => 1)
(check (*from-asm-make-hint-eval '(1 2) '(arg 1)) => 2)
(check (*from-asm-make-hint-eval '(1 2) '(+ 10 (arg 1))) => 12)

(check (from-asm-make-hint-eval 'stack '(2 pushscope) '((stack -1) (scope 1)))
       => -1)

(check (from-asm-make-hint-eval 'local '(0 getlocal 4) '((stack 1) (local (arg 0))))
       => 4)

(check (from-asm-make-hint
	'((code
	   ((_ getlocal 0)
	    (_ setlocal 1)
	    (_ getlocal 2)
	    (_ setlocal 3)
	    (_ getlocal 0)
	    (_ pushscope)
	    (_ findpropstrict ((ns_set 1) "print"))
	    (_ pushstring "Hello, World!!")
	    (_ callproperty ((package "") "print") 1)
	    (_ returnvoid)))))
       => '((max_stack 2) (local_count 4) (init_scope_depth 0) (max_scope_depth 1)))

;; hello wolrd without hint
(check
 (let ((bstr (to-bytes write-asm hello-world-asm-short)))
   (from-bytes read-asm bstr)) => hello-world-asm)


;(pretty-print (from-asm hello-world-asm))
;(pretty-print (encode-id hello-world-asm NEW-CONSTANT-DICT))



