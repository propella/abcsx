;;; ABC file writer and reader
;;;
;;; This read ABC file and construct two kinds of form.
;;;
;;; ABC form - It keeps original structure of ABC
;;; ASM form - For human, easy to read
;;;
;;; Typical usage:
;;; (call-with-input-file "hello.abc" (lambda (port) (pretty-print (to-asm (read-abc port)))))
;;;

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

(require "instruction.ss")

(provide (all-defined-out))

;;;;;;;;;; The main public reader and writer ;;;;;;;;;;

;; De-serialize binary stream to ASM form
(define (read-asm port)
  (to-asm (read-abc port)))

;; Serialize ASM or ABC form to binary stream
;; If (car expr) is 'abc, the form is abc.
(define (write-asm form port)
  (let ((abc (cond
	      ((eq? (car form) 'asm) (from-asm form))
	      ((eq? (car form) 'abc) form)
	      (else (error (format "This is not ABC nor ASM form: ~a" form))))))
    (write-abc abc port)))

;;;;;;;;;; ABC Form Reader ;;;;;;;;;;

;;; abcFile reader

(define (read-abc port)
  (let* ((minor_version (read-u16 port))
	 (major_version (read-u16 port))
	 (constant_pool (read-cpool_info port))
	 (method (read-list0 read-method_info port))
	 (metadata (read-list0 read-metadata_info port))
	 (class_count (read-u30 port))
	 (instance (build-list class_count (lambda (_) (read-instance_info port))))
	 (class (build-list class_count (lambda (_) (read-class_info port))))
	 (script (read-list0 read-script_info port))
	 (method_body (read-list0 read-method_body_info port))
	 )
    `(abc
      (minor_version ,minor_version)
      (major_version ,major_version)
      (constant_pool ,constant_pool)
      (method ,method)
      (metadata ,metadata)
      (instance ,instance)
      (class ,class)
      (script ,script)
      (method_body ,method_body))))

;;; cpool reader

(define (read-cpool_info port)
  (let* ((integers (read-list1 read-s32 port))
	 (uintegers (read-list1 read-u32 port))
	 (doubles (read-list1 read-d64 port))
	 (strings (read-list1 read-string_info port))
	 (namespaces (read-list1 read-namespace_info port))
	 (ns_sets (read-list1 read-ns_set_info port))
	 (multinames (read-list1 read-multiname_info port)))
    `((integer ,integers)
      (uinteger ,uintegers)
      (double ,doubles)
      (string ,strings)
      (namespace ,namespaces)
      (ns_set ,ns_sets)
      (multiname ,multinames))))

;;; multiname_info reader

(define CONSTANT_QName #x07)
(define CONSTANT_QNameA #x0D)
(define CONSTANT_RTQName #x0F)
(define CONSTANT_RTQNameA #x10)
(define CONSTANT_RTQNameL #x11)
(define CONSTANT_RTQNameLA #x12)
(define CONSTANT_Multiname #x09)
(define CONSTANT_MultinameA #x0E)
(define CONSTANT_MultinameL #x1B)
(define CONSTANT_MultinameLA #x1C)

(define (read-multiname_info port)
  (let ((kind (read-u8 port)))
    (cond
     ((= kind CONSTANT_QName) (read-multiname_kind_QName port))
     ((= kind CONSTANT_QNameA) (multiname-attribute (read-multiname_kind_QName port)))
     ((= kind CONSTANT_RTQName) (read-multiname_kind_RTQName port))
     ((= kind CONSTANT_RTQNameA) (multiname-attribute (read-multiname_kind_RTQName port)))
     ((= kind CONSTANT_RTQNameL) (read-multiname_kind_RTQNameL port))
     ((= kind CONSTANT_RTQNameLA) (multiname-attribute (read-multiname_kind_RTQNameL port)))
     ((= kind CONSTANT_Multiname) (read-multiname_kind_Multiname port))
     ((= kind CONSTANT_MultinameA) (multiname-attribute (read-multiname_kind_Multiname port)))
     ((= kind CONSTANT_MultinameL) (read-multiname_kind_MultinameL port))
     ((= kind CONSTANT_MultinameLA) (multiname-attribute (read-multiname_kind_MultinameL port)))
     )))

(define (multiname-attribute x)
  `(attribute ,x))

(define (read-multiname_kind_QName port)
  `(,(read-id 'namespace port)
    ,(read-id 'string port)))

(define (read-multiname_kind_RTQName port)
  (read-id 'string port))

(define (read-multiname_kind_RTQNameL port)
  'RTQNameL)

(define (read-multiname_kind_Multiname port)
  (let ((name (read-id 'string port))
	(ns_set (read-id 'ns_set port)))
    `(,ns_set ,name)))

(define (read-multiname_kind_MultinameL port)
  `(ns_set ,(read-u30 port)))

;; CONSTANT_Namespace 0x08 
;; CONSTANT_PackageNamespace 0x16 
;; CONSTANT_PackageInternalNs 0x17 
;; CONSTANT_ProtectedNamespace 0x18 
;; CONSTANT_ExplicitNamespace 0x19 
;; CONSTANT_StaticProtectedNs 0x1A 
;; CONSTANT_PrivateNs 0x05 

(define (transpose dict)
  (map (lambda (pair) (cons (cdr pair) (car pair))) dict))

(define CONSTANT_Namespace
  '((#x08 . ns)
   (#x16 . package)
   (#x17 . internal)
   (#x18 . protected)
   (#x19 . explicit)
   (#x1a . static)
   (#x05 . private)))

(define CONSTANT_Namespace_write (transpose CONSTANT_Namespace))

;;; namespace_info reader

(define (read-namespace_info port)
  (let ((kind (read-u8 port))
	(name (read-id 'string port)))
    `(,(cdr (assoc kind CONSTANT_Namespace)) ,name)))

;;; ns_set_info reader

(define (read-ns_set_info port)
  (cons 'ns_set
	(read-list0 (lambda (p) (read-id 'namespace p)) port)))

;;; method_info reader

(define (read-method_info port)
  (let* ((param_count (read-u30 port))
	 (return_type (read-id 'multiname port))
	 (param_type (build-list param_count (lambda (_) (read-id 'multiname port))))
	 (name (read-id 'string port))
	 (flags (read-u8 port))
	 (options (if (method-info-HAS_OPTIONAL flags) (read-option_info port) '()))
	 (param_names (if (method-info-HAS_PARAM_NAMES flags) (read-param_info param_count port) '()))
	 )
    `((return_type ,return_type)
      (param_type ,param_type)
      (name ,name)
      (flags ,flags)
      (options ,options)
      (param_names ,param_names))))

(define (read-option_info port)
  (read-list0 read-option_detail port))

(define (read-option_detail port) ; todo
  `((val ,(read-u30 port))
    (kind ,(read-u8 port))))

(define (read-param_info param_count port)
  (build-list param_count (lambda (_) (read-u30 port))))
  
;;; metadata_info reader

(define (read-metadata_info port)
  `((name (string ,(read-u30 port)))
    (items ,(read-list0 read-item_info port))))

(define (read-item_info port)
  `((key ,(read-id 'string port))
    (value ,(read-id 'string port))))

;;; instance_info reader

(define Trait_Slot 0)
(define Trait_Method 1)
(define Trait_Getter 2)
(define Trait_Setter 3)
(define Trait_Class 4)
(define Trait_Function 5)
(define Trait_Const 6)

(define CONSTANT_ClassSealed #x01)
(define CONSTANT_ClassFinal #x02)
(define CONSTANT_ClassInterface #x04)
(define CONSTANT_ClassProtectedNs #x08)

(define (read-instance_info port)
;  (printf "pos ~a\n" (file-position port))
  (let* ((name (read-id 'multiname port))
	 (super_name (read-id 'multiname port))
	 (flags (read-u8 port))
	 (protectedNs (if (positive? (bitwise-and flags CONSTANT_ClassProtectedNs))
			  (read-u30 port)
			  '()))
	 (interface (read-list0 read-u30 port))
	 (iinit (read-u30 port))
	 (trait (read-list0 read-traits_info port)))
    `((name ,name)
      (super_name ,super_name)
      (flags ,flags)
      (protectedNs (namespace ,protectedNs))
      (interface ,(map (lambda (i) `(multiname ,i)) interface))
      (iinit (method ,iinit))
      (trait ,trait)
      )))

;;; traits_info reader

(define (read-traits_info port)
  (let* ((name (read-id 'multiname port))
	 (kind (read-u8 port))
	 (trait-type (bitwise-and kind #b1111))
	 (is-ATTR_Final    (positive? (bitwise-and kind #b00010000)))
	 (is-ATTR_Override (positive? (bitwise-and kind #b00100000)))
	 (is-ATTR_Metadata (positive? (bitwise-and kind #b01000000)))
	 (data (read-traits_info_data name trait-type port))
	 (typename (car data))
	 (others (cdr data))
	 (metadata (if is-ATTR_Metadata
		       (read-list0 read-u30 port)
		       '())))
;    (if (zero? name) (error "traits_info.name cannot be zero") '())
    `((kind ,typename)
      (name ,name)
      ,@others
      (metadata ,metadata))))
  
(define (read-traits_info_data name kind port)
  (cond
   ((= kind Trait_Slot) `(slot . ,(read-trait_slot port)))
   ((= kind Trait_Method) `(method . ,(read-trait_method port)))
   ((= kind Trait_Getter) `(getter . ,(read-trait_method port)))
   ((= kind Trait_Setter) `(setter . ,(read-trait_method port)))
   ((= kind Trait_Class) `(class . ,(read-trait_class port)))
   ((= kind Trait_Function) `(function . ,(read-trait_function port)))
   ((= kind Trait_Const) `(const ,(read-trait_slot port)))
   (else (error (format "Unknown trait type ~a" kind)))
))

(define (read-trait_slot port)
  (let* ((slot_id (read-u30 port))
	 (type_name (read-id 'multiname port))
	 (vindex (read-u30 port))
	 (vkind (if (= vindex 0) 0 (read-u8 port))))
  `((slot_id ,slot_id)
    (type_name ,type_name)
    (vindex ,vindex)
    (vkind ,vkind))))

(define (read-trait_class port)
  `((slot_id ,(read-u30 port))
    (classi (class ,(read-u30 port)))))

(define (read-trait_function port)
  `((slot_id ,(read-u30 port))
    (function ,(read-u30 port))))

(define (read-trait_method port)
  `((disp_id ,(read-u30 port))
    (method ,(read-u30 port))))

;;; class_info reader

(define (read-class_info port)
  (let ((cinit (read-u30 port))
	(traits (read-list0 read-traits_info port)))
    `((cinit (method ,cinit))
      (traits ,traits))))

;;; script_info reader

(define (read-script_info port)
  (let ((init (read-id 'method port))
	(trait (read-list0 read-traits_info port)))
    `((init ,init)
      (trait ,trait))))

;;; method_body_info reader

(define (read-method_body_info port)
  (let* ((method (read-u30 port))
	(max_stack (read-u30 port))
	(local_count (read-u30 port))
	(init_scope_depth (read-u30 port))
	(max_scope_depth (read-u30 port))
	(code_length (read-u30 port))
	(code (read-bytes code_length port))
	(exception (read-list0 read-exception_info port))
	(trait (read-list0 read-traits_info port)))
    `((method ,method)
      (max_stack ,max_stack)
      (local_count ,local_count)
      (init_scope_depth ,init_scope_depth)
      (max_scope_depth ,max_scope_depth)
      (code ,(decode-instructions code))
      (exception ,exception)
      (trait ,trait))))

(define (read-exception_info port)
  `((from ,(read-u30 port))
    (to ,(read-u30 port))
    (target ,(read-u30 port))
    (exc_type (string ,(read-u30 port)))
    (var_name (string ,(read-u30 port)))))

;;; code reader

(define (decode-instructions code)
  (let ((code-port (open-input-bytes code)))
    (read-instructions code-port)))

(define (read-instructions port)
  (let* ((code (reverse (read-instruction '() port)))
	 (labels (find-label code)))
    (insert-label code labels)))

;; Extract and insert jump labels
(define (insert-label code labels)
  (if (null? code)
      '()
      (let* ((line (car code))
	     (pos (car line))
	     (inst (cadr line))
	     (args (cddr line))
	     (destination (destination-from-args args))
	     (*line (if destination
			`(,pos ,inst ,(label-from-list destination labels))
			line))
	     (dst-label (label-from-list pos labels))
	     (more (cons *line (insert-label (cdr code) labels))))
	(if dst-label
	    (cons dst-label more)
	    more)
)))

(define (destination-from-args xs)
  (if (and (not (null? xs))
	   (pair? (car xs))
	   (eq? (car (car xs)) 'offset))
      (cadr (car xs))
      #f))

;; make a label from label list and offset.
(define (label-from-list destination labels)
  (let ((found (memq destination labels)))
    (if found
	(string->symbol (string-append "L" (number->string (length found))))
	#f)))

;; Return all jump addreses in reverse order
;; The result numbers are used for label index. For example,
;; 3 in (3 4 5) is L2 because 3 is the third in reverse order.
(define (find-label code)
  (foldl *find-label '() code))

(define (*find-label line labels)
  (let* ((args (cddr line))
	 (offset (destination-from-args args)))
    (if offset
	(find-label-new offset labels)
	labels)))

(define (find-label-new pos labels)
  (if (memq pos labels)
      labels
      (cons pos labels)))
;;

(define (read-instruction decoded port)
  (let ((pos (file-position port))
	(inst (read-byte port)))
    (if (eof-object? inst)
	decoded
	(let ((spec (hash-ref read-instruction-map inst)))
	  (read-instruction
;	   (cons (dprint (read-arguments spec pos port)) decoded) port)))))
	   (cons (read-arguments spec pos port) decoded) port)))))

(define (read-arguments spec pos port)
  (let ((opname (car spec))
	(types (cdr spec)))
    (let ((args (map (lambda (type)
		       (cond
			((eq? type 'u8) (read-u8 port))
			((eq? type 'offset)
			 `(offset ,(+ 4 pos (read-s24 port))))
			((eq? type 's24) (read-s24 port))
			((eq? type 'u30) (read-u30 port))
			((eq? type 'register) (read-u30 port))
			((eq? type 'integer) (read-id 'integer port))
			((eq? type 'uinteger) (read-id 'uinteger port))
			((eq? type 'double) (read-id 'double port))
			((eq? type 'string) (read-id 'string port))
			((eq? type 'multiname) (read-id 'multiname port))
			((eq? type 'method) (read-id 'method port))
			(else (error (format "unknown param type ~a" type)))))
		     types)))
      `(,pos ,opname ,@args))))

;;; Reader Utilities

(define (read-id type port)
  `(,type ,(read-u30 port)))

; Read entries by func. The count is the number of entries plus one.
(define (read-list1 func port)
  (let ((count (read-u30 port)))
    (if (= count 0)
	'()
	(build-list (- count 1) (lambda (_) (func port))))))

; Read entries by func. The count is the number of entries.
(define (read-list0 func port)
  (let ((count (read-u30 port)))
    (build-list count (lambda (_) (func port)))))

;;; Primitive Data Types Readers

(define (read-u8 port)
  (read-byte port))

; Two-byte unsigned integer value.  
(define (read-u16 port)
  (let ((l (read-byte port))
	(h (read-byte port)))
    (+ (* h 256) l)))

(define (read-u30 port) ;; todo
    (read-u32 port))

(define (read-s24 port)
  (let ((b0 (read-byte port))
	(b1 (read-byte port))
	(b2 (read-byte port)))
    (if (positive? (bitwise-and b2 #b10000000))
	(+ (* b2 #x10000) (* b1 #x100) b0 #x-1000000)
	(+ (* b2 #x10000) (* b1 #x100) b0))))

(define (read-s32 port)
    (let ((u32 (read-u32 port)))
      (if (< u32 #x80000000)
	  u32
	  (- u32 #x100000000))))

; 8-byte IEEE-754 floating point value.
(define (read-d64 port)
  (let ((bx (read-bytes 8 port)))
    (floating-point-bytes->real bx #f)))

(define (read-string_info port)
  (let ((size (read-u30 port)))
    (bytes->string/utf-8 (read-bytes size port))))

(define (read-u32 port)
  (let ((a (read-byte port)))
    (if (< a 128) a
	(let ((a (bitwise-and a #x7f))
	      (b (read-byte port)))
	  (if (< b 128) (+ (arithmetic-shift b 7) a)
	      (let ((b (bitwise-and b #x7f))
		    (c  (read-byte port)))
		(if (< c 128) (+ (arithmetic-shift c 14) (arithmetic-shift b 7) a)
		    (let ((c (bitwise-and c #x7f))
			  (d  (read-byte port)))
		      (if (< d 128) (+ (arithmetic-shift d 21) (arithmetic-shift c 14) (arithmetic-shift b 7) a)
			  (let ((d (bitwise-and d #x7f))
				(e  (bitwise-and (read-byte port) #x0f)))
			    (+ (arithmetic-shift e 28) (arithmetic-shift d 21) (arithmetic-shift c 14) (arithmetic-shift b 7) a)))))))))))

;;;;;;;;;; ASM Form Decoder ;;;;;;;;;;

(define (to-asm abc)
  (let* ((*abc (set-method-body (cdr abc)))
	 (**abc (decode-id *abc *abc))
	 (cpool (assoc 'constant_pool **abc))
	 (ns_set (assoc 'ns_set (cadr cpool)))
	 (method (assoc 'method **abc))
	 (instance (assoc 'instance **abc))
	 (class (assoc 'class **abc))
	 (script (assoc 'script **abc)))
    (list 'asm ns_set method instance class script)))

;;; ASM form

; For some reason, a method_body refers a method instead of the other way.
; So I have to make reverse reference by my hand.
(define (set-method-body abc)
  (let* ((method (assoc 'method abc))
	 (method-bodies (cadr (assoc 'method_body abc)))
	 (methods (make-method-body-reference (cadr method) method-bodies 0)))
    (map (lambda (tag)
	   (if (eq? (car tag) 'method) `(method ,methods)
	       tag))
	 abc)))

(define (make-method-body-reference methods method-bodies index)
  (if (null? methods)
      '()
      (let* ((method-body
	      (findf (lambda (m)
		       (let ((id (car m))) ; The first element should be '(method id)
			 (= (cadr id) index)))
		     method-bodies))
	     (method-body1 (cdr method-body)) ; remove '(method id) in the method-body
	     (max_stack (assoc 'max_stack method-body))
	     (local_count (assoc 'local_count method-body))
	     (init_scope_depth (assoc 'init_scope_depth method-body))
	     (max_scope_depth (assoc 'max_scope_depth method-body))
	     (code (assoc 'code method-body))
	     (exception (assoc 'exception method-body))
	     (trait (assoc 'trait method-body))
	     (method `((signature ,(car methods))
		       (hint (,max_stack ,local_count ,init_scope_depth ,max_scope_depth))
		       ,code
		       ,trait
		       ,exception)))
	(cons method
	      (make-method-body-reference (cdr methods) method-bodies (+ index 1))))))
  
;;; Convert index expression to the literal
;;; (string 1) => "hello"

(define (decode-id e abc)
  (if (pair? e)
      (if (constant-id? e)
	  (let ((key (car e))
		(id (cadr e)))
	    (cond
	     ((eq? key 'integer) (find-constant 'integer id 0 abc))
	     ((eq? key 'uinteger) (find-constant 'uinteger id 0 abc))
	     ((eq? key 'double) (find-constant 'double id 0.0 abc))
	     ((eq? key 'string) (find-constant 'string id '* abc))
	     ((eq? key 'namespace) (find-constant 'namespace id '* abc)) ; ???
	     ((eq? key 'multiname) (find-constant 'multiname id '* abc)) ; ???
;	     ((eq? key 'method) (find-element 'method id abc))
	     (else (cons (decode-id (car e) abc) (decode-id (cdr e) abc)))))
	  (cons (decode-id (car e) abc) (decode-id (cdr e) abc)))
      e))

(define (constant-id? e)
  (and (pair? e)
       (symbol? (car e))
       (pair? (cdr e))
       (number? (cadr e))))

; find one-base index from the constant pool
(define (find-constant type id default abc)
  (let* ((pool (cadr (assoc 'constant_pool abc)))
	 (table (cadr (assoc type pool))))
    (if (zero? id)
	default
	(decode-id (list-ref table (- id 1)) abc))))

; find zero-base index from a body
(define (find-element type id abc)
  (let* ((pool abc)
	 (table (cadr (assoc type pool))))
    (decode-id (list-ref table id) abc)))

;;;;;;;;;; ABC Form Writer ;;;;;;;;;;

;;; abcFile writer

(define (write-abc abc port)
  (if (not (eq? (car abc) 'abc)) (error (format "This is not a ABC form: ~a" abc)) '())
  (let* ((*abc (cdr abc))
	 (instance (ref 'instance *abc))
	 (class_count (length instance)))
    (write-u16 16 port)
    (write-u16 46 port)
    (write-cpool_info (cadr (assoc 'constant_pool *abc)) port)
    (write-list0 write-method_info (ref 'method *abc) port)
    (write-list0 write-metadata_info (ref 'metadata *abc) port)
    (write-u30 class_count port)
    (for-each (lambda (each) (write-instance_info each port)) instance)
    (for-each (lambda (each) (write-class_info each port)) (ref 'class *abc))
    (write-list0 write-script_info (ref 'script *abc) port)
    (write-list0 write-method_body_info (ref 'method_body *abc) port)
))

;;; cpool_info writer

(define (write-cpool_info x port)
  (write-list1 write-s32 (cadr (assoc 'integer x)) port)
  (write-list1 write-u32 (cadr (assoc 'uinteger x)) port)
  (write-list1 write-d64 (cadr (assoc 'double x)) port)
  (write-list1 write-string_info (cadr (assoc 'string x)) port)
  (write-list1 write-namespace_info (cadr (assoc 'namespace x)) port)
  (write-list1 write-ns_set_info (cadr (assoc 'ns_set x)) port)
  (write-list1 write-multiname_info (cadr (assoc 'multiname x)) port)
  '())

(define (write-multiname_info x port)
  (cond
   ((eq? (car x) 'attribute) (error "attribute is not supported yet"))
   ((eq? (car x) 'RTQNameL) (error "RTQNameL is not supported yet"))
   ((eq? (car x) 'string) (error "RTQName is not supported yet"))
   ((eq? (car x) 'ns_set) (error "MultinameL is not supported yet"))
   ((eq? (car (car x)) 'namespace) (write-multiname_kind_QName x port))
   ((eq? (car (car x)) 'ns_set) (write-multiname_kind_Multiname x port))
   ))

(define (write-multiname_kind_QName x port)
  (let ((kind #x07)
	(ns (cadr (car x)))
	(name (cadr (cadr x))))
    (write-u8 kind port)
    (write-u30 ns port)
    (write-u30 name port)))

(define (write-multiname_kind_Multiname x port)
  (let ((kind #x09)
	(ns_set (cadr (car x)))
	(name (cadr (cadr x))))
    (write-u8 kind port)
    (write-u30 name port)
    (write-u30 ns_set port)))

(define (write-ns_set_info x port)
  (let ((xs (cdr x)))
    (write-list0 (lambda (x p) (write-u30 (cadr x) p)) xs port)))

(define (write-namespace_info x port)
  (let ((kind (car x))
	(name (cadr (cadr x))))
    (write-u8 (cdr (assoc kind CONSTANT_Namespace_write)) port)
    (write-u30 name port)))

(define (write-string_info x port)
  (let ((bytes (string->bytes/utf-8 x)))
    (write-u30 (bytes-length bytes) port)
    (write-bytes bytes port)))

;;; method_info writer

(define	(method-info-NEED_ARGUMENTS  flags) (positive? (bitwise-and flags #x01)))
(define	(method-info-NEED_ACTIVATION flags) (positive? (bitwise-and flags #x02)))
(define	(method-info-NEED_REST       flags) (positive? (bitwise-and flags #x04)))
(define	(method-info-HAS_OPTIONAL    flags) (positive? (bitwise-and flags #x08)))
(define	(method-info-SET_DXNS        flags) (positive? (bitwise-and flags #x40)))
(define	(method-info-HAS_PARAM_NAMES flags) (positive? (bitwise-and flags #x80)))

;; Find a key in the dictionary and return only the value.
;; dictionary is like ((key0 value0) (key1 value1) ...)
;; The optional third argument is returned if the key is not found.

(define (ref key dictionary . default)
  (let ((found (assoc key dictionary)))
    (cond (found (cadr found))
	  ((not (null? default)) (car default))
	  (else error (format "'~a' does not found in ~a." key dictionary)))))

(define (write-method_info x port)
  (let* ((param_type (ref 'param_type x))
	 (param_count (length param_type)))
    (write-u30 param_count port)
    (write-u30 (cadr (ref 'return_type x)) port)
    (for-each (lambda (type) (write-id 'multiname type port)) param_type) ;todo
    (write-u30 (cadr (ref 'name x)) port)
    (write-u8 (ref 'flags x) port)
    (if (method-info-HAS_OPTIONAL (ref 'flags x)) (error "HAS_OPTIONAL is not supported yet") '()) ;todo
    (if (method-info-HAS_PARAM_NAMES (ref 'flags x)) (error "HAS_OPTIONAL is not supported yet") '()) ;todo
  ))

;;; traits_info writer
(define (write-traits_info x port)
  (let ((kind (cadr (assoc 'kind x))))
    (cond
     ((eq? kind 'slot) (write-trait_slot x port))
     ((eq? kind 'method) (error "method trait is not supported yet"))
     ((eq? kind 'getter) (error "getter trait is not supported yet"))
     ((eq? kind 'setter) (error "setter trait is not supported yet"))
     ((eq? kind 'class) (error "class trait is not supported yet"))
     ((eq? kind 'function) (error "function trait is not supported yet"))
     ((eq? kind 'const) (error "const trait is not supported yet"))
     (else (error (format "Unknown trait type ~a" kind))))))

(define (write-trait_slot x port)
  (let ((slot_id (ref 'slot_id x))
	(type_name (ref 'type_name x))
	(vindex (if (= (ref 'vindex x) 0)
		    0
		    (error "vindex is not supported yet")))
	(metadata_count (if (null? (ref 'metadata x))
			    0
			    (error "metadata is not supported et"))))
    (write-id 'multiname (ref 'name x) port)
    (write-u8 Trait_Slot port)
    (write-u30 slot_id port)
    (write-id 'multiname type_name port)
    (write-u30 0 port) ; vindex
    ))

;;; instance_info writer
(define (write-instance_info x port)
  (error "instance_info is not supported yet"))
(define (write-class_info x port)
  (error "instance_info is not supported yet"))

;;; metadata_info writer
(define (write-metadata_info x port)
  (let ((name (cadr (ref 'name x)))
	(items (ref 'items x)))
    (write-u30 name port)
    (write-list0 write-item_info items port)))

(define (write-item_info x port)
  (let ((key (cadr (ref 'key x)))
	(value (cadr (ref 'value x))))
    (write-u30 key port)
    (write-u30 value port)))
  
;;; script_info writer
(define (write-script_info x port)
  (write-u30 (cadr (ref 'init x)) port)
  (write-list0 write-traits_info (ref 'trait x) port))

;;; method_body_info writer
(define (write-method_body_info x port)
  (let* ((code (encode-instructions (ref 'code x)))
	 (code_length (bytes-length code)))
    (write-u30 (ref 'method x) port)
    (write-u30 (ref 'max_stack x) port)
    (write-u30 (ref 'local_count x) port)
    (write-u30 (ref 'init_scope_depth x) port)
    (write-u30 (ref 'max_scope_depth x) port)
    (write-u30 code_length port)
    (write-bytes code port)
    (write-list0 write-exception_info (ref 'exception x) port)
    (write-list0 write-traits_info (ref 'trait x) port)
    ))

;;; code writer

(define (encode-instructions code)
;(dprint code)
  (let ((*code (write-label-replace code)))
    (call-with-output-bytes
     (lambda (port)
       (write-instructions *code port)))))

(define (write-label-replace lines)
  (let ((dst-labels (write-label-extract lines)))
    (*write-label-replace lines dst-labels)))

(define (*write-label-replace lines dst-labels)
  (if (null? lines)
      '()
      (let ((line (car lines)))
	(if (symbol? line)
	    (*write-label-replace (cdr lines) dst-labels) ; the line is label
	    (let* ((pos (car line))
		   (inst (cadr line))
		   (args (cddr line))
		   (*line (if (and (= (length args) 1)
				   (symbol? (car args)))
			      (let ((offset (hash-ref dst-labels (car args))))
				`(,pos ,inst (offset ,offset)))
			      line)))
	      (cons *line (*write-label-replace (cdr lines) dst-labels)))))))

;; Return a dictionary where label -> position
(define (write-label-extract lines)
  (let ((dst-labels (make-hasheq)))
    (foldl (lambda (line pos)
	     (*write-label-extract line pos dst-labels))
	   0
	   lines)
    dst-labels))
  
;; Add into dst-labels if the line is a label (symbol),
;; Return next position.
(define (*write-label-extract line pos dst-labels)
  (if (symbol? line)
      (begin (hash-set! dst-labels line pos)
	     pos)
      (+ (instruction-length line) pos)))

(define (instruction-length line)
  (let* ((inst (cadr line))
	 (spec (hash-ref write-instruction-map inst))
	 (arg-types (cddr spec))
	 (arg-values (cddr line))
	 (arg-length (foldl instruction-arg-length 0 arg-types arg-values)))
    (add1 arg-length)))

(define (instruction-arg-length type value sum)
;(dprint (list type value sum))
  (+ sum
     (cond
      ((eq? type 'u8) 1)
      ((eq? type 'offset) 3)
      ((memq type '(u30 register)) (instruction-u30-length value))
      ((memq type '(double integer method multiname string uinteger))
       (instruction-u30-length (cadr value)))
      (else (error (format "Unknown type (instruction-arg-length): ~a ~a" type value))))))

(define (instruction-u30-length value)
  (cond
   ((= (arithmetic-shift value -7) 0) 1)
   ((= (arithmetic-shift value -14) 0) 2)
   ((= (arithmetic-shift value -21) 0) 3)
   ((= (arithmetic-shift value -28) 0) 4)
   (else 5)))
    
(define (write-arguments1 args spec src-labels port)
  (for-each (lambda (type arg)
	      (cond
	       ((eq? type 'u8) (write-u8 arg port))
	       ((eq? type 's24) (write-s24 arg port))
	       ((eq? type 'u30) (write-u30 arg port))
	       ((eq? type 'double) (write-id type arg port))
	       ((eq? type 'integer) (write-id type arg port))
	       ((eq? type 'method) (write-id type arg port))
	       ((eq? type 'multiname) (write-id type arg port))
	       ((eq? type 'offset) (write-label arg src-labels port))
	       ((eq? type 'register) (write-u30 arg port))
	       ((eq? type 'string) (write-id type arg port))
	       ((eq? type 'uinteger) (write-id type arg port))
	       (else (error (format "Unknown argument type: ~a" type)))
	       ))
	    spec args))

(define (write-label label src-labels port)
  (let ((position (file-position port)))
    (if (symbol? label)
	(begin (hash-set! src-labels position label)
	       (write-s24 0 port) ; A place holder
	       )
	(let* ((offset (cadr label))
	       (diff (- offset position 3)))
	  (write-s24 diff port)) ; in case of (offset -10) form.
	)))

;;

(define (write-instructions code port)
  (for-each (lambda (statement) 
	      (write-instruction statement port)) code))

(define (write-instruction statement port)
  (let* ((inst (cadr statement))
	 (spec (hash-ref write-instruction-map inst)))
    (write-u8 (car spec) port)
    (write-arguments (cddr statement) (cddr spec) port)))
    
(define (write-arguments args spec port)
  (for-each (lambda (type arg)
	      (cond
	       ((eq? type 'u8) (write-u8 arg port))
	       ((eq? type 's24) (write-s24 arg port))
	       ((eq? type 'u30) (write-u30 arg port))
	       ((eq? type 'register) (write-u30 arg port))
	       ((eq? type 'integer) (write-id type arg port))
	       ((eq? type 'uinteger) (write-id type arg port))
	       ((eq? type 'double) (write-id type arg port))
	       ((eq? type 'string) (write-id type arg port))
	       ((eq? type 'multiname) (write-id type arg port))
	       ((eq? type 'method) (write-id type arg port))
	       ((eq? type 'offset) (write-offset arg port))
	       (else (error (format "Unknown argument type: ~a" type)))
	       ))
	    spec args))

(define (write-offset x port)
  (let ((offset (+ (file-position port) 3)))
    (write-s24 (- (cadr x) offset) port)))

;; Make sure if the type is correct and write the index.
(define (write-id type x port)
  (cond
   ((not (pair? x)) (error (format "Type error, expect: (~a i) given: ~a" type x)))
   ((eq? type (car x)) (write-u30 (cadr x) port))
   (else (error (format "Type error, expect: (~a i) given: ~a" type x)))))

(define (write-exception_info x port)
  (error "exception_info is not supported yet"))

; todo: in case of #f (entry is none)
; Write entries (xs) by func. The count is the number of entries plus one.
; func is an output function as (func object port)
(define (write-list1 func xs port)
  (if xs
      (let ((n (add1 (length xs))))
	(write-u30 n port)
	(for-each (lambda (x) (func x port)) xs))
      '()))

(define (write-list0 func xs port)
  (if xs
      (let ((n (length xs)))
	(write-u30 n port)
	(for-each (lambda (x) (func x port)) xs))
      '()))

(define (write-u8 i port)
  (write-byte i port))

(define (write-s24 i port)
  (let* ((i0 (if (positive? i)
		 i
		 (+ i #x1000000)))
	 (b0 (bitwise-and i0 #xff))
	 (b1 (bitwise-and (arithmetic-shift i0 -8) #xff))
	 (b2 (bitwise-and (arithmetic-shift i0 -16) #xff)))
    (write-byte b0 port)
    (write-byte b1 port)
    (write-byte b2 port)))

(define (write-d64 e port)
  (write-bytes (real->floating-point-bytes e 8 #f) port))

(define (write-u16 i port)
  (let ((b0 (bitwise-and #xff i))
	(b1 (arithmetic-shift i -8)))
    (write-byte b0 port)
    (write-byte b1 port)))

(define (write-s32 i port)
  (write-u32
   (if (negative? i)
       (+ i #x100000000)
       i) port))

(define (write-u30 i port)
  (write-u32 i port))

(define (write-u32 i port)
  (let ((e (bitwise-and (arithmetic-shift i -28) #x7f))
	(d (bitwise-and (arithmetic-shift i -21) #x7f))
	(c (bitwise-and (arithmetic-shift i -14) #x7f))
	(b (bitwise-and (arithmetic-shift i  -7) #x7f))
	(a (bitwise-and i #x7f)))
    (if (or (positive? b) (positive? c) (positive? d) (positive? e))
	(begin (write-byte (bitwise-ior a #x80) port)
	       (if (or (positive? c) (positive? d) (positive? e))
		   (begin (write-byte (bitwise-ior b #x80) port)
			  (if (or (positive? d) (positive? e))
			      (begin (write-byte (bitwise-ior c #x80) port)
				     (if (positive? e)
					 (begin (write-byte (bitwise-ior d #x80) port)
						(write-byte e port))
					 (write-byte d port)))
			      (write-byte c port)))
		   (write-byte b port)))
	(write-byte a port))))

;;;;;;;;;; ASM Form Encoder ;;;;;;;;;;

(define NEW-CONSTANT-DICT
  '((integer)
    (uinteger)
    (double)
    (string)
    (namespace)
    (multiname)))

(define (from-asm x)
  (if (not (eq? (car x) 'asm)) (error (format "This is not a ASM form: ~a" x)) '())
  (encode-id (cdr x) NEW-CONSTANT-DICT
    (lambda (body dict)
      (let* ((ns_set (ref 'ns_set body '()))
	     (method (ref 'method body))
	     (script (ref 'script body))
	     (method-signature (from-asm-method-signature method))
	     (method_body (from-asm-method_body method))
	     (constant (from-asm-constant dict ns_set)))
	`(abc
	  (minor_version 16) (major_version 46)
	  (constant_pool ,constant)
	  (method ,method-signature)
	  (metadata ())
	  (instance ())
	  (class ())
	  (script ,script)
	  (method_body ,method_body)
	  )
	))))

(define (from-asm-constant dict ns_set)
  (list
   (list 'integer (reverse (cdr (assoc 'integer dict))))
   (list 'uinteger (reverse (cdr (assoc 'uinteger dict))))
   (list 'double (reverse (cdr (assoc 'double dict))))
   (list 'string (reverse (cdr (assoc 'string dict))))
   (list 'namespace (reverse (cdr (assoc 'namespace dict))))
   (list 'ns_set ns_set)
   (list 'multiname (reverse (cdr (assoc 'multiname dict))))
))

(define (from-asm-method-signature xs)
  (map (lambda (x) (ref 'signature x)) xs))

;;; Extract method_bodies from list of method (signature + body)
;;; Each method has its index for the signature like (method 1)
(define (from-asm-method_body xs)
  (*from-asm-method_body xs 0))

(define (*from-asm-method_body xs index)
  (if (null? xs)
      '()
      (let* ((method (car xs))
	     (hint (from-asm-make-hint method))
	     (code (ref 'code method)))
	(cons `((method ,index)
		,@hint
		(code ,code)
		(exception ())
		(trait ()))
	      (*from-asm-method_body (cdr xs) (add1 index))))))

;;; Visit code and extract the hint information from it.
;;; (hint ((max_stack ?) (local_count ?) (init_scope_depth 0) (max_scope_depth ?)))
(define (from-asm-make-hint method)
  (let ((hint (assoc 'hint method)))
    (if hint
	(cadr hint)
	(*from-asm-make-hint (ref 'code method) 0 0 0 0 0))))

(define (*from-asm-make-hint lines stack max_stack local_count scope max_scope)
  (if (null? lines)
      `((max_stack ,max_stack)
	(local_count ,(add1 local_count))
	(init_scope_depth 0)
	(max_scope_depth ,max_scope))
      (if (symbol? (car lines))
	  (*from-asm-make-hint (cdr lines) stack max_stack local_count scope max_scope)
	  (let* ((line (car lines))
		 (spec (hash-ref hint-instruction-map (cadr line)))
		 (*stack (+ stack (from-asm-make-hint-eval 'stack line spec)))
		 (*max_stack (max max_stack *stack))
		 (local (from-asm-make-hint-eval 'local line spec))
		 (*local_count (max local_count local))
		 (*scope (+ scope (from-asm-make-hint-eval 'scope line spec)))
		 (*max_scope (max max_scope *scope)))
	    (*from-asm-make-hint (cdr lines) *stack *max_stack *local_count *scope *max_scope)))))

;;; (*from-asm-make-hint-eval '(1 2) '(+ 10 (arg 1))) => 12
(define (from-asm-make-hint-eval key command spec)
  (let ((expr (assoc key spec)))
    (if expr
	(*from-asm-make-hint-eval (cddr command) (cadr expr))
	0)))

;;; A tiny interpreter for hint specification
;;; (arg 0) is replaced to the value of first argument.
(define (*from-asm-make-hint-eval args expr)
  (if (pair? expr)
      (let ((params (map (lambda (each) (*from-asm-make-hint-eval args each))
			 (cdr expr)))
	    (key (car expr)))
	(cond
	 ((eq? key 'arg) (list-ref args (car params)))
	 ((eq? key '+) (apply + params))
	 ((eq? key '-) (apply - params))
	 (else (error (format "Unknown hint definition: ~a" expr)))))
      expr))

;;; Encode literals into index form, and build a constant dictionary.
;;; "hello" => (string 1)
;;;
;;; CPS style encoder. Encoded expression and new dictionary is passed to cont.
;;; cont is a function like (cont expr dictionary)

(define (encode-id x dict cont)
  (cond
   ((string? x) (encode-id-add 'string x dict cont))
   ((eq? x '*) (cont '(multiname 0) dict)) ;note: '* can be multiname, namespace, or ns_set. but never mind.
   ((not (pair? x)) (cont x dict))
   (else (encode-id-map encode-id x dict
	   (lambda (x dict) (encode-id-apply x dict cont))))))

;; Map the CPS function on the list
(define (encode-id-map proc xs dict cont)
  (if (null? xs)
      (cont xs dict)
      (proc (car xs) dict
        (lambda (x dict)
	  (encode-id-map proc (cdr xs) dict
            (lambda (xs dict) (cont (cons x xs) dict)))))))

;; Make a index depending on the first element.
;; (package "") => (namespace 1)
;; ((package "") "hello") => (multiname 1)

(define (encode-id-apply x dict cont)
  (cond
   ((namespace-id? x) (encode-id-add 'namespace x  dict cont))
   ((multiname-id? x) (encode-id-add 'multiname x  dict cont))
   ((eq? (car x) 'code) (encode-id-code x dict cont))
   (else (cont x dict))))

;; Register a value into the dictionary
(define (encode-id-add type x dict cont)
  (let* ((pool (assoc type dict))
	 (found (member x (cdr pool))))
    (if found
	(cont (list type (length found)) dict)
	(let* ((*pool (cons type  (cons x (cdr pool))))
	       (*dict (cons *pool (remove pool dict)))
	       (id (length (cdr *pool))))
	  (cont (list type id) *dict)))))

;; (namespace-id? (package "")) => #t
(define (namespace-id? x)
  (and (pair? x) ; maybe unnecessary
       (memq (car x) '(ns package internal protected explicit static private))))

;; (multiname-id? ((namespace 1) (string 1)) => #t
(define (multiname-id? x)
  (and (pair? x) ; maybe unnecessary
       (pair? (car x))
       (number? (cadr (car x)))
       (or (eq? (car (car x)) 'namespace)
	   (eq? (car (car x)) 'ns_set))))

;; Encode instructions
;; Because encoding of argument depends on each instruction, I need a
;; separate path to encode it again. It might be better to fix later.

(define (encode-id-code x dict cont)
  (encode-id-map *encode-id-code (cadr x) dict
    (lambda (x dict) (cont (list 'code x) dict))))

(define (*encode-id-code x dict cont)
  (if (symbol? x)
      (cont x dict)
      (let* ((pos (car x))
	     (symcode (cadr x))
	     (values (cddr x))
	     (types (cddr (hash-ref write-instruction-map symcode)))
	     (pairs (map (lambda (type value) (cons type value)) types values)))
	(encode-id-map encode-id-typed-add pairs dict 
		       (lambda (args dict)
			 (cont `(,pos ,symcode ,@args) dict))))))
  
(define (encode-id-typed-add pair dict cont)
  (let ((type (car pair))
	(value (cdr pair)))
    (if (memq type '(integer uinteger double))
	(encode-id-add type value dict cont)
	(cont value dict))))

;;; todo rename func name

(define (dprint obj)
  (pretty-print obj)
  obj)
