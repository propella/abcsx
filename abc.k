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

;;;;;;;;;; ABC Form Reader ;;;;;;;;;;

;;;; Primitive Data Types Reader

;; Read one byte unsigned integer from the port.
(define read-u8 read-byte)

;; Read two bytes unsigned integer value from the port.
(define read-u16
  (lambda (port)
    (let ((l (read-byte port))
	  (h (read-byte port)))
      (+ (* h 256) l))))

;; Read a variale number integer from the port.
(define read-u32
  (lambda (port)
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
			      (+ (arithmetic-shift e 28) (arithmetic-shift d 21) (arithmetic-shift c 14) (arithmetic-shift b 7) a))))))))))))

(define read-u30 read-u32)

(define read-s24
  (lambda (port)
    (let ((b0 (read-byte port))
	  (b1 (read-byte port))
	  (b2 (read-byte port)))
      (if (positive? (bitwise-and b2 #b10000000))
	  (+ (* b2 #x10000) (* b1 #x100) b0 #x-1000000)
	  (+ (* b2 #x10000) (* b1 #x100) b0)))))

(define read-s32
  (lambda (port)
    (let ((u32 (read-u32 port)))
      (if (< u32 #x80000000)
	  u32
	  (- u32 #x100000000)))))

;; 8-byte IEEE-754 floating point value.
(define read-d64
  (lambda (port) (floating-point-bytes->real (read-bytes 8 port) #f)))

;; Read u30 from the port and construct a datum typed the type.
(define read-id
  (lambda (type port) `(,type ,(read-u30 port))))

;;;; List Reader

;; Read entries by func. The count is the number of entries plus one.
(define read-list1
  (lambda (func port)
    (let ((count (read-u30 port)))
      (if (= count 0)
	  '()
	  (build-list (- count 1) (lambda (_) (func port)))))))

;; Read entries by func. The count is the number of entries.
(define read-list0
  (lambda (func port)
    (let ((count (read-u30 port)))
      (build-list count (lambda (_) (func port))))))

;;;; Constant Pool Reader

;;; string_info reader

(define read-string_info
  (lambda (port)
    (let ((size (read-u30 port)))
      (bytes->string/utf-8 (read-bytes size port)))))

;;; namespace_info reader

(define CONSTANT_Namespace
  '((#x08 . ns)
   (#x16 . package)
   (#x17 . internal)
   (#x18 . protected)
   (#x19 . explicit)
   (#x1a . static)
   (#x05 . private)))

(define read-namespace_info (lambda (port)
  (let ((kind (read-u8 port))
	(name (read-id 'string port)))
    `(,(cdr (assoc kind CONSTANT_Namespace)) ,name))))

;;; ns_set_info reader

(define read-ns_set_info (lambda (port)
  (cons 'ns_set
	(read-list0 (lambda (p) (read-id 'namespace p)) port))))

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

(define multiname-attribute
  (lambda (x) `(attribute ,x)))

(define read-multiname_kind_QName
  (lambda (port)
    `(,(read-id 'namespace port)
      ,(read-id 'string port))))

(define read-multiname_kind_RTQName
  (lambda (port) (read-id 'string port)))

(define read-multiname_kind_RTQNameL
  (lambda (port) 'RTQNameL))

(define read-multiname_kind_Multiname
  (lambda (port)
    (let ((name (read-id 'string port))
	  (ns_set (read-id 'ns_set port)))
      `(,ns_set ,name)))) ;; Note: the order is swapped.

(define read-multiname_kind_MultinameL
  (lambda (port) (read-id 'ns_set port)))

(define transpose
  (lambda (dict)  (map (lambda (pair) (cons (cdr pair) (car pair))) dict)))

(define CONSTANT_Namespace_write (transpose CONSTANT_Namespace))

(define read-multiname_info (lambda (port)
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
     ))))

;;; cpool reader

(define read-cpool_info (lambda (port)
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
      (multiname ,multinames)))))

;;; method_info reader

(define	method-info-NEED_ARGUMENTS  (lambda (flags) (positive? (bitwise-and flags #x01))))
(define	method-info-NEED_ACTIVATION (lambda (flags) (positive? (bitwise-and flags #x02))))
(define	method-info-NEED_REST       (lambda (flags) (positive? (bitwise-and flags #x04))))
(define	method-info-HAS_OPTIONAL    (lambda (flags) (positive? (bitwise-and flags #x08))))
(define	method-info-SET_DXNS        (lambda (flags) (positive? (bitwise-and flags #x40))))
(define	method-info-HAS_PARAM_NAMES (lambda (flags) (positive? (bitwise-and flags #x80))))

(define read-option_detail ; todo
  (lambda (port) 
    `((val ,(read-u30 port))
      (kind ,(read-u8 port)))))

(define read-option_info
  (lambda (port) (read-list0 read-option_detail port)))

(define read-param_info
  (lambda (param_count port) (build-list param_count (lambda (_) (read-u30 port)))))

(define read-method_info
  (lambda (port)
    (let* ((param_count (read-u30 port))
	   (return_type (read-id 'multiname port))
	   (param_type (build-list param_count (lambda (_) (read-id 'multiname port))))
	   (name (read-id 'string port))
	   (flags (read-u8 port))
	   (options (if (method-info-HAS_OPTIONAL flags) (read-option_info port) '()))
	   (param_names (if (method-info-HAS_PARAM_NAMES flags) (read-param_info param_count port) '())))
      `((return_type ,return_type)
	(param_type ,param_type)
	(name ,name)
	(flags ,flags)
	(options ,options)
	(param_names ,param_names)))))

;;; metadata_info reader

(define read-item_info
  (lambda (port)
    `((key ,(read-id 'string port))
      (value ,(read-id 'string port)))))

(define read-metadata_info
  (lambda (port)
    `((name ,(read-id 'string port))
      (items ,(read-list0 read-item_info port)))))

;;; traits_info reader

(define Trait_Slot 0)
(define Trait_Method 1)
(define Trait_Getter 2)
(define Trait_Setter 3)
(define Trait_Class 4)
(define Trait_Function 5)
(define Trait_Const 6)

(define read-trait_slot
  (lambda (port)
    (let* ((slot_id (read-u30 port))
	   (type_name (read-id 'multiname port))
	   (vindex (read-u30 port))
	   (vkind (if (= vindex 0) 0 (read-u8 port))))
      `((slot_id ,slot_id)
	(type_name ,type_name)
	(vindex ,vindex)
	(vkind ,vkind)))))

(define read-trait_class
  (lambda (port)
    `((slot_id ,(read-u30 port))
      (classi ,(read-id 'class port)))))

(define read-trait_function
  (lambda (port)
    `((slot_id ,(read-u30 port))
      (function ,(read-u30 port)))))

(define read-trait_method
  (lambda (port)
    `((disp_id ,(read-u30 port))
      ,(read-id 'method port))))
  
(define read-traits_info_data (lambda (name kind port)
  (cond
   ((= kind Trait_Slot) `(slot . ,(read-trait_slot port)))
   ((= kind Trait_Method) `(method . ,(read-trait_method port)))
   ((= kind Trait_Getter) `(getter . ,(read-trait_method port)))
   ((= kind Trait_Setter) `(setter . ,(read-trait_method port)))
   ((= kind Trait_Class) `(class . ,(read-trait_class port)))
   ((= kind Trait_Function) `(function . ,(read-trait_function port)))
   ((= kind Trait_Const) `(const ,(read-trait_slot port)))
   (else (error (format "Unknown trait type ~a" kind)))
)))

(define read-traits_info (lambda (port)
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
		       (read-list0 read-metadata_info port)
		       '())))
;    (if (zero? name) (error "traits_info.name cannot be zero") '())
    `((kind ,typename)
      (name ,name)
      ,@others
      (metadata ,metadata)))))

(define read-traits_info*
  (lambda (port) (read-list0 read-traits_info port)))

;;; instance_info reader

(define CONSTANT_ClassSealed #x01)
(define CONSTANT_ClassFinal #x02)
(define CONSTANT_ClassInterface #x04)
(define CONSTANT_ClassProtectedNs #x08)

(define read-instance_info
  (lambda (port)
    (let* ((name (read-id 'multiname port))
	   (super_name (read-id 'multiname port))
	   (flags (read-u8 port))
	   (protectedNs (if (positive? (bitwise-and flags CONSTANT_ClassProtectedNs))
			    (read-u30 port)
			    '()))
	   (interface (read-list0 read-u30 port))
	   (iinit (read-id 'method port))
	   (trait (read-traits_info* port)))
      `((name ,name)
	(super_name ,super_name)
	(flags ,flags)
	(protectedNs (namespace ,protectedNs))
	(interface ,(map (lambda (i) `(multiname ,i)) interface))
	(iinit ,iinit)
	(trait ,trait)
	))))

;;; class_info reader

(define read-class_info
  (lambda (port)
    `((cinit ,(read-id 'method port))
      (trait ,(read-traits_info* port)))))

;;; script_info reader

(define read-script_info
  (lambda (port)
    `((init ,(read-id 'method port))
      (trait ,(read-traits_info* port)))))

;;; code reader

;; make a label from label symbol list and offset.
(define label-from-list
  (lambda (destination labels)
    (let ((found (memq destination labels)))
      (if found
	  (string->symbol (string-append "L" (number->string (length found))))
	  #f))))

(define destination-from-args
  (lambda (xs)
    (if (and (not (null? xs))
	     (pair? (car xs))
	     (eq? (car (car xs)) 'offset))
	(cadr (car xs))
	#f)))

;; Extract and insert jump labels
(define insert-label
  (lambda (code labels)
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
	      more))
	)))

;; label

(define find-label-new
  (lambda (pos labels)
    (if (memq pos labels)
	labels
	(cons pos labels))))

(define *find-label
  (lambda (line labels)
    (let* ((args (cddr line))
	   (offset (destination-from-args args)))
      (if offset
	  (find-label-new offset labels)
	  labels))))

;; Return all jump addreses in reverse order
;; The result numbers are used for label index. For example,
;; 3 in (3 4 5) is L2 because 3 is the third in reverse order.
(define find-label
  (lambda (code) (fold *find-label '() code)))

;; instruction reader

(define read-arguments
  (lambda (spec pos port)
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
	`(,pos ,opname ,@args)))))

(define read-instruction
  (lambda (decoded port)
    (let ((pos (file-position port))
	  (inst (read-byte port)))
      (if (eof-object? inst)
	  decoded
	  (let ((spec (hash-ref read-instruction-map inst)))
	    (read-instruction
	     (cons (read-arguments spec pos port) decoded) port))))))

(define read-instructions
  (lambda (port)
    (let* ((code (reverse (read-instruction '() port)))
	   (labels (find-label code)))
      (insert-label code labels))))

(define decode-instructions
  (lambda (code)
    (let ((code-port (open-input-bytes code)))
      (read-instructions code-port))))

(define read-exception_info
  (lambda (port)
    `((from ,(read-u30 port))
      (to ,(read-u30 port))
      (target ,(read-u30 port))
      (exc_type ,(read-id 'string port))
      (var_name ,(read-id 'string port)))))

;;; method_body_info reader

(define read-method_body_info
  (lambda (port)
    (let* ((method (read-u30 port))
	   (max_stack (read-u30 port))
	   (local_count (read-u30 port))
	   (init_scope_depth (read-u30 port))
	   (max_scope_depth (read-u30 port))
	   (code_length (read-u30 port))
	   (code (read-bytes code_length port))
	   (exception (read-list0 read-exception_info port))
	   (trait (read-traits_info* port)))
      `((method ,method)
	(max_stack ,max_stack)
	(local_count ,local_count)
	(init_scope_depth ,init_scope_depth)
	(max_scope_depth ,max_scope_depth)
	(code ,(decode-instructions code))
	(exception ,exception)
	(trait ,trait)))))

;;; abcFile reader

(define read-abc
  (lambda (port)
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
	(method_body ,method_body)))))

;;;;;;;;;; ASM Form Decoder ;;;;;;;;;;

;;; ASM form

(define make-method-body-reference
  (lambda (methods method-bodies index)
    (if (null? methods)
	'()
	(let* ((method-body
		(find (lambda (m)
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
		(make-method-body-reference (cdr methods) method-bodies (+ index 1)))))))

;; For some reason, a method_body refers a method instead of the other way.
;; So I have to make reverse reference by my hand.
(define set-method-body
  (lambda (abc)
    (let* ((method (assoc 'method abc))
	   (method-bodies (cadr (assoc 'method_body abc)))
	   (methods (make-method-body-reference (cadr method) method-bodies 0)))
      (map (lambda (tag)
	     (if (eq? (car tag) 'method) `(method ,methods)
		 tag))
	   abc))))

;; find one-base index from the constant pool
(define find-constant
  (lambda (type id default abc)
    (let* ((pool (cadr (assoc 'constant_pool abc)))
	   (table (cadr (assoc type pool))))
      (if (zero? id)
	  default
	  (decode-id (list-ref table (- id 1)) abc)))))

(define constant-id?
  (lambda (e)
    (and (pair? e)
	 (symbol? (car e))
	 (pair? (cdr e))
	 (number? (cadr e)))))

;;; Convert index expression to the literal
;;; (string 1) => "hello"

(define decode-id
  (lambda (e abc)
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
	       ;; ((eq? key 'method) (find-element 'method id abc))
	       (else (cons (decode-id (car e) abc) (decode-id (cdr e) abc)))))
	    (cons (decode-id (car e) abc) (decode-id (cdr e) abc)))
	e)))

; find zero-base index from a body
(define find-element
  (lambda (type id abc)
    (let* ((pool abc)
	   (table (cadr (assoc type pool))))
      (decode-id (list-ref table id) abc))))

(define to-asm
  (lambda (abc)
    (let* ((*abc (set-method-body (cdr abc)))
	   (**abc (decode-id *abc *abc))
	   (cpool (assoc 'constant_pool **abc))
	   (ns_set (assoc 'ns_set (cadr cpool)))
	   (method (assoc 'method **abc))
	   (instance (assoc 'instance **abc))
	   (class (assoc 'class **abc))
	   (script (assoc 'script **abc)))
      (list 'asm ns_set method instance class script))))

;;;;;;;;;; ABC Form Writer ;;;;;;;;;;

;;; Primitive Data Writer

(define write-u8 write-byte)

(define write-s24 (lambda (i port)
  (let* ((i0 (if (positive? i)
		 i
		 (+ i #x1000000)))
	 (b0 (bitwise-and i0 #xff))
	 (b1 (bitwise-and (arithmetic-shift i0 -8) #xff))
	 (b2 (bitwise-and (arithmetic-shift i0 -16) #xff)))
    (write-byte b0 port)
    (write-byte b1 port)
    (write-byte b2 port))))

(define write-d64 (lambda (e port)
  (write-bytes (real->floating-point-bytes e 8 #f) port)))

;; This can handle both 32 bits signed and unsigned integer
(define write-u32
  (lambda (i port)
    (let* ((sign (not (= 0 (bitwise-and i #x80000000))))
	   (rest (bitwise-and i #x7fffffff))
	   (e (bitwise-and (arithmetic-shift rest -28) #x7))
	   (d (bitwise-and (arithmetic-shift i -21) #x7f))
	   (c (bitwise-and (arithmetic-shift i -14) #x7f))
	   (b (bitwise-and (arithmetic-shift i  -7) #x7f))
	   (a (bitwise-and i #x7f)))
      (if (or (positive? b) (positive? c) (positive? d) (positive? e) sign)
	  (begin (write-byte (bitwise-ior a #x80) port)
		 (if (or (positive? c) (positive? d) (positive? e) sign)
		     (begin (write-byte (bitwise-ior b #x80) port)
			    (if (or (positive? d) (positive? e) sign)
				(begin (write-byte (bitwise-ior c #x80) port)
				       (if (or (positive? e) sign)
					   (begin (write-byte (bitwise-ior d #x80) port)
						  (if sign
						      (write-byte (bitwise-ior e #x8) port)
						      (write-byte e port)))
					   (write-byte d port)))
				(write-byte c port)))
		     (write-byte b port)))
	  (write-byte a port)))))

(define write-u16 (lambda (i port)
  (let ((b0 (bitwise-and #xff i))
	(b1 (arithmetic-shift i -8)))
    (write-byte b0 port)
    (write-byte b1 port))))

(define write-s32 write-u32)
(define write-u30 write-u32)

;; Write a u30 index with making sure if the type is correct and write the index.
(define write-id
  (lambda (type x port)
    (cond
     ((not (pair? x)) (error (format "Type error, expect: (~a i) given: ~a" type x)))
     ((eq? type (car x)) (write-u30 (cadr x) port))
     (else (error (format "Type error, expect: (~a i) given: ~a" type x))))))

;;; List Writer

;; Write the number of entries, and entries itself by func.
;; There are two versions.
;; write-list1's count is the number of entries plus one that is used in cpool info.
;; write-list1's count is the number of entries.

(define write-list1
  (lambda (func xs port)
    (let ((n (add1 (length xs))))
      (write-u30 n port)
      (for-each (lambda (x) (func x port)) xs))))

(define write-list0
  (lambda (func xs port)
    (let ((n (length xs)))
      (write-u30 n port)
      (for-each (lambda (x) (func x port)) xs))))

;;; cpool_info writer

(define write-string_info (lambda (x port)
  (let ((bytes (string->bytes/utf-8 x)))
    (write-u30 (bytes-length bytes) port)
    (write-bytes bytes port))))

(define write-namespace_info (lambda (x port)
  (let ((kind (car x))
	(name (cadr (cadr x))))
    (write-u8 (cdr (assoc kind CONSTANT_Namespace_write)) port)
    (write-u30 name port))))

(define write-ns_set_info (lambda (x port)
  (let ((xs (cdr x)))
    (write-list0 (lambda (x p) (write-u30 (cadr x) p)) xs port))))

(define write-multiname_kind_QName (lambda (x port)
  (let ((kind #x07)
	(ns (cadr (car x)))
	(name (cadr (cadr x))))
    (write-u8 kind port)
    (write-u30 ns port)
    (write-u30 name port))))

(define write-multiname_kind_Multiname (lambda (x port)
  (let ((kind #x09)
	(ns_set (cadr (car x)))
	(name (cadr (cadr x))))
    (write-u8 kind port)
    (write-u30 name port)
    (write-u30 ns_set port))))

(define write-multiname_info (lambda (x port)
  (cond
   ((eq? (car x) 'attribute) (error "attribute is not supported yet"))
   ((eq? (car x) 'RTQNameL) (error "RTQNameL is not supported yet"))
   ((eq? (car x) 'string) (error "RTQName is not supported yet"))
   ((eq? (car x) 'ns_set) (error "MultinameL is not supported yet"))
   ((eq? (car (car x)) 'namespace) (write-multiname_kind_QName x port))
   ((eq? (car (car x)) 'ns_set) (write-multiname_kind_Multiname x port))
   )))

(define write-cpool_info
  (lambda (x port)
    (write-list1 write-s32 (cadr (assoc 'integer x)) port)
    (write-list1 write-u32 (cadr (assoc 'uinteger x)) port)
    (write-list1 write-d64 (cadr (assoc 'double x)) port)
    (write-list1 write-string_info (cadr (assoc 'string x)) port)
    (write-list1 write-namespace_info (cadr (assoc 'namespace x)) port)
    (write-list1 write-ns_set_info (cadr (assoc 'ns_set x)) port)
    (write-list1 write-multiname_info (cadr (assoc 'multiname x)) port)))

;;; method_info writer

;; Find a key in the dictionary and return only the value.
;; dictionary is like ((key0 value0) (key1 value1) ...)
;; The optional third argument is returned if the key is not found.

(define ref (lambda (key dictionary . default)
  (let ((found (assoc key dictionary)))
    (cond (found (cadr found))
	  ((not (null? default)) (car default))
	  (else (error (format "'~a' is not found in ~a." key dictionary)))))))

(define write-method_info (lambda (x port)
  (let* ((param_type (ref 'param_type x))
	 (param_count (length param_type)))
    (write-u30 param_count port)
    (write-id 'multiname (ref 'return_type x) port)
    (for-each (lambda (type) (write-id 'multiname type port)) param_type) ;todo
    (write-id 'string (ref 'name x) port)
    (write-u8 (ref 'flags x) port)
    (if (method-info-HAS_OPTIONAL (ref 'flags x)) (error "HAS_OPTIONAL is not supported yet") '()) ;todo
    (if (method-info-HAS_PARAM_NAMES (ref 'flags x)) (error "HAS_OPTIONAL is not supported yet") '()) ;todo
  )))

(define write-exception_info (lambda (x port) (error "exception_info is not supported yet")))

;;; metadata_info writer

(define write-item_info
  (lambda (x port)
    (write-id 'string (ref 'key x) port)
    (write-id 'string (ref 'value x) port)))

(define write-metadata_info
  (lambda (x port)
    (write-id 'string (ref 'name x) port)
    (write-list0 write-item_info (ref 'items x) port)))

;;; traits_info writer

(define write-trait_slot
  (lambda (x port)
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
      ; metadata omitted (ATTR_Metadata)
      )))

(define write-trait_class ;; todo: slot_id and metadata can be converged with other slots.
  (lambda (x port)
    (let ((slot_id (ref 'slot_id x))
	  (classi (ref 'classi x))
	  (has-metadata (> (length (ref 'metadata x)) 0))
	  )
      (write-id 'multiname (ref 'name x) port)
      (write-u8 (if has-metadata
		    (bitwise-ior Trait_Class #b01000000) ; ATTR_Metadata
		    Trait_Class)
		port)
      (write-u30 slot_id port)
      (write-id 'class classi port)
      (if has-metadata
	  (write-list0 write-metadata_info (ref 'metadata x) port)
	  '()))))

(define write-traits_info
  (lambda (x port)
    (let ((kind (cadr (assoc 'kind x))))
      (cond
       ((eq? kind 'slot) (write-trait_slot x port))
       ((eq? kind 'method) (error "method trait is not supported yet"))
       ((eq? kind 'getter) (error "getter trait is not supported yet"))
       ((eq? kind 'setter) (error "setter trait is not supported yet"))
       ((eq? kind 'class) (write-trait_class x port))
       ((eq? kind 'function) (error "function trait is not supported yet"))
       ((eq? kind 'const) (error "const trait is not supported yet"))
       (else (error (format "Unknown trait type ~a" kind)))))))

(define write-instance_info
  (lambda (x port)
    (let ((name (ref 'name x))
	  (super_name (ref 'super_name x))
	  (flags (ref 'flags x))
	  (interface (ref 'interface x))
	  (iinit (ref 'iinit x)))
      (write-id 'multiname name port)
      (write-id 'multiname super_name port)
      (write-u8 flags port)
      (if (positive? (bitwise-and flags CONSTANT_ClassProtectedNs))
	  (write-id 'namespace (ref 'protectedNs x) port)
	  '())
      (write-list0 (lambda (x p) (write-id 'multiname x p)) interface port)
      (write-id 'method iinit port)
      (write-list0 write-traits_info (ref 'trait x) port)
)))

(define write-class_info
  (lambda (x port)
    (write-id 'method (ref 'cinit x) port)
    (write-list0 write-traits_info (ref 'trait x) port)))

;;; script_info writer

(define write-script_info
  (lambda (x port)
    (write-id 'method (ref 'init x) port)
    (write-list0 write-traits_info (ref 'trait x) port)))

;;; code writer

;; Return (inst args ...) without line number.
(define get-statement
  (lambda (line)
    (if (or (number? (car line))
	    (eq? (car line) '_))
	(cdr line)
	line)))

;; Caluculate instruction length

(define instruction-u30-length
  (lambda (value)
    (cond
     ((= (arithmetic-shift value -7) 0) 1)
     ((= (arithmetic-shift value -14) 0) 2)
     ((= (arithmetic-shift value -21) 0) 3)
     ((= (arithmetic-shift value -28) 0) 4)
     (else 5))))

(define instruction-arg-length
  (lambda (type value sum)
    (+ sum
       (cond
	((eq? type 'u8) 1)
	((eq? type 'offset) 3)
	((memq type '(u30 register)) (instruction-u30-length value))
	((memq type '(double integer method multiname string uinteger))
	 (instruction-u30-length (cadr value)))
	(else (error (format "Unknown type (instruction-arg-length): ~a ~a" type value)))))))

(define instruction-length
  (lambda (line)
    (let* ((statement (get-statement line))
	   (inst (car statement))
	   (spec (hash-ref write-instruction-map inst))
	   (arg-types (cddr spec))
	   (arg-values (cdr statement))
	   (arg-length (fold instruction-arg-length 0 arg-types arg-values)))
      (add1 arg-length))))

;; Add into dst-labels if the line is a label (symbol),
;; Return next position.
(define *write-label-extract
  (lambda (line pos dst-labels)
    (if (symbol? line)
	(begin (hash-set! dst-labels line pos)
	       pos)
	(+ (instruction-length line) pos))))

;; Return a dictionary where label -> position
(define write-label-extract
  (lambda (lines)
    (let ((dst-labels (make-hasheq)))
      (fold (lambda (line pos)
	       (*write-label-extract line pos dst-labels))
	     0
	     lines)
      dst-labels)))

;; Remove label symbol and replace label to offset.

(define write-label (lambda (label src-labels port)
  (let ((position (file-position port)))
    (if (symbol? label)
	(begin (hash-set! src-labels position label)
	       (write-s24 0 port) ; A place holder
	       )
	(let* ((offset (cadr label))
	       (diff (- offset position 3)))
	  (write-s24 diff port)) ; in case of (offset -10) form.
	))))

;; Replace a label to an offset reference
;; - A line of symbol (destination label) is ommited. e.g. L1 -> removed
;; - A line number is ommited if it exists. e.g. (42 jump L1) -> (jump (offset 7))
(define *write-label-replace (lambda (lines dst-labels)
  (if (null? lines)
      '()
      (let ((line (car lines)))
	(if (symbol? line)
	    (*write-label-replace (cdr lines) dst-labels) ; the line is label
	    (let* ((statement (get-statement line))
		   (inst (car statement))
		   (args (cdr statement))
		   (*line (if (and (= (length args) 1)
				   (symbol? (car args)))
			      (let ((offset (hash-ref dst-labels (car args))))
				`(,inst (offset ,offset)))
			      statement)))
	      (cons *line (*write-label-replace (cdr lines) dst-labels))))))))

(define write-label-replace
  (lambda (lines)
    (let ((dst-labels (write-label-extract lines)))
      (*write-label-replace lines dst-labels))))

(define write-offset (lambda (x port)
  (let ((offset (+ (file-position port) 3)))
    (write-s24 (- (cadr x) offset) port))))

(define write-arguments (lambda (args spec port)
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
	    spec args)))

;; Write a statement to the port.
;; A line number placeholder is already removed in write-label-replace.
(define write-instruction
  (lambda (statement port)
    (let* ((inst (car statement))
	   (spec (hash-ref write-instruction-map inst))
	   (args (cdr statement)))
      (write-u8 (car spec) port)
      (write-arguments args (cddr spec) port))))

(define write-instructions
  (lambda (code port)
    (for-each (lambda (statement) 
		(write-instruction statement port)) code)))

(define encode-instructions
  (lambda (code)
    (let ((*code (write-label-replace code)))
      (call-with-output-bytes
       (lambda (port)
	 (write-instructions *code port))))))

;;; method_body_info writer

(define write-method_body_info (lambda (x port)
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
    )))

;;; abcFile writer

(define write-abc
  (lambda (abc port)
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
      (write-list0 write-method_body_info (ref 'method_body *abc) port))))

;;;;;;;;;; ASM Form Encoder ;;;;;;;;;;

(define NEW-CONSTANT-DICT
  '((integer)
    (uinteger)
    (double)
    (string)
    (namespace)
    (multiname)))

(define from-asm-constant (lambda (dict ns_set)
  (list
   (list 'integer (reverse (cdr (assoc 'integer dict))))
   (list 'uinteger (reverse (cdr (assoc 'uinteger dict))))
   (list 'double (reverse (cdr (assoc 'double dict))))
   (list 'string (reverse (cdr (assoc 'string dict))))
   (list 'namespace (reverse (cdr (assoc 'namespace dict))))
   (list 'ns_set ns_set)
   (list 'multiname (reverse (cdr (assoc 'multiname dict))))
)))

(define from-asm-method-signature (lambda (xs)
  (map (lambda (x) (ref 'signature x)) xs)))

;; A tiny interpreter for hint specification
;; (arg 0) is replaced to the value of first argument.
;; (*from-asm-make-hint-eval '(1 2) '(+ 10 (arg 1))) => 12 (10 + the second argument)
(define *from-asm-make-hint-eval
  (lambda (args expr)
    (if (pair? expr)
	(let ((params (map (lambda (each) (*from-asm-make-hint-eval args each))
			   (cdr expr)))
	      (key (car expr)))
	  (cond
	   ((eq? key 'arg) (list-ref args (car params)))
	   ((eq? key '+) (apply + params))
	   ((eq? key '-) (apply - params))
	   (else (error (format "Unknown hint definition: ~a" expr)))))
	expr)))

(define from-asm-make-hint-eval
  (lambda (key line spec)
    (let ((expr (assoc key spec)))
;(pretty-print expr)
      (if expr
	  (*from-asm-make-hint-eval (cdr (get-statement line)) (cadr expr))
	  0))))

(define *from-asm-make-hint (lambda (lines stack max_stack local_count scope max_scope)
  (if (null? lines)
      `((max_stack ,max_stack)
	(local_count ,(add1 local_count))
	(init_scope_depth 0)
	(max_scope_depth ,max_scope))
      (if (symbol? (car lines))
	  (*from-asm-make-hint (cdr lines) stack max_stack local_count scope max_scope)
	  (let* ((statement (get-statement (car lines)))
		 (spec (hash-ref hint-instruction-map (car statement)))
		 (*stack (+ stack (from-asm-make-hint-eval 'stack statement spec)))
		 (*max_stack (max max_stack *stack))
		 (local (from-asm-make-hint-eval 'local statement spec))
		 (*local_count (max local_count local))
		 (*scope (+ scope (from-asm-make-hint-eval 'scope statement spec)))
		 (*max_scope (max max_scope *scope)))
	    (*from-asm-make-hint (cdr lines) *stack *max_stack *local_count *scope *max_scope))))))

;; Visit code and extract the hint information from it.
;; (hint ((max_stack ?) (local_count ?) (init_scope_depth 0) (max_scope_depth ?)))
(define from-asm-make-hint
  (lambda (method)
    (let* ((hint (assoc 'hint method))
	   (signature (ref 'signature method #f))
	   (local_count (if signature
			    (length (ref 'param_type signature))
			    0)))
      (if hint
	  (cadr hint)
	  (*from-asm-make-hint (ref 'code method) 0 0 local_count 0 0)))))

(define *from-asm-method_body
  (lambda (xs index)
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
		(*from-asm-method_body (cdr xs) (add1 index)))))))

;; Extract method_bodies from list of method (signature + body)
;; Each method has its index for the signature like (method 1)
(define from-asm-method_body
  (lambda (xs) (*from-asm-method_body xs 0)))

;;; encode-id

;; Map the CPS function on the list
(define encode-id-map (lambda (proc xs dict cont)
  (if (null? xs)
      (cont xs dict)
      (proc (car xs) dict
        (lambda (x dict)
	  (encode-id-map proc (cdr xs) dict
            (lambda (xs dict) (cont (cons x xs) dict))))))))

;; Make a index depending on the first element.
;; (package "") => (namespace 1)
;; ((package "") "hello") => (multiname 1)

;; Register a value into the dictionary
(define encode-id-add (lambda (type x dict cont)
  (let* ((pool (assoc type dict))
	 (found (member x (cdr pool))))
    (if found
	(cont (list type (length found)) dict)
	(let* ((*pool (cons type  (cons x (cdr pool))))
	       (*dict (cons *pool (delete pool dict)))
	       (id (length (cdr *pool))))
	  (cont (list type id) *dict))))))

;; (namespace-id? (package "")) => #t
(define namespace-id? (lambda (x)
  (and (pair? x) ; maybe unnecessary
       (memq (car x) '(ns package internal protected explicit static private)))))

;; (multiname-id? ((namespace 1) (string 1)) => #t
(define multiname-id? (lambda (x)
  (and (pair? x) ; maybe unnecessary
       (pair? (car x))
       (eq? (length (car x)) 2)
       (number? (cadr (car x)))
       (or (eq? (car (car x)) 'namespace)
	   (eq? (car (car x)) 'ns_set)))))

;; Encode instructions
;; Because encoding of argument depends on each instruction, I need a
;; separate path to encode it again. It might be better to fix later.
  
(define encode-id-typed-add (lambda (pair dict cont)
  (let ((type (car pair))
	(value (cdr pair)))
    (if (memq type '(integer uinteger double))
	(encode-id-add type value dict cont)
	(cont value dict)))))

(define *encode-id-code (lambda (x dict cont)
  (if (symbol? x)
      (cont x dict)
      (let* ((pos (car x))
	     (statement (get-statement x))
	     (symcode (car statement))
	     (values (cdr statement))
	     (types (cddr (hash-ref write-instruction-map symcode)))
	     (pairs (map (lambda (type value) (cons type value)) types values)))
	(encode-id-map encode-id-typed-add pairs dict 
		       (lambda (args dict)
			 (cont `(,symcode ,@args) dict)))))))

(define encode-id-code
  (lambda (x dict cont)
    (encode-id-map *encode-id-code (cadr x) dict
      (lambda (x dict) (cont (list 'code x) dict)))))

;; Encode literals into index form, and build a constant dictionary.
;; "hello" => (string 1)
;;
;; CPS style encoder. Encoded expression and new dictionary is passed to cont.
;; cont is a function like (cont expr dictionary)

(define encode-id-apply
  (lambda (x dict cont)
    (cond
     ((namespace-id? x) (encode-id-add 'namespace x  dict cont))
     ((multiname-id? x) (encode-id-add 'multiname x  dict cont))
     ((eq? (car x) 'code) (encode-id-code x dict cont))
     (else (cont x dict)))))

(define encode-id
  (lambda (x dict cont)
    (cond
     ((string? x) (encode-id-add 'string x dict cont))
     ((eq? x '*) (cont '(multiname 0) dict)) ;note: '* can be multiname, namespace, or ns_set. but never mind.
     ((not (pair? x)) (cont x dict))
     (else (encode-id-map encode-id x dict
       (lambda (x dict) (encode-id-apply x dict cont)))))))

(define from-asm (lambda (x)
  (if (not (eq? (car x) 'asm)) (error (format "This is not a ASM form: ~a" x)) '())
  (encode-id (cdr x) NEW-CONSTANT-DICT
    (lambda (body dict)
      (let* ((ns_set (ref 'ns_set body '()))
	     (method (ref 'method body))
	     (script (ref 'script body))
	     (method-signature (from-asm-method-signature method))
	     (metadata (ref 'metadata body '()))
	     (instance (ref 'instance body '()))
	     (class (ref 'class body '()))
	     (method_body (from-asm-method_body method))
	     (constant (from-asm-constant dict ns_set)))
	`(abc
	  (minor_version 16) (major_version 46)
	  (constant_pool ,constant)
	  (method ,method-signature)
	  (metadata ,metadata)
	  (instance ,instance)
	  (class ,class)
	  (script ,script)
	  (method_body ,method_body)
	  )
	)))))

;;; todo rename func name

(define dprint (lambda (obj)
  (pretty-print obj)
  obj))

;;;;;;;;;; The main public reader and writer ;;;;;;;;;;

;; De-serialize binary stream to ASM form
(define read-asm
  (lambda (port)
    (to-asm (read-abc port))))

;; Serialize ASM or ABC form to binary stream
;; If (car expr) is 'abc, the form is abc.
(define write-asm
  (lambda (form port)
    (let ((abc (cond
		((eq? (car form) 'asm) (from-asm form))
		((eq? (car form) 'abc) form)
		(else (error (format "This is not ABC nor ASM form: ~a" form))))))
      (write-abc abc port))))

;;;;;;;;;; Driver ;;;;;;;;;;

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
  
(define run
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
