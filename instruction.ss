;;; ABC instruction set specification

#lang scheme
(provide (all-defined-out))

;;; Specification
;;; (opcode (sym type type...) [option ...])
;;; options are
;;; (stack 1) means more 1 room is needed in the max_stack
;;; (scope 1) means at least 1 room is needed in max_scope
;;; (local 1) means at least 1 room is needed in local_count
;;; (stack (- 1 (arg 1))) means more (1 - the second argument) room is needed in max_stack

(define instructions
  '(
    (#x08 (kill register))
    (#x09 (label))
    (#x10 (jump offset))
    (#x11 (iftrue offset) (stack -1))
    (#x12 (iffalse offset) (stack -1))
    (#x15 (iflt offset) (stack -2))
    (#x17 (ifgt offset) (stack -2))
    (#x1c (pushwith) (stack -1) (scope 1))
    (#x1d (popscope) (scope -1))
    (#x21 (pushundefined) (stack 1))
    (#x24 (pushbyte u8) (stack 1))
    (#x25 (pushshort u30) (stack 1))
    (#x26 (pushtrue) (stack 1))
    (#x29 (pop) (stack -1))
    (#x2a (dup) (stack 1))
    (#x2b (swap))
    (#x2c (pushstring string) (stack 1))
    (#x2d (pushint integer) (stack 1))
    (#x2e (pushuint uinteger) (stack 1)) ; note: pushuint = 46 (0x2e) wrong description in spec P96
    (#x2f (pushdouble double) (stack 1)) ; note: pushdouble = 46 (0x2f) wrong description in spec
    (#x30 (pushscope) (stack -1) (scope 1))
    (#x40 (newfunction method) (stack 1))
    (#x41 (call u30) (stack (- -1 (arg 0))))
    (#x46 (callproperty multiname u30) (stack (- 0 (arg 1)))) ; FIXME: Doesn't consider runtime names
    (#x47 (returnvoid))
    (#x48 (returnvalue) (stack -1))
    (#x49 (constructsuper u30) (stack (- -1 (arg 0)))) ; FIXME: Doesn't consider runtime names
    (#x4a (constructprop multiname u30) (stack (- 0 (arg 1)))) ; FIXME: Doesn't consider runtime names
    (#x4c (callproplex multiname u30) (stack (- 0 (arg 1)))) ; FIXME: Doesn't consider runtime names
    (#x4f (callpropvoid multiname u30) (stack (- 1 (arg 1))))
    (#x56 (newarray u30) (stack (- 1 (arg 1))))
    (#x57 (newactivation) (stack 1))
    (#x58 (newclass u30)) ; todo ClassInfo
    (#x5a (newcatch u30) (stack 1)) ; todo exception info
    (#x5d (findpropstrict multiname) (stack 1)) ; FIXME: Doesn't consider runtime names
    (#x5e (findproperty multiname) (stack 1)) ; FIXME: Doesn't consider runtime names
    (#x5f (finddef multiname) (stack 1)) ; FIXME: Doesn't consider runtime names
    (#x61 (setproperty multiname) (stack -2)) ; FIXME: Doesn't consider runtime names
    (#x62 (getlocal register) (stack 1) (local (arg 0)))
    (#x63 (setlocal register) (stack -1))
    (#x65 (getscopeobject u8) (stack 1))
    (#x66 (getproperty multiname)) ; FIXME: Doesn't consider runtime names
    (#x68 (initproperty multiname) (stack -2)) ; FIXME: Doesn't consider runtime names
    (#x6c (getslot u30))
    (#x6d (setslot u30) (stack -2))
    (#x75 (convert_d))
    (#x80 (coerce multiname))
    (#x82 (coerce_a))
    (#x90 (negate))
    (#x91 (increment))
    (#x95 (typeof))
    (#x96 (not))
    (#xa0 (add) (stack -1))
    (#xa3 (divide) (stack -1))
    (#xab (equals) (stack -1))
    (#xac (strictequals) (stack -1))
    (#xad (lessthan) (stack -1))
    (#xb4 (in) (stack -1))
    (#xd0 (getlocal_0) (stack 1) (local 0))
    (#xd1 (getlocal_1) (stack 1) (local 1))
    (#xd2 (getlocal_2) (stack 1) (local 2))
    (#xd3 (getlocal_3) (stack 1) (local 3))
    (#xd4 (setlocal_0) (stack -1) (local 0))
    (#xd5 (setlocal_1) (stack -1) (local 1))
    (#xd6 (setlocal_2) (stack -1) (local 2))
    (#xd7 (setlocal_3) (stack -1) (local 3))
    (#xf0 (debugline u30))
    (#xf1 (debugfile string))
    ))

;;; Map from symbol to spec
;;; pushbyte => (#24 pushbyte u8) 
(define write-instruction-map
  (make-immutable-hash
   (map (lambda (each)
	  (let* ((spec (cadr each))
		 (sym (car spec))
		 (opcode (car each)))
	    (cons sym (cons opcode spec))))
	  instructions)))

;;; Map from opcode to spec
;;; #x24 => (pushbyte u8) 
(define read-instruction-map
  (make-immutable-hash
   (map (lambda (each) (cons (car each) (cadr each))) instructions)))

;;; Map from symcode to hint information
;;; pushscope => ((stack -1) (scope 1))
(define hint-instruction-map
  (make-immutable-hash
   (map (lambda (each)
	  (let ((sym (car (cadr each)))
		(hint (cddr each)))
	    (cons sym hint))) instructions)))
