;; test cases for swf.k

;; proc is (proc obj port)
(define to-bytes
  (lambda (proc obj) (call-with-output-bytes (lambda (port) (proc obj port)))))

(check (push-bits 1 1 ()) => '(1))
(check (push-bits 7 3 '(0 0 0)) => '(1 1 1 0 0 0))

(check (to-bytes write-fixed8dot8 1.0) => (bytes #x00 #x01))
(check (to-bytes write-fixed8dot8 1.5) => (bytes #x80 #x01))
(check (bits->u8 '(0 0 0 0  0 0 0 1)) => #x01)
(check (bits->u8 '(1 0 0 0  0 0 0 0)) => #x80)
(check (to-bytes write-u16 #xff) => (bytes #xff #x00))
(check (to-bytes write-u16 #x100) => (bytes #x00 #x01))
(check (to-bytes write-u32 #xff) => (bytes #xff #x00 #x00 #x00))
(check (to-bytes write-u32 #x100) => (bytes #x00 #x01 #x00 #x00))

(check (to-bytes *write-bits '(0 0 0 1 0 0 0 0)) => (bytes #x10))
(check (to-bytes *write-bits '(0 0 0 0 1 1 1 1  0 0 0 1 0 0 0 0)) => (bytes #x0f #x10))
(check (to-bytes write-bits '(0 0 0 0 1 1 1 1  0 0 0 1)) => (bytes #x0f #x10))
(check (to-bytes write-rgb '(#x12 #x34 #x56)) => (bytes #x12 #x34 #x56))

(check (make-spec 100 200 'Hello '("hello.abc"))
       => '((frame-size 2000 4000)
            (frame-rate . 30)
            (frame-count . 1)
            (tags (file-attribute)
                  (symbol-class . Hello)
                  (do-abc . "hello.abc")
                  (show-frame)
                  (end-tag))))

(check (call-with-output-bytes
        (lambda (out) (record-header-body 0 (bytes) out))) => (bytes 0 0))
(check (call-with-output-bytes
        (lambda (out) (record-header-body 1 (bytes 1 2 3) out))) => (bytes #x43 00 1 2 3))

(check (nbits-unsigned '(7)) => 3)
