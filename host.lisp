;;; host.lisp -- serial driver for the Z80 Forth running in cpmsim (console on TCP).
;;; Run: PORT=n sbcl --script host.lisp COMMAND
;;;   load         send lisp.fs to the Forth, enter the Lisp, send prelude.scm
;;;   test-forth   kernel word tests over serial (fresh kernel expected)
;;;   test [FILE]  load, then run the R7RS subset (default ../r7rs-tests.scm), print pass counts
;;;   eval EXPR    load, then print the value of each EXPR
(require :sb-bsd-sockets)
(defpackage :z80host (:use :cl))
(in-package :z80host)

(defvar *dir* (directory-namestring *load-truename*))
(defvar *io*)
(defvar *log* nil "Echo the conversation to stdout.")

(defun connect ()
  (let ((port (parse-integer (or (sb-ext:posix-getenv "PORT") "4455"))))
    (loop repeat 100
          do (handler-case
                 (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
                   (sb-bsd-sockets:socket-connect s #(127 0 0 1) port)
                   (setf *io* (sb-bsd-sockets:socket-make-stream s :input t :output t :element-type 'character
                                                                   :external-format :latin-1 :buffering :full))
                   (return-from connect))
               (error () (sleep 0.1))))
    (error "no emulator on port ~d" port)))

(defun read-until (done-p)
  "Read characters until (DONE-P text) is true; return the text."
  (let ((buf (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for c = (read-char *io*)
          do (vector-push-extend c buf) (when *log* (write-char c) (finish-output))
          until (funcall done-p buf))
    (coerce buf 'simple-string)))

(defun ends-with (s suffix)
  (let ((n (length s)) (m (length suffix)))
    (and (>= n m) (string= s suffix :start1 (- n m)))))

(defun send (line)
  (write-string line *io*) (write-char #\Return *io*) (finish-output *io*))

(defun strip-echo (text)
  "Drop the echoed input line: everything up to the first CR LF."
  (let ((p (search (coerce '(#\Return #\Newline) 'string) text)))
    (if p (subseq text (+ p 2)) text)))

(defun forth (line)
  "Send one Forth line; return its output and whether it ended in ok."
  (send line)
  (let* ((out (read-until (lambda (b) (and (ends-with b (coerce '(#\Return #\Newline) 'string))
                                          (let ((l (string-right-trim '(#\Return #\Newline) b)))
                                            (or (ends-with l " ok") (ends-with l " ?") (search " error " l)))))))
         (body (string-right-trim '(#\Return #\Newline) out)))
    (values (string-trim " " (subseq body (min (length line) (length body))
                                     (if (ends-with body " ok") (- (length body) 3) (length body))))
            (ends-with body " ok"))))

(defparameter +prompt+ (coerce '(#\Return #\Newline #\> #\Space) 'string))

(defun lisp (line)
  "Send one Lisp line; return what it printed."
  (send line)
  (let ((out (read-until (lambda (b) (ends-with b +prompt+)))))
    (strip-echo (subseq out 0 (- (length out) (length +prompt+))))))

(defun file-lines (name)
  (with-open-file (s (merge-pathnames name *dir*))
    (loop for l = (read-line s nil) while l collect l)))

(defun load-target ()
  (read-until (lambda (b) (ends-with b (format nil "Forth~c~c" #\Return #\Newline))))
  (dolist (l (file-lines "lisp.fs"))
    (unless (or (string= (string-trim " " l) "") (eql 0 (search "\\ " l)))
      (multiple-value-bind (out ok) (forth l)
        (unless ok (error "lisp.fs: ~a~%  -> ~a" l out)))))
  (send "lisp")
  (read-until (lambda (b) (ends-with b +prompt+)))
  (dolist (l (file-lines "prelude.scm"))
    (unless (or (string= (string-trim " " l) "") (char= (char l 0) #\;))
      (let ((out (lisp l)))
        (when (search "error:" out) (error "prelude.scm: ~a~%  -> ~a" l out))))))

;;; ---- tests

(defparameter *forth-tests*
  '(("1 2 + ." "3") ("7 2 - ." "5") ("-3 4 * ." "-12") ("100 0 7 um/mod . ." "14 2")
    ("65535 2 um* . ." "1 -2") ("5 negate ." "-5") ("6 2/ . -6 2/ ." "3 -3")
    ("1 2 swap . ." "1 2") ("1 2 over . . ." "1 2 1") ("1 2 3 rot . . ." "1 3 2") ("1 2 nip ." "2")
    ("3 dup * ." "9") ("1 2 drop ." "1") ("1 2 2dup . . . ." "2 1 2 1")
    ("1 2 = . 2 2 = ." "0 -1") ("1 2 < . 2 1 < . -1 1 < ." "-1 0 -1") ("-1 1 u< ." "0") ("0 0= . 5 0= ." "-1 0")
    ("-5 0< . 5 0< ." "-1 0") ("12 10 and . 12 10 or . 12 10 xor ." "8 14 6") ("0 invert ." "-1")
    ("variable v 42 v ! v @ ." "42") ("5 v +! v @ ." "47") ("65 v c! v c@ ." "65")
    ("7 constant seven seven ." "7") (": sq dup * ; 12 sq ." "144")
    (": f if 1 else 2 then ; 0 f . 5 f ." "2 1")
    (": cnt 0 begin dup 5 < while 1+ repeat ; cnt ." "5")
    (": cd 3 begin dup . 1- dup 0= until drop ; cd" "3 2 1")
    (": fact dup 1 > if dup 1- recurse * then ; 7 fact ." "5040")
    ("1 2 3 depth . drop drop drop" "3") ("1 >r r@ r> . ." "1 1")
    (": hi .\" hello\" ; hi" "hello") ("char A ." "65") (": c [char] B ; c ." "66")
    ("3 ' sq execute ." "9") ("1 2 3 2 pick . drop drop drop" "1")
    (": t 7 throw ; ' t catch ." "7") ("1 2 ' + catch . ." "0 3")
    ("5 0 1 0 d+ . ." "0 6") ("s\" abc\" drop s\" abc\" drop 3 s= ." "-1")
    ("here 10 allot here swap - ." "10") ("frobnicate" "frobnicate ?")
    ("( a comment ) 4 ." "4") ("4 . \\ the rest is ignored" "4")))

(defun test-forth ()
  (read-until (lambda (b) (ends-with b (format nil "Forth~c~c" #\Return #\Newline))))
  (let ((pass 0) (fail 0))
    (loop for (in want) in *forth-tests*
          for got = (forth in)
          do (if (string= got want) (incf pass)
                 (progn (incf fail) (format t "FAIL ~a~%  want ~s~%  got  ~s~%" in want got))))
    (format t "forth kernel: ~d passed, ~d failed, ~d total~%" pass fail (+ pass fail))
    fail))

(defun conformance (file)
  (let ((pass 0) (fail 0))
    (dolist (l (with-open-file (s file) (loop for l = (read-line s nil) while l collect l)))
      (unless (or (string= (string-trim " " l) "") (char= (char l 0) #\;))
        (let* ((p (search "==>" l))
               (expr (string-trim " " (if p (subseq l 0 p) l)))
               (got (string-trim " " (lisp expr))))
          (cond ((search "error:" got) (incf fail) (format t "FAIL ~a~%  got ~a~%" expr got))
                ((null p) (incf pass))
                ((string= got (string-trim " " (subseq l (+ p 3))))  (incf pass))
                (t (incf fail) (format t "FAIL ~a~%  want ~a~%  got  ~a~%" expr (string-trim " " (subseq l (+ p 3))) got))))))
    (format t "conformance: ~d passed, ~d failed, ~d total~%" pass fail (+ pass fail))
    fail))

(defun main (args)
  (let ((cmd (first args)))
    (connect)
    (cond ((equal cmd "load") (load-target))
          ((equal cmd "test-forth") (sb-ext:exit :code (if (zerop (test-forth)) 0 1)))
          ((equal cmd "test")
           (load-target)
           (sb-ext:exit :code (if (zerop (conformance (or (second args) (merge-pathnames "../r7rs-tests.scm" *dir*)))) 0 1)))
          ((equal cmd "eval") (load-target) (dolist (e (rest args)) (format t "~a~%" (lisp e))))
          (t (format t "usage: host.lisp load | test-forth | test [FILE] | eval EXPR...~%")))))

(handler-case (main (cdr sb-ext:*posix-argv*))
  (sb-sys:interactive-interrupt () (sb-ext:exit :code 130))
  (error (e) (format t "~&FAILED: ~a~%" e) (sb-ext:exit :code 1)))
