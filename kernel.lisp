;;; kernel.lisp -- layer 3: the Forth kernel, as data for asm.lisp.
;;; Indirect-threaded. BC = IP, SP = data stack, IX = return stack, HL/DE/A scratch.
;;; Header: link(2) | flags+len(1) | name | CFA(2) | body.  Flags: #x80 immediate, #x40 hidden.
;;; Run: sbcl --script kernel.lisp OUT.mos   writes the image with a Mostek header (load at 0).
(load (merge-pathnames "asm.lisp" *load-truename*))
(in-package :z80)

;;; Memory map (64 KiB): image + dictionary 0000-E7FF, TIB E800-EBFF,
;;; return stack EC00-F7FF (IX starts at F800), data stack F800-FFFF (SP starts at 0000).
(defconstant +tib+ #xE800)
(defconstant +tib-size+ 1023)
(defconstant +r0+ #xF800)
(defconstant +console-status+ 40)  ; z80pack cpmsim: TCP console 1 status (bit0 rx, bit1 tx)
(defconstant +console-data+ 41)

(defvar *forms* '())
(defvar *link* 0)
(defvar *current* nil)
(defun emit (&rest forms) (dolist (f forms) (push f *forms*)))
(defun wlabel (name) (intern (format nil "W:~a" name) :z80))
(defun name-of (x) (if (stringp x) x (string-downcase (symbol-name x))))

(defun header (name &optional immediate)
  (let ((hdr (gensym "HDR")))
    (setf *current* name)
    (emit hdr `(dw ,*link*) `(db ,(+ (length name) (if immediate #x80 0))) `(db ,name) (wlabel name))
    (setf *link* hdr)))

(defun code (name &rest asm)
  "A primitive: CFA points at the code that follows it."
  (let ((body (gensym "CODE")))
    (header (name-of name))
    (emit `(dw ,body) body)
    (apply #'emit asm)
    (emit '(jp next))))

(defun compile-tokens (tokens)
  "Thread TOKENS, resolving control words at build time like a Forth compiler would."
  (let ((cs '()))
    (dolist (tk tokens)
      (let ((s (and (symbolp tk) (symbol-name tk))))
        (flet ((label () (let ((l (gensym "L"))) (emit l) l))
               (fwd (op) (let ((l (gensym "F"))) (emit `(dw ,(wlabel op) ,l)) l)))
          (cond ((integerp tk) (emit `(dw ,(wlabel "lit") ,(logand tk #xFFFF))))
                ((equal s "IF") (push (fwd "0branch") cs))
                ((equal s "ELSE") (let ((f (fwd "branch"))) (emit (pop cs)) (push f cs)))
                ((equal s "THEN") (emit (pop cs)))
                ((equal s "BEGIN") (push (label) cs))
                ((equal s "WHILE") (let ((f (fwd "0branch"))) (push f (cdr cs))))
                ((equal s "REPEAT") (emit `(dw ,(wlabel "branch") ,(pop cs))) (emit (pop cs)))
                ((equal s "UNTIL") (emit `(dw ,(wlabel "0branch") ,(pop cs))))
                ((equal s "AGAIN") (emit `(dw ,(wlabel "branch") ,(pop cs))))
                ((equal s "RECURSE") (emit `(dw ,(wlabel *current*))))
                ((and (consp tk) (eq (car tk) 'quote)) (emit `(dw ,(wlabel "lit") ,(wlabel (name-of (cadr tk))))))
                ((and (consp tk) (symbolp (car tk)) (equal (symbol-name (car tk)) "ADDR")) (emit `(dw ,(wlabel "lit") ,(cadr tk))))
                ((and (consp tk) (member (car tk) '("s\"" ".\"") :test #'equal))
                 (emit `(dw ,(wlabel "(s\")")) `(db ,(length (cadr tk))) `(db ,(cadr tk)))
                 (when (equal (car tk) ".\"") (emit `(dw ,(wlabel "type")))))
                (t (emit `(dw ,(wlabel (name-of tk)))))))))
    (when cs (error "unbalanced control flow in ~a" *current*))))

(defun xt (name) (list 'quote name))
(defun dq (s) (list ".\"" s))
(defun colon (name &rest tokens)
  (let ((imm (eq (car tokens) :immediate)))
    (header (name-of name) imm)
    (emit '(dw docol))
    (compile-tokens (if imm (cdr tokens) tokens))
    (emit `(dw ,(wlabel "exit")))))

(defun var (name &optional (init 0))
  (header (name-of name)) (emit '(dw dovar) `(dw ,init)))
(defun const (name value)
  (header (name-of name)) (emit '(dw docon) `(dw ,value)))

(defun flag-code (test)
  "Push -1 when the condition jump TEST (e.g. z, c) is taken, else 0."
  (let ((l (gensym)))
    `((ld hl 0) (jr ,(if (eq test 'z) 'nz 'nc) ,l) (dec hl) ,l (push hl))))

(defun build ()
  (setf *forms* '() *link* 0)
  ;; boot and inner interpreter
  (emit '(jp start)
        'next '(ld a (@ bc)) '(ld l a) '(inc bc) '(ld a (@ bc)) '(ld h a) '(inc bc)
        '(ld e (@ hl)) '(inc hl) '(ld d (@ hl)) '(inc hl) '(ex de hl) '(jp (@ hl))
        'docol '(dec ix) '(ld (@ ix 0) b) '(dec ix) '(ld (@ ix 0) c) '(ld b d) '(ld c e) '(jp next)
        'dovar '(push de) '(jp next)
        'docon '(ex de hl) '(ld e (@ hl)) '(inc hl) '(ld d (@ hl)) '(push de) '(jp next)
        'ipsave '(dw 0)
        'start `(ld sp 0) `(ld ix ,+r0+) '(ld bc cold) '(jp next)
        'cold `(dw ,(wlabel "cold")))
  ;; primitives
  (code 'exit '(ld c (@ ix 0)) '(inc ix) '(ld b (@ ix 0)) '(inc ix))
  (code 'lit '(ld a (@ bc)) '(ld l a) '(inc bc) '(ld a (@ bc)) '(ld h a) '(inc bc) '(push hl))
  (code 'branch 'branch '(ld a (@ bc)) '(ld l a) '(inc bc) '(ld a (@ bc)) '(ld b a) '(ld c l))
  (code '0branch '(pop hl) '(ld a h) '(or l) '(jr z branch) '(inc bc) '(inc bc))
  (header "execute") (emit '(dw execute) 'execute '(pop hl) '(ld e (@ hl)) '(inc hl) '(ld d (@ hl)) '(inc hl) '(ex de hl) '(jp (@ hl)))
  (code 'dup '(pop hl) '(push hl) '(push hl))
  (code 'drop '(pop hl))
  (code 'swap '(pop hl) '(ex (@ sp) hl) '(push hl))
  (code 'over '(pop hl) '(pop de) '(push de) '(push hl) '(push de))
  (code 'rot '(pop de) '(pop hl) '(ex (@ sp) hl) '(push de) '(push hl))
  (code 'nip '(pop hl) '(pop de) '(push hl))
  (code '>r '(pop hl) '(dec ix) '(ld (@ ix 0) h) '(dec ix) '(ld (@ ix 0) l))
  (code 'r> '(ld l (@ ix 0)) '(inc ix) '(ld h (@ ix 0)) '(inc ix) '(push hl))
  (code 'r@ '(ld l (@ ix 0)) '(ld h (@ ix 1)) '(push hl))
  (code 'sp@ '(ld hl 0) '(add hl sp) '(push hl))
  (code 'sp! '(pop hl) '(ld sp hl))
  (code 'rp@ '(push ix))
  (code 'rp! '(pop ix))
  (code 'pick '(pop hl) '(add hl hl) '(add hl sp) '(ld e (@ hl)) '(inc hl) '(ld d (@ hl)) '(push de))
  (code '@ '(pop hl) '(ld e (@ hl)) '(inc hl) '(ld d (@ hl)) '(push de))
  (code '! '(pop hl) '(pop de) '(ld (@ hl) e) '(inc hl) '(ld (@ hl) d))
  (code 'c@ '(pop hl) '(ld l (@ hl)) '(ld h 0) '(push hl))
  (code 'c! '(pop hl) '(pop de) '(ld (@ hl) e))
  (code '+! '(pop hl) '(pop de) '(ld a (@ hl)) '(add a e) '(ld (@ hl) a) '(inc hl) '(ld a (@ hl)) '(adc a d) '(ld (@ hl) a))
  (code '+ '(pop hl) '(pop de) '(add hl de) '(push hl))
  (code '- '(pop de) '(pop hl) '(or a) '(sbc hl de) '(push hl))
  (dolist (op '(and or xor))
    (code op '(pop hl) '(pop de) '(ld a l) (list op 'e) '(ld l a) '(ld a h) (list op 'd) '(ld h a) '(push hl)))
  (code 'd+ '(ld (@ ipsave) bc) '(pop bc) '(pop de) '(pop hl) '(ex (@ sp) hl) '(add hl de) '(ex (@ sp) hl) '(adc hl bc) '(push hl) '(ld bc (@ ipsave)))
  (code 'invert '(pop hl) '(ld a l) '(cpl) '(ld l a) '(ld a h) '(cpl) '(ld h a) '(push hl))
  (code 'negate '(pop de) '(ld hl 0) '(or a) '(sbc hl de) '(push hl))
  (code '2* '(pop hl) '(add hl hl) '(push hl))
  (code '2/ '(pop hl) '(sra h) '(rr l) '(push hl))
  (code 'u2/ '(pop hl) '(srl h) '(rr l) '(push hl))
  (code '1+ '(pop hl) '(inc hl) '(push hl))
  (code '1- '(pop hl) '(dec hl) '(push hl))
  (apply #'code '= '(pop hl) '(pop de) '(or a) '(sbc hl de) (flag-code 'z))
  (apply #'code '0= '(pop hl) '(ld a h) '(or l) (flag-code 'z))
  (apply #'code '0< '(pop hl) '(add hl hl) (flag-code 'c))
  (apply #'code 'u< '(pop de) '(pop hl) '(or a) '(sbc hl de) (flag-code 'c))
  (let ((same (gensym)) (done (gensym)))
    (apply #'code '< '(pop de) '(pop hl) '(ld a h) '(xor d) `(jp p ,same) '(ld a h) '(rla) `(jr ,done)
           same '(or a) '(sbc hl de) done (flag-code 'c)))
  (let ((l (gensym)) (s (gensym)))
    (code 'um* '(ld (@ ipsave) bc) '(pop bc) '(pop de) '(ld hl 0) '(ld a 16)
          l '(add hl hl) '(rl e) '(rl d) `(jr nc ,s) '(add hl bc) `(jr nc ,s) '(inc de)
          s '(dec a) `(jr nz ,l) '(push hl) '(push de) '(ld bc (@ ipsave))))
  (let ((l (gensym)) (big (gensym)) (set (gensym)) (nx (gensym)))
    (code 'um/mod '(ld (@ ipsave) bc) '(pop bc) '(pop hl) '(pop de) '(ld a 16)
          l '(sla e) '(rl d) '(adc hl hl) `(jr c ,big) '(or a) '(sbc hl bc) `(jr nc ,set) '(add hl bc) `(jr ,nx)
          big '(or a) '(sbc hl bc) set '(inc e) nx '(dec a) `(jr nz ,l) '(push hl) '(push de) '(ld bc (@ ipsave))))
  (let ((l (gensym)))
    (code 'key l `(in a (@ ,+console-status+)) '(and 1) `(jr z ,l) `(in a (@ ,+console-data+)) '(ld l a) '(ld h 0) '(push hl)))
  (let ((l (gensym)))
    (code 'emit '(pop hl) l `(in a (@ ,+console-status+)) '(and 2) `(jr z ,l) '(ld a l) `(out (@ ,+console-data+) a)))
  (apply #'code 'key? `(in a (@ ,+console-status+)) '(and 1) '(xor 1) (flag-code 'z))
  (let ((s (gensym)))
    (code 'cmove '(ld (@ ipsave) bc) '(pop bc) '(pop de) '(pop hl) '(ld a b) '(or c) `(jr z ,s) '(ldir) s '(ld bc (@ ipsave))))
  (let ((l (gensym)) (d (gensym)))
    (code 'fill '(ld (@ ipsave) bc) '(pop de) '(pop bc) '(pop hl) l '(ld a b) '(or c) `(jr z ,d)
          '(ld (@ hl) e) '(inc hl) '(dec bc) `(jr ,l) d '(ld bc (@ ipsave))))
  (let ((l (gensym)) (eq (gensym)) (ne (gensym)) (out (gensym)))
    (code 's= '(ld (@ ipsave) bc) '(pop bc) '(pop de) '(pop hl) l '(ld a b) '(or c) `(jr z ,eq)
          '(ld a (@ de)) '(cp (@ hl)) `(jr nz ,ne) '(inc hl) '(inc de) '(dec bc) `(jr ,l)
          eq '(ld hl -1) `(jr ,out) ne '(ld hl 0) out '(push hl) '(ld bc (@ ipsave))))
  (code 'bye '(halt))
  ;; variables and constants
  (var 'state) (var '>in) (var "#tib") (var 'handler) (var 'erraddr) (var 'errlen) (var 'lastc)
  (var 'dp 'dict-end)
  (var 'latest 'last-header)
  (const 'tib +tib+) (const 'r0 +r0+) (const 's0 0) (const 'bl 32)
  (const 'docol 'docol) (const 'dovar 'dovar) (const 'docon 'docon)
  ;; high-level words
  (colon '2drop 'drop 'drop)
  (colon '2dup 'over 'over)
  (colon '?dup 'dup 'if 'dup 'then)
  (colon '<> '= '0=)
  (colon '> 'swap '<)
  (colon '* 'um* 'drop)
  (colon 'here 'dp '@)
  (colon 'allot 'dp '+!)
  (colon "," 'here '! 2 'allot)
  (colon 'c\, 'here 'c! 1 'allot)
  (colon 'count 'dup '1+ 'swap 'c@)
  (colon 'type 'begin 'dup 'while 'swap 'dup 'c@ 'emit '1+ 'swap '1- 'repeat '2drop)
  (colon 'cr 13 'emit 10 'emit)
  (colon 'space 32 'emit)
  (colon "(u.)" 0 10 'um/mod '?dup 'if 'recurse 'then 48 '+ 'emit)
  (colon 'u. "(u.)" 'space)
  (colon "." 'dup '0< 'if 45 'emit 'negate 'then 'u.)
  (colon 'depth 'sp@ 'negate '2/)
  (colon 'catch 'sp@ '>r 'handler '@ '>r 'rp@ 'handler '! 'execute 'r> 'handler '! 'r> 'drop 0)
  (colon 'throw '?dup 'if 'handler '@ 'rp! 'r> 'handler '! 'r> 'swap '>r 'sp! 'drop 'r> 'then)
  ;; line input: echo, backspace, CR/LF/CRLF all end a line
  (colon 'eol? 'dup 13 '= 'over 10 '= 'or)
  (colon 'refill 0 "#tib" '! 0 '>in '!
         'begin 'key 'dup 10 '= 'lastc '@ 13 '= 'and 'if 'drop 0 'lastc '! 'key 'then 'dup 'lastc '!
                'eol? '0= 'while
                'dup 8 '= 'over 127 '= 'or
                'if 'drop "#tib" '@ 'if -1 "#tib" '+! 8 'emit 'space 8 'emit 'then
                'else "#tib" '@ +tib-size+ '< 'if 'dup 'emit 'tib "#tib" '@ '+ 'c! 1 "#tib" '+! 'else 'drop 'then 'then
         'repeat 'drop)
  (colon 'in-char '>in '@ "#tib" '@ '< 'if 'tib '>in '@ '+ 'c@ 'else 0 'then)
  (colon 'more? '>in '@ "#tib" '@ '<)
  (colon 'parse-name
         'begin 'more? 'if 'in-char 33 '< 'else 0 'then 'while 1 '>in '+! 'repeat
         'tib '>in '@ '+ 0
         'begin 'in-char 32 '> 'while '1+ 1 '>in '+! 'repeat)
  (colon 'parse '>r 'tib '>in '@ '+ 0
         'begin 'more? 'if 'in-char 'r@ '<> 'else 0 'then 'while '1+ 1 '>in '+! 'repeat
         'r> 'drop 'more? 'if 1 '>in '+! 'then)
  (colon 'find 'latest '@
         'begin 'dup 'while
           'dup 2 '+ 'c@ 95 'and 2 'pick '=
           'if 'dup 3 '+ 3 'pick 3 'pick 's=
             'if 'nip 'nip 'dup 2 '+ 'c@ 128 'and 'if 1 'else -1 'then 'swap
                 'dup 2 '+ 'c@ 31 'and '+ 3 '+ 'swap 'exit 'then 'then
           '@ 'repeat)
  (colon 'digit? 48 '- 'dup 10 'u<)
  (colon 'number 'over 'c@ 45 '= 'dup '>r 'if '1- 'swap '1+ 'swap 'then
         'dup '0= 'if '2drop 'r> 'drop 0 'exit 'then
         0 '>r 'begin 'dup 'while 'over 'c@ 'digit? '0= 'if 'drop '2drop 'r> 'r> '2drop 0 'exit 'then
                   'r> 10 '* '+ '>r '1- 'swap '1+ 'swap 'repeat
         '2drop 'r> 'r> 'if 'negate 'then -1)
  (colon 'undefined 'errlen '! 'erraddr '! -13 'throw)
  (colon 'interpret
         'begin 'parse-name 'dup 'while
           'find '?dup
           'if -1 '= 'state '@ 'and 'if "," 'else 'execute 'then
           'else '2dup 'number 'if '>r '2drop 'r> 'state '@ 'if ''lit "," "," 'then 'else 'undefined 'then 'then
         'repeat '2drop)
  (colon '.error 'dup -13 '= 'if 'drop 'space 'erraddr '@ 'errlen '@ 'type (dq " ?")
         'else (dq " error ") "." 'then)
  (colon 'quit 'r0 'rp! 0 'state '! 0 'handler '!
         'begin 'refill ''interpret 'catch '?dup 'if '.error 's0 'sp! 0 'state '! 'else (dq " ok") 'then 'cr 'again)
  ;; compiler
  (colon 'header 'parse-name 'here 'latest '@ "," 'latest '! 'dup 'c\, 'here 'swap 'dup 'allot 'cmove)
  (colon 'create 'header 'dovar ",")
  (colon 'variable 'create 0 ",")
  (colon 'constant 'header 'docon "," ",")
  (colon 'flags! 'latest '@ 2 '+ 'dup 'c@ 'rot 'xor 'swap 'c!)  ; toggle header flag bits
  (colon "]" -1 'state '!)
  (colon "[" :immediate 0 'state '!)
  (colon ":" 'header 'docol "," 64 'flags! "]")
  (colon ";" :immediate ''exit "," 64 'flags! "[")
  (colon 'immediate 128 'flags!)
  (colon "'" 'parse-name 'find '0= 'if 'undefined 'then)
  (colon "[']" :immediate "'" ''lit "," ",")
  (colon 'literal :immediate ''lit "," ",")
  (colon 'if :immediate ''0branch "," 'here 0 ",")
  (colon 'then :immediate 'here 'swap '!)
  (colon 'else :immediate ''branch "," 'here 0 "," 'swap 'here 'swap '!)
  (colon 'begin :immediate 'here)
  (colon 'until :immediate ''0branch "," ",")
  (colon 'again :immediate ''branch "," ",")
  (colon 'while :immediate ''0branch "," 'here 0 "," 'swap)
  (colon 'repeat :immediate ''branch "," "," 'here 'swap '!)
  (colon 'recurse :immediate 'latest '@ 'dup 2 '+ 'c@ 31 'and '+ 3 '+ ",")
  (colon "(" :immediate 41 'parse '2drop)
  (colon "\\" :immediate "#tib" '@ '>in '!)
  (colon "(s\")" 'r> 'count '2dup '+ '>r)
  (colon ",str" 1 '>in '+! 34 'parse 'dup 'c\, 'here 'swap 'dup 'allot 'cmove)
  (colon "s\"" :immediate 'state '@ 'if (xt "(s\")") "," ",str" 'else 1 '>in '+! 34 'parse 'then)
  (colon ".\"" :immediate (xt "(s\")") "," ",str" ''type ",")
  (colon 'char 'parse-name 'drop 'c@)
  (colon "[char]" :immediate 'char ''lit "," ",")
  (colon 'words 'latest '@ 'begin 'dup 'while 'dup 3 '+ 'over 2 '+ 'c@ 31 'and 'type 'space '@ 'repeat 'drop)
  (colon 'cold (dq "Z80 Forth") 'cr 'quit)
  (emit `(equ last-header ,*link*) 'dict-end)
  (assemble (reverse *forms*)))

(let ((out (second sb-ext:*posix-argv*)))
  (when out
    (let ((bytes (build)))
      (with-open-file (s out :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (write-sequence #(#xFF 0 0) s) (write-sequence bytes s))
      (format t "kernel: ~d bytes~%" (length bytes)))))
