\ lisp.fs -- layer 4: a small Scheme written in Forth words, loaded through the Z80 Forth.
\ Values are 16-bit. A heap object is one 4-byte cell (car, cdr) plus a type byte:
\ 1 pair  2 symbol (car: counted name, cdr: global value)  3 closure ((params . body) . env)
\ 4 primitive (car: xt)  5 number (32-bit: car low, cdr high)  6 string (car: char list)
\ 7 vector (car: element list)  8 macro (car: transformer closure).  Bit 7: GC mark.
\ Immediates: () = 0, #f = 2, #t = 4, unspecified = 6, unbound = 8.

: tuck swap over ;
: -rot rot rot ;
: 2swap rot >r rot r> ;
: 0<> 0= 0= ;
: abs dup 0< if negate then ;
: dnegate invert swap invert swap 1 0 d+ ;
: d- dnegate d+ ;
: dabs dup 0< if dnegate then ;
: d= rot = >r = r> and ;
: d< rot 2dup = if 2drop u< else > nip nip then ;
: d> 2swap d< ;
: d<= d> 0= ;
: d>= d< 0= ;
: d* >r swap >r 2dup um* 2swap r> * swap r> * + + ;
: mu/mod >r 0 r@ um/mod r> swap >r um/mod r> ;

0 constant nil
2 constant false
4 constant true
6 constant unspec
8 constant unbound
: bool if true else false then ;

\ ---- heap and garbage collector (mark-sweep; stacks are scanned conservatively)
7000 constant #cells
create heap0 #cells 4 * 4 + allot
heap0 3 + -4 and constant heap
create types #cells allot
variable free
variable symlist
variable errobj
variable errmsg
variable errlen
variable display?
: cell# heap - 2/ 2/ ;
: type@ cell# types + c@ 127 and ;
: heap? dup heap - #cells 4 * u< over 3 and 0= and if cell# types + c@ 127 and 0<> else drop 0 then ;
: car @ ;
: cdr 2 + @ ;
: cadr cdr car ;
: cddr cdr cdr ;
: caddr cddr car ;
: set-cdr 2 + ! ;
: typ? over heap? if swap type@ = else 2drop 0 then ;
: pair? 1 typ? ;
: lerror errlen ! errmsg ! errobj ! -100 throw ;
: mark
  begin dup heap? while
    dup cell# types + dup c@ dup 128 and if 2drop drop exit then
    128 or swap c!
    dup type@ dup 1 = over 3 = or swap 8 = or if dup car recurse cdr else
    dup type@ 2 = if cdr else
    dup type@ dup 6 = swap 7 = or if car else drop 0 then then then
  repeat drop ;
: scan-data sp@ begin dup while dup @ mark 2 + repeat drop ;
: scan-return rp@ begin dup r0 u< while dup @ mark 2 + repeat drop ;
: sweep 0 free ! 0
  begin dup #cells < while
    dup types + dup c@ dup 128 and
    if 127 and swap c! else drop 0 swap c! dup 4 * heap + free @ over set-cdr free ! then
  1+ repeat drop ;
: gc symlist @ mark scan-data scan-return sweep ;
: alloc free @ 0= if gc free @ 0= if 0 s" out of memory" lerror then then
  free @ dup cdr free ! tuck cell# types + c! ;
: mk2 alloc tuck set-cdr tuck ! ;
: cons 1 mk2 ;
: mknum 5 mk2 ;
: num@ dup car swap cdr ;
: num? 5 typ? ;
: n@ dup num? 0= if s" not a number" lerror then num@ ;
: >num dup 0< mknum ;
: snoc 0 cons over if tuck swap set-cdr else nip nip dup then ;
: len 0 swap begin dup pair? while swap 1+ swap cdr repeat drop ;
types #cells 0 fill
sweep

\ ---- symbols
: sym-name car count ;
: name= sym-name rot over = if s= else 2drop drop 0 then ;
: mksym here >r dup c, here swap dup allot cmove r> unbound 2 mk2 dup symlist @ cons symlist ! ;
: intern symlist @ begin dup while >r 2dup r@ car name= if 2drop r> car exit then r> cdr repeat drop mksym ;
s" quote" intern constant 'quote
s" if" intern constant 'if
s" define" intern constant 'define
s" set!" intern constant 'set!
s" lambda" intern constant 'lambda
s" begin" intern constant 'begin
s" define-macro" intern constant 'define-macro

\ ---- environments: a list of frames, each frame an alist; globals live in the symbol
: assq begin dup while 2dup car car = if nip car exit then cdr repeat nip ;
: lookup begin dup while 2dup car assq ?dup if nip nip cdr exit then cdr repeat
  drop dup cdr dup unbound = if drop s" unbound variable" lerror then nip ;
: set-var begin dup while 2dup car assq ?dup if nip nip set-cdr exit then cdr repeat drop set-cdr ;
: define-var dup if >r swap cons r@ car cons r> ! else drop set-cdr then ;

\ ---- evaluator; eval-step returns ( x' env' 0 ) for a tail call or ( v -1 ) when done
variable 'eval
: eval* 'eval @ execute ;
: evlis 0 0 2swap begin over while over car over eval* >r 2swap r> snoc 2swap swap cdr swap repeat 2drop drop ;
: bind over 0= if 2drop 0 exit then
  over 2 typ? if cons 0 cons exit then
  over car over car cons >r cdr swap cdr swap recurse r> swap cons ;
: eval-body begin over cdr while over car over eval* drop swap cdr swap repeat swap car swap ;
: closure-env over car car swap bind over cdr cons swap car cdr swap ;
: apply-proc over 4 typ? if swap car execute exit then
  over 3 typ? if closure-env eval-body eval* exit then
  drop s" not a procedure" lerror ;
: expand car swap 0 cons apply-proc ;
: replace dup pair? 0= if 0 cons 'begin swap cons then
  2dup car swap ! cdr swap set-cdr ;
: macro-of dup 2 typ? if cdr dup 8 typ? if exit then then drop 0 ;
: do-if >r cdr dup car r@ eval* false = if cddr dup if car else drop unspec then else cadr then r> ;
: define-form dup car dup pair? if dup car >r cdr swap cdr cons 'lambda swap cons r> swap exit then swap cadr ;
: do-define >r cdr define-form r@ eval* over r> define-var ;
: do-set >r cdr dup cadr r@ eval* swap car r> set-var unspec ;
: do-defmacro >r cdr dup cadr r> eval* 0 8 mk2 swap car tuck set-cdr ;
: eval-step
  over heap? 0= if drop -1 exit then
  over 2 typ? if lookup -1 exit then
  over pair? 0= if drop -1 exit then
  over car
  dup 'quote = if 2drop cadr -1 exit then
  dup 'if = if drop do-if 0 exit then
  dup 'define = if drop do-define -1 exit then
  dup 'set! = if drop do-set -1 exit then
  dup 'lambda = if drop swap cdr swap 3 mk2 -1 exit then
  dup 'begin = if drop swap cdr dup 0= if 2drop unspec -1 exit then swap eval-body 0 exit then
  dup 'define-macro = if drop do-defmacro -1 exit then
  dup macro-of ?dup if nip >r over r> expand >r over r> replace 0 exit then
  drop over car over eval* >r swap cdr swap evlis r> swap
  over 3 typ? if closure-env eval-body 0 exit then
  apply-proc -1 ;
: eval begin eval-step until ;
' eval 'eval !

\ ---- printer
: ud. 10 mu/mod 2dup or if recurse else 2drop then 48 + emit ;
: .num dup 0< if 45 emit dnegate then ud. ;
: .string display? @ if car begin dup while dup car car emit cdr repeat drop exit then
  34 emit car begin dup while dup car car dup 34 = over 92 = or if 92 emit then emit cdr repeat drop 34 emit ;
: print
  dup nil = if drop ." ()" exit then
  dup false = if drop ." #f" exit then
  dup true = if drop ." #t" exit then
  dup unspec = if drop exit then
  dup heap? 0= if drop ." #<?>" exit then
  dup type@
  dup 5 = if drop num@ .num exit then
  dup 2 = if drop sym-name type exit then
  dup 6 = if drop .string exit then
  dup 7 = if drop 35 emit car recurse exit then
  dup 8 = if 2drop ." #<macro>" exit then
  dup 1 = if drop 40 emit dup car recurse cdr
    begin dup pair? while 32 emit dup car recurse cdr repeat
    dup if ."  . " recurse else drop then 41 emit exit then
  2drop ." #<procedure>" ;

\ ---- reader
: adv 1 >in +! ;
: skip-ws begin more? if in-char dup 59 = if drop #tib @ >in ! 0 else 33 < then else 0 then while adv repeat ;
: delim? dup 33 < over 40 = or over 41 = or over 34 = or swap 59 = or ;
: token tib >in @ + 0 begin more? if in-char delim? 0= else 0 then while 1+ adv repeat ;
: snumber over c@ 45 = dup >r if 1- swap 1+ swap then dup 0= if 2drop r> drop 0 exit then
  0 0 2swap begin dup while over c@ 48 - dup 10 u< 0= if 2drop 2drop drop r> drop 0 exit then
    >r 2swap 10 0 d* r> 0 d+ 2swap 1- swap 1+ swap repeat
  2drop r> if dnegate then mknum -1 ;
: read-atom token 2dup snumber if nip nip exit then intern ;
variable 'read
: read* 'read @ execute ;
: dot? in-char 46 = if >in @ 1+ #tib @ < if tib >in @ 1+ + c@ delim? else -1 then else 0 then ;
: read-tail 0 0 begin skip-ws more? 0= if 0 s" unbalanced (" lerror then in-char 41 <> while
    dot? if adv read* swap set-cdr skip-ws adv exit then
    read* snoc repeat adv drop ;
: read-hash adv in-char 40 = if adv read-tail 0 7 mk2 exit then
  token drop c@ 116 = if true else false then ;
: read-string adv 0 0 begin more? if in-char 34 <> else 0 then while
    in-char 92 = if adv in-char 110 = if 10 else in-char then else in-char then adv >num snoc repeat
  adv drop 0 6 mk2 ;
: read skip-ws in-char
  dup 40 = if drop adv read-tail exit then
  dup 39 = if drop adv read* 0 cons 'quote swap cons exit then
  dup 34 = if drop read-string exit then
  dup 35 = if drop read-hash exit then
  dup 41 = if drop adv 0 s" unexpected )" lerror then
  drop read-atom ;
' read 'read !

\ ---- primitives: each takes the evaluated argument list and returns a value
: prim: parse-name intern >r 4 alloc tuck ! r> set-cdr ;
: pcar car dup pair? 0= if s" car: not a pair" lerror then car ;
: pcdr car dup pair? 0= if s" cdr: not a pair" lerror then cdr ;
: pcons dup car swap cadr cons ;
: pset-car dup cadr swap car ! unspec ;
: pset-cdr dup cadr swap car set-cdr unspec ;
: eqv 2dup = if 2drop -1 exit then over num? over num? and if num@ rot num@ d= exit then 2drop 0 ;
: equal 2dup eqv if 2drop -1 exit then
  over pair? over pair? and if over car over car recurse if cdr swap cdr recurse exit then 2drop 0 exit then
  over heap? over heap? and if over type@ over type@ = if over type@ dup 6 = swap 7 = or if car swap car recurse exit then then then
  2drop 0 ;
: peq dup car swap cadr = bool ;
: peqv dup car swap cadr eqv bool ;
: pequal dup car swap cadr equal bool ;
: pnot car false = bool ;
: pnull car 0= bool ;
: ppair car pair? bool ;
: psymbol car 2 typ? bool ;
: pproc car dup 3 typ? swap 4 typ? or bool ;
: pnumber car num? bool ;
: pstring car 6 typ? bool ;
: pvector? car 7 typ? bool ;
: pboolean car dup true = swap false = or bool ;
: p+ 0 0 rot begin dup while >r r@ car n@ d+ r> cdr repeat drop mknum ;
: p* 1 0 rot begin dup while >r r@ car n@ d* r> cdr repeat drop mknum ;
: p- dup cdr 0= if car n@ dnegate mknum exit then
  dup car n@ rot cdr begin dup while >r r@ car n@ d- r> cdr repeat drop mknum ;
: chain >r begin dup cdr while dup car n@ 2 pick cadr n@ r@ execute 0= if drop r> drop false exit then cdr repeat
  drop r> drop true ;
: p= ['] d= chain ;
: p< ['] d< chain ;
: p> ['] d> chain ;
: p<= ['] d<= chain ;
: p>= ['] d>= chain ;
: divargs dup car n@ rot cadr n@ over 0< <> if 2drop 0 s" divisor out of 16-bit range" lerror then
  dup 0= if 0 s" division by zero" lerror then ;
: sdiv over >r dup >r abs >r dabs r> mu/mod r> r@ xor 0< if dnegate then rot r> 0< if negate then -rot ;
: pquotient divargs sdiv mknum nip ;
: premainder divargs sdiv 2drop >num ;
: pmodulo divargs dup >r sdiv 2drop dup if dup 0< r@ 0< <> if r@ + then then r> drop >num ;
: mklist 0 rot begin dup while >r over swap cons r> 1- repeat drop nip ;
: pmake-vector dup car n@ drop swap cdr dup if car else drop unspec then mklist 0 7 mk2 ;
: pvector 0 7 mk2 ;
: nth begin dup while swap cdr swap 1- repeat drop ;
: pvector-ref dup car car swap cadr n@ drop nth car ;
: pvector-set dup caddr swap dup car car swap cadr n@ drop nth ! unspec ;
: pvector-length car car len >num ;
: plist->vector car 0 7 mk2 ;
: pstring-length car car len >num ;
: pdisplay car -1 display? ! print 0 display? ! unspec ;
: pwrite car print unspec ;
: pnewline drop cr unspec ;
: spread dup cdr 0= if car exit then dup car swap cdr recurse cons ;
: papply dup car swap cdr spread apply-proc ;
: pforth drop -200 throw ;
: proom drop 0 free @ begin dup while swap 1+ swap cdr repeat drop >num ;
' pcar prim: car
' pcdr prim: cdr
' pcons prim: cons
' pset-car prim: set-car!
' pset-cdr prim: set-cdr!
' peq prim: eq?
' peqv prim: eqv?
' pequal prim: equal?
' pnot prim: not
' pnull prim: null?
' ppair prim: pair?
' psymbol prim: symbol?
' pproc prim: procedure?
' pnumber prim: number?
' pnumber prim: integer?
' pstring prim: string?
' pvector? prim: vector?
' pboolean prim: boolean?
' p+ prim: +
' p* prim: *
' p- prim: -
' p= prim: =
' p< prim: <
' p> prim: >
' p<= prim: <=
' p>= prim: >=
' pquotient prim: quotient
' premainder prim: remainder
' pmodulo prim: modulo
' pmake-vector prim: make-vector
' pvector prim: vector
' pvector-ref prim: vector-ref
' pvector-set prim: vector-set!
' pvector-length prim: vector-length
: pinner car car ;
' pinner prim: vector->list
' plist->vector prim: list->vector
' pinner prim: string->list
' pstring-length prim: string-length
' pdisplay prim: display
' pwrite prim: write
' pnewline prim: newline
' papply prim: apply
' pforth prim: forth
' proom prim: room
: plist ;
' plist prim: list

\ ---- REPL: one or more expressions per line; (forth) returns to the Forth prompt
: .lerror dup -100 = if drop ." error: " errmsg @ errlen @ type space errobj @ print else ." error " . then ;
: eval-line begin skip-ws more? while read cr nil eval 0 display? ! print repeat ;
: lisp begin cr ." > " refill ['] eval-line catch ?dup if dup -200 = if drop exit then cr .lerror then again ;
