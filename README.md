# forthlisp-z80

The same four layers as [forthlisp-stm32](https://github.com/equwal/forthlisp-stm32), for a
Z80 with a 64 KiB address space. A Z80 assembler in SBCL builds a Forth kernel written
from scratch, and a Z80-sized Scheme loads through that Forth. No third-party Forth.

```
 4  LISP     lisp.fs + prelude.scm: a Z80-sized R7RS-small subset, loaded through the console
 3  FORTH    kernel.lisp: an indirect-threaded Forth in Z80 assembly
 2  SBCL     asm.lisp: a two-pass Z80 assembler in SBCL (test-asm.lisp checks it
             against z80pack's z80asm)
 1  ASM      Z80 instructions
```

## Quickstart

```sh
git clone https://github.com/equwal/forthlisp        # the core, next to this repo
git clone https://github.com/equwal/forthlisp-z80
cd forthlisp-z80
./build-emu.sh             # fetch and build z80pack cpmsim + z80asm (MIT) in ~/src/z80pack
./z80-forth                # the kernel's own prompt
./z80-lisp                 # Scheme:  (+ 1 2)  ->  3
./test.sh                  # assembler, kernel words, conformance
```

Needs SBCL, a C compiler, git and `nc`. Override the tools with `SBCL`, `Z80PACK`,
`CPMSIM`, `Z80ASM`, `NC`, `STTY`. `PORT` picks the emulator's TCP console port.

## Emulator

z80pack's `cpmsim` (MIT licence, by Udo Munk) runs headless with its console on a TCP
port. `build-emu.sh` clones it and makes three small build edits, explained in `NOTES.md`.
The emulator is fetched, not vendored.

## Tests

- assembler encodings vs `z80asm`;
- kernel words over the console;
- the core's `r7rs-tests.scm`.

`NOTES.md` gives the counts and every limit: heap size, integer range, line length,
stack depth.

## Influences

- Paul Khuong, "SBCL: the ultimate assembly code breadboard"
  (https://pvk.ca/Blog/2014/03/15/sbcl-the-ultimate-assembly-code-breadboard/)
- Ron Garret, gll-mag-patch (https://github.com/rongarret/gll-mag-patch)
- Ron Garret, "Lisping at JPL" (https://flownet.com/gat/jpl-lisp.html)

## Licence

MIT (see `LICENSE`). z80pack is not part of this repo; `build-emu.sh` downloads it under
its own MIT licence.

## CI

`ci-local.sh` (in the forthlisp core repo) runs every test layer of all three repos in a
clean `ubuntu:24.04` container. It runs after each publish, without a hosted CI service.
The last result is on https://dickt.store/forthlisp/.
