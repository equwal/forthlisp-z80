#!/bin/sh
# test.sh -- run the three test layers: assembler, kernel words, Lisp conformance.
# Each kernel-level layer gets its own fresh emulator. PORT sets the first TCP port.
SRC=$(cd "$(dirname "$0")" && pwd)
SBCL=${SBCL:-sbcl}
PORT=${PORT:-4470}
rc=0
nice $SBCL --script "$SRC/test-asm.lisp" || rc=1
nice $SBCL --script "$SRC/kernel.lisp" "$SRC/kernel.mos"
for t in test-forth test; do
  P=$(PORT=$PORT "$SRC/emu.sh" "$SRC/kernel.mos")
  if [ $t = test ]; then arg=$SRC/r7rs-tests-z80.scm; else arg=; fi
  PORT=$PORT $SBCL --script "$SRC/host.lisp" $t $arg || rc=1
  kill $P 2>/dev/null
  PORT=$((PORT + 1))
done
exit $rc
