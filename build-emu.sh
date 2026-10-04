#!/bin/sh
# build-emu.sh -- fetch and build the z80pack cpmsim emulator (MIT licence) and its
# z80asm reference assembler in ~/src/z80pack. Named pipes for the auxiliary device
# are switched off: they need a cpmrecv helper and are shared between instances.
set -e
Z=${Z80PACK:-$HOME/src/z80pack}
[ -d "$Z" ] || git clone --depth 1 https://github.com/udo-munk/z80pack.git "$Z"
sed -i 's|^#define PIPES|/* #define PIPES */|' "$Z/cpmsim/srcsim/sim.h"
# upstream's no-PIPES path still names the old constant STOPPED
sed -i 's/= STOPPED;/= ST_STOPPED;/' "$Z/cpmsim/srcsim/simio.c"
# a TCP console peer that disconnects (read returns 0) stops the CPU with a fatal I/O
# error upstream; drop the connection instead, so the next one can attach
sed -i 's/if ((errno == EAGAIN) || (errno == EINTR)) {/if (1) { \/* peer gone: drop it *\//' "$Z/cpmsim/srcsim/simio.c"
(cd "$Z/cpmsim/srcsim" && nice make clean >/dev/null && nice make >/dev/null)
(cd "$Z/z80asm" && nice make >/dev/null)
echo "built $Z/cpmsim/cpmsim and $Z/z80asm/z80asm ($(git -C "$Z" rev-parse --short HEAD))"
