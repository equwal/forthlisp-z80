#!/bin/sh
# emu.sh IMAGE -- run the Z80 image in z80pack cpmsim, console on TCP 127.0.0.1:$PORT.
# Prints the emulator PID. The kernel waits for a connection before it prints its banner.
set -e
CPMSIM=${CPMSIM:-${Z80PACK:-$HOME/src/z80pack}/cpmsim/cpmsim}
PORT=${PORT:-4455}
IMG=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
RUN=/tmp/z80emu-$PORT   # cpmsim reads ./conf/net_server.conf
mkdir -p "$RUN/conf"
printf '1 0 %s\n' "$PORT" > "$RUN/conf/net_server.conf"   # console 1, no telnet, TCP port
cd "$RUN"
# -x loads the image (Mostek header: load at 0000).
"$CPMSIM" -x "$IMG" </dev/null >"$RUN/log" 2>&1 &
echo $!
