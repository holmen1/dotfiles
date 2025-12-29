#!/bin/sh
# FreeBSD
# Export packages built from ports

[ $# -eq 0 ] && { echo "Usage: $0 <output>"; exit 1; }

pkg query -e '%a = 0' '%R %o' | awk '$1 != "FreeBSD-ports" && $1 != "FreeBSD-base"  {print $2}' > "$1"
echo "Exported to $1"
