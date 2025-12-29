#!/bin/sh
# FreeBSD
# Export manually installed packages

[ $# -eq 0 ] && { echo "Usage: $0 <output>"; exit 1; }

pkg query -e '%a = 0' '%R %o' | awk '$1 == "FreeBSD-ports" {print $2}' > "$1"
echo "Exported to $1"
