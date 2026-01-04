#!/bin/sh
# FreeBSD

[ $# -eq 0 ] && { echo "Usage: $0 <output-directory>"; exit 1; }

# Export manually installed packages
pkg query -e '%a = 0' '%R %o' | awk '$1 == "FreeBSD-ports" {print $2}' > "$1/pkglist.txt"
echo "Exported list of explicitly installed packages to $1/pkglist.txt"

# Export packages built from ports
pkg query -e '%a = 0' '%R %o' | awk '$1 != "FreeBSD-ports" && $1 != "FreeBSD-base"  {print $2}' > "$1/foreignpkglist.txt"
echo "Exported list of ports to $1/foreignpkglist.txt"
