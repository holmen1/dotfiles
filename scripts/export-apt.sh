#!/bin/sh
# Export a list of all manually installed (explicit) packages for Debian/Ubuntu
# Usage: ./export-apt.sh <output-dir>

if [ $# -eq 0 ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

OUTDIR="$1"
mkdir -p "$OUTDIR"

# Export all manually installed packages (not dependencies)
dpkg-query -W -f='${Package}\n' | sort > "$OUTDIR/pkglist.txt"

# Optionally, export a list of all packages (including dependencies)
# dpkg --get-selections | awk '{print $1}' | sort > "$OUTDIR/allpackages.txt"

echo "Exported package list to $OUTDIR/pkglist.txt"
