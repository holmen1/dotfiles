#!/bin/sh
# Export a list of all manually installed (explicit) packages for Debian/Ubuntu
# Usage: ./export-apt.sh <output-dir>

if [ $# -eq 0 ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

OUTDIR="$1"
mkdir -p "$OUTDIR"

BASELIST="$(dirname "$0")/../install/debianinstall/packages/install/pkglist.txt"

if [ -f "$BASELIST" ]; then
    # Export only manually installed packages not in base list
    apt-mark showmanual | sort | grep -vxFf "$BASELIST" > "$OUTDIR/pkglist.txt"
    echo "Exported user-installed package list (excluding base) to $OUTDIR/pkglist.txt"
else
    # Export all manually installed packages
    apt-mark showmanual | sort > "$OUTDIR/pkglist.txt"
    echo "Exported manually installed package list to $OUTDIR/pkglist.txt (no base filter found)"
fi

# Optionally, export a list of all packages (including dependencies)
# dpkg --get-selections | awk '{print $1}' | sort > "$OUTDIR/allpackages.txt"
