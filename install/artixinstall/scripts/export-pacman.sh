#!/bin/sh
# Artix Linux

[ $# -eq 0 ] && { echo "Usage: $0 <output-directory>"; exit 1; }

OUTPUT_DIR="$1"

pacman -Qqen > "$OUTPUT_DIR/pkglist.txt"
echo "Exported list of explicitly installed packages to $OUTPUT_DIR/pkglist.txt"

pacman -Qqem > "$OUTPUT_DIR/foreignpkglist.txt"
echo "Exported list of foreign packages to $OUTPUT_DIR/foreignpkglist.txt"
