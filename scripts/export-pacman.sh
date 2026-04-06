#!/bin/sh
# Arch Linux

# Check if output directory is provided
[ $# -eq 0 ] && { echo "Usage: $0 <output-directory>"; exit 1; }

OUTPUT_DIR="$1"

# Export the list of explicitly installed packages
pacman -Qqen > "$OUTPUT_DIR/pkglist.txt"
echo "Exported list of explicitly installed packages to $OUTPUT_DIR/pkglist.txt"

# Export the list of foreign packages (e.g., from AUR)
pacman -Qqem > "$OUTPUT_DIR/foreignpkglist.txt"
echo "Exported list of foreign packages to $OUTPUT_DIR/foreignpkglist.txt"
