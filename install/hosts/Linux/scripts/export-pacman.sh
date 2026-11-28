#!/bin/sh
# Artix Linux

set -e

[ "$#" -eq 1 ] || { echo "Usage: $0 <output-directory>" >&2; exit 1; }
OUTPUT_DIR=$1

query_pacman() {
    if pacman "$1" > "$2" 2> "$OUTPUT_DIR/pacman.err"; then
        cat "$OUTPUT_DIR/pacman.err" >&2
    else
        status=$?
        if [ "$status" -eq 1 ] && [ ! -s "$2" ] && [ ! -s "$OUTPUT_DIR/pacman.err" ]; then
            return 0
        fi
        cat "$OUTPUT_DIR/pacman.err" >&2
        echo "Error: pacman $1 failed" >&2
        return 1
    fi
}

query_pacman -Qqen "$OUTPUT_DIR/pkglist.txt"
query_pacman -Qqem "$OUTPUT_DIR/foreignpkglist.txt"
