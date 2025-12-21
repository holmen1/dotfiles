#!/bin/sh
# FreeBSD
# Install packages from pkglist

[ $# -eq 0 ] && { echo "Usage: $0 <pkglist>"; exit 1; }

[ -f "$1" ] || { echo "Error: $1 not found"; exit 1; }

xargs sudo pkg install -y < "$1"
