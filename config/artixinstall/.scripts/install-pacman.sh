#!/bin/sh
# Artix Linux — install packages from a pkglist or standard input.

set -e

if [ "$#" -gt 0 ]; then
    [ -f "$1" ] || { echo "Error: $1 not found" >&2; exit 1; }
    exec < "$1"
fi

set -f
while IFS= read -r line || [ -n "$line" ]; do
    for package in $line; do
        if ! pacman -Q "$package" >/dev/null 2>&1; then
            echo "Installing $package..."
            sudo pacman -S --noconfirm "$package"
        fi
    done
done
sudo -k
