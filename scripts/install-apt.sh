#!/bin/sh
# Debian/Ubuntu
# Install packages from pkglist

[ $# -eq 0 ] && { echo "Usage: $0 <pkglist>"; exit 1; }

[ -f "$1" ] || { echo "Error: $1 not found"; exit 1; }

while IFS= read -r package; do
    if ! dpkg -s "$package" >/dev/null 2>&1; then
        echo "Installing $package..."
        sudo apt-get install -y "$package"
    else
        echo "$package is already installed, skipping."
    fi
done < "$1"
