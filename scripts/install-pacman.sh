#!/bin/sh
# Arch Linux
# Install packages from pkglist

[ $# -eq 0 ] && { echo "Usage: $0 <pkglist>"; exit 1; }

[ -f "$1" ] || { echo "Error: $1 not found"; exit 1; }

MANAGER=yay

# Check for yay and set the package manager accordingly
if ! command -v yay > /dev/null 2>&1; then
  echo "Warning: $MANAGER not found. Defaulting to pacman. AUR packages will not be installed."
  MANAGER="sudo pacman"
fi

while IFS= read -r package; do
    if ! pacman -Q "$package" "$package" >/dev/null 2>&1; then
        echo "Installing $package..."
        $MANAGER -S --noconfirm "$package"
    else
        echo "$package is already installed, skipping."
    fi
done < "$1"

