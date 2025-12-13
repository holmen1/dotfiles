#!/bin/sh

MANAGER=yay

# Check for yay and set the package manager accordingly
if ! command -v yay > /dev/null 2>&1; then
  echo "Warning: $MANAGER not found. Defaulting to pacman. AUR packages will not be installed."
  MANAGER="sudo pacman"
fi

# Function to install packages
install_packages() {
  local pkglist=$1
  while IFS= read -r package; do
    if ! pacman -Q "$package" > /dev/null 2>&1; then
      echo "Installing $package..."
      $MANAGER -S --noconfirm "$package"
    else
      echo "$package is already installed"
    fi
  done < "$pkglist"
}

# Check if the package list file is provided
if [ -z "$1" ]; then
  echo "Usage: $0 <package-list-file>"
  exit 1
fi

# Install packages from the provided list
install_packages "$1"
