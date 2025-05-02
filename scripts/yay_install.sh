#!/bin/bash

# Function to install packages
install_packages() {
  local pkglist=$1
  while IFS= read -r package; do
    if ! pacman -Qi "$package" &> /dev/null; then
      yay -S --noconfirm "$package"
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
