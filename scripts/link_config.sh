#!/bin/sh

if [ -z "$1" ]; then
  echo "Usage: $0 <config-file>"
  exit 1
fi

CONFIG_FILE="$1"
BASE_DIR="~/repos/dotfiles"

if [ -f "$CONFIG_FILE" ]; then
  . "$CONFIG_FILE"
else
  echo "Error: Config file '$CONFIG_FILE' not found!"
  exit 1
fi

if [ -z "$packages" ]; then
  echo "Error: No packages defined in config file!"
  exit 1
fi

if ! command -v stow >/dev/null 2>&1; then
  echo "Error: stow is not installed."
  exit 1
fi

DOTFILES_DIR_EXPANDED=$(eval echo "$DOTFILES_DIR")

echo "Using config file: $CONFIG_FILE"
echo ""

for package in $packages; do
    output=$(stow -v -d "$DOTFILES_DIR_EXPANDED" -t "$HOME" "$package" 2>&1)
    if echo "$output" | grep -q "LINK"; then
        echo "$package: Stowed"
    else
        echo "$package: Already stowed"
    fi
done

echo "Done."


