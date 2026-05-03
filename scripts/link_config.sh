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
    # Dry run to find intended link targets
    dry=$(stow -n -v -R -d "$DOTFILES_DIR_EXPANDED" -t "$HOME" "$package" 2>&1)
    # Back up any real files that would conflict
    echo "$dry" | awk '/^LINK:/{print $2}' | while read -r target; do
        full="$HOME/$target"
        if [ -f "$full" ] && [ ! -L "$full" ]; then
            mv "$full" "${full}.bak"
            echo "$package: backed up $target"
        fi
    done
    # Restow
    output=$(stow -R -v -d "$DOTFILES_DIR_EXPANDED" -t "$HOME" "$package" 2>&1)
    if echo "$output" | grep -q "LINK"; then
        echo "$package: Stowed"
    else
        echo "$package: Restowed (no changes)"
    fi
done

echo "Done."


