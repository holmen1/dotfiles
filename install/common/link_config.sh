#!/bin/sh

if [ -z "$1" ]; then
  echo "Usage: $0 <config-file>"
  exit 1
fi

CONFIG_FILE="$1"
STOW_DIR="$HOME/repos/dotfiles/config"
TARGET="$HOME"

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

echo "Using config file: $CONFIG_FILE"
echo ""

for package in $packages; do
    # Dry run to find intended link targets, --adopt needed here to prevent escape
    dry=$(stow -n -v -R --adopt --ignore='.*\.md' -d "$STOW_DIR" -t "$TARGET" "$package" 2>&1)
    # Back up any real files that would conflict
    echo "$dry" | awk '/^LINK:/{print $2}' | while read -r target; do
        full="$HOME/$target"
        if [ -f "$full" ] && [ ! -L "$full" ]; then
            mv "$full" "${full}.bak"
            echo "$package: backed up $target"
        fi
    done
    # Restow
    stow -R -v --ignore='.*\.md' -d "$STOW_DIR" -t "$TARGET" "$package" 2>&1
    #echo "$package: Stowed"
done

echo "Done."

