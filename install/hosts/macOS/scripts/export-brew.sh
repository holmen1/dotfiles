#!/bin/sh
# macOS — export installed Homebrew packages to Brewfile

BREWFILE="$HOME/repos/dotfiles/install/hosts/macOS/Brewfile"

brew bundle dump --file="$BREWFILE" --force
echo "Exported to $BREWFILE"
