#!/bin/sh
# macOS — export installed Homebrew packages to Brewfile

BREWFILE="$HOME/repos/dotfiles/install/profiles/macinstall/Brewfile"

brew bundle dump --file="$BREWFILE" --force
echo "Exported to $BREWFILE"
