#!/bin/sh
# macOS — install all packages and apps from Brewfile

BREWFILE="$HOME/repos/dotfiles/install/macinstall/Brewfile"

brew bundle --file="$BREWFILE"
