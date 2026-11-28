#!/bin/sh
# macOS — install all packages and apps from Brewfile

BREWFILE="$HOME/repos/dotfiles/install/hosts/macOS/Brewfile"

brew bundle --file="$BREWFILE"
