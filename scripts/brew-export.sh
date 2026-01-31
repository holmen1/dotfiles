#!/bin/sh

# Export Homebrew packages to a Brewfile
brew bundle dump --file=~/repos/dotfiles/install/macinstall/Brewfile --force
