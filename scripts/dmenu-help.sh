#!/bin/sh

README="$HOME/repos/dotfiles/dotfiles/xmonad/README.md"

# Show xmonad keybindings in dmenu
sed -n 12,40p "$README" | dmenu -l 27 -i -p "XMonad Help" \
-nb "#222222" -nf "#ffffff" -sb "#222222" -sf "#ffffff" \
-fn "JetBrainsMono Nerd Font Mono-14"

