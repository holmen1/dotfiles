#!/bin/sh

README="$HOME/repos/dotfiles/dotfiles/lf/README.md"

# Show lf keybindings in dmenu
sed -n 14,40p "$README" | dmenu -l 23 -i -p "lf Help" \
-nb "#222222" -nf "#ffffff" -sb "#222222" -sf "#ffffff" \
-fn "JetBrainsMono Nerd Font Mono-14"

