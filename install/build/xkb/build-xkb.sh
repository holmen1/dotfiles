#!/bin/sh
# build-xkb.sh — compile XKB keymap for X11 keyboard customisation
#
# Produces: dotfiles/xkb/.config/xkb/keymap.xkb
#
# Re-run this whenever dotfiles/xkb/.config/xkb/symbols/local changes.
#
# Deps: setxkbmap, xkbcomp

set -e

DOTFILES="${HOME}/repos/dotfiles"
SYMBOLS_DIR="${DOTFILES}/install/build/xkb"
KEYMAP="${DOTFILES}/dotfiles/xkb/.config/xkb/keymap.xkb"

mkdir -p "$(dirname "$KEYMAP")"

setxkbmap se -option ctrl:nocaps,lv3:lwin_switch -print \
    | sed '/xkb_symbols/s|include "\(.*\)"|include "\1+local(numpad)+local(remaps)"|' \
    | xkbcomp -w0 -I"${SYMBOLS_DIR}" - -o "${KEYMAP}"

echo "Keymap written to ${KEYMAP}"
