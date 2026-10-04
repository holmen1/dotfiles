#!/bin/sh
# build-xkb.sh — compile XKB keymap for X11 keyboard customisation
#
# Deps: setxkbmap, xkbcomp

set -e

[ $# -eq 0 ] && { echo "Usage: $0 <layout>"; exit 1; }

DOTFILES="${HOME}/repos/dotfiles"
SYMBOLS_DIR="${DOTFILES}/install/build/xkb"
KEYMAP="$HOME/.cache/custom-keymap.xkb"

LAYOUT="${1:-se}"

setxkbmap "$LAYOUT" -option ctrl:nocaps,lv3:lwin_switch -print \
    | sed '/xkb_symbols/s|include "\(.*\)"|include "\1+local('"${LAYOUT}"')"|' \
    | xkbcomp -w0 -I"${SYMBOLS_DIR}" - -o "${KEYMAP}"

echo "Keymap written to ${KEYMAP}"
