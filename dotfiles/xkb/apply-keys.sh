#!/bin/sh
# apply-keys.sh — load compiled XKB keymap + start xcape
#
#   CapsLock : hold = Control, tap = Escape
#   Super    : hold = num-layer
#     u i o  →  7 8 9
#     j k l  →  4 5 6
#     m , .  →  1 2 3
#       n    →  0
#
# Deps: xkbcomp, xcape
# To rebuild keymap.xkb: install/build/xkb/build-xkb.sh

KEYMAP="$HOME/.config/xkb/keymap.xkb"

xkbcomp -w0 "$KEYMAP" "$DISPLAY"
pkill -x xcape 2>/dev/null || true
xcape -e 'Control_L=Escape' &
