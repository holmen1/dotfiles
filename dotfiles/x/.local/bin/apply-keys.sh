#!/bin/sh
# apply-keys.sh — X11 keyboard customisation (XKB + xcape)
#
#   CapsLock : hold = Control, tap = Escape
#   Super    : hold = num-layer
#     u i o  →  7 8 9
#     j k l  →  4 5 6
#     m , .  →  1 2 3
#       n    →  0
#
# Deps: setxkbmap, xkbcomp, xcape

# Apply layout: Swedish + ctrl:nocaps + lv3:lwin_switch (Super = level-3)
# Merge user symbol file local(numpad) for the digit layer.
setxkbmap se -option ctrl:nocaps,lv3:lwin_switch -print \
    | sed '/xkb_symbols/s|include "\(.*\)"|include "\1+local(numpad)"|' \
    | xkbcomp -w0 -I"$HOME/.config/xkb" - "$DISPLAY"

# xcape: synthesise Escape when CapsLock (Control_L) is tapped alone
pkill -x xcape 2>/dev/null || true
xcape -e 'Control_L=Escape' &
