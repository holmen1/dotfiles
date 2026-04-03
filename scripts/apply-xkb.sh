#!/bin/sh
# apply-xkb.sh — apply compiled XKB keymap and restart xcape
#
# CapsLock: hold = Control, tap = Escape
# Super:    hold = num-layer (u/i/o=7/8/9, j/k/l=4/5/6, m/,/.=1/2/3, n=0)
# Remaps:   å=  ö: ä/ ,< .>
#
# Build the keymap first with: install/build/xkb/build-xkb.sh

case "$(uname -s)" in
    FreeBSD*) KEYMAP="$HOME/.config/xkb/keymap-bsd.xkb" ;;
    *)        KEYMAP="$HOME/.config/xkb/keymap-linux.xkb" ;;
esac

if [ -f "$KEYMAP" ]; then
    xkbcomp -w0 "$KEYMAP" "$DISPLAY"
    pkill -x xcape 2>/dev/null || true
    xcape -e 'Control_L=Escape' &
    echo "custom" > "$HOME/.cache/xkb-layout"
else
    echo "Warning: $KEYMAP not found. Run install/build/xkb/build-xkb.sh first." >&2
fi
