#!/bin/sh
# Minimal XKB toggle script

KEYMAP="$HOME/.cache/custom-keymap.xkb"
STATE="$HOME/.cache/xkb-layout"

[ -f "$STATE" ] || { echo "Warning: $STATE not found, staying in 'se' layout." >&2; exit 1; }

if [ "$(cat "$STATE")" = "custom" ]; then
    pkill -x xcape 2>/dev/null || true
    setxkbmap se
    echo se > "$STATE"
else
    xkbcomp -w0 "$KEYMAP" "$DISPLAY"
    pkill -x xcape 2>/dev/null || true
    xcape -e 'Control_L=Escape' &
    echo custom > "$STATE"
fi

