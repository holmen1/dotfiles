#!/bin/sh
# Minimal XKB toggle script

KEYMAP="$HOME/.cache/custom-keymap.xkb"
STATE="$HOME/.cache/xkb-layout"

[ -f "$STATE" ] || { echo "Warning: $STATE not found; start X before toggling layouts." >&2; exit 1; }

if [ "$(cat "$STATE")" = "custom" ]; then
    pkill -x xcape 2>/dev/null || true
    if [ "$(hostname -s)" = "besk" ]; then
        setxkbmap us || { echo >&2 "Failed to restore XKB layout: us"; exit 1; }
        echo us > "$STATE"
    else
        setxkbmap se || { echo >&2 "Failed to restore XKB layout: se"; exit 1; }
        echo se > "$STATE"
    fi
else
    if ! xkbcomp -w0 "$KEYMAP" "$DISPLAY"; then
        echo >&2 "Failed to load custom XKB map: $KEYMAP"
        exit 1
    fi
    pkill -x xcape 2>/dev/null || true
    xcape -e 'Control_L=Escape' &
    echo custom > "$STATE"
fi
