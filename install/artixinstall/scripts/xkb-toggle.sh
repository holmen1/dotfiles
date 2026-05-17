#!/bin/sh
# Minimal XKB toggle script

case "$(uname -s)" in
    FreeBSD*) KEYMAP="$HOME/.config/xkb/keymap-bsd.xkb" ;;
    *)        KEYMAP="$HOME/.config/xkb/keymap-linux.xkb" ;;
esac
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

