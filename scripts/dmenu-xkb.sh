#!/bin/sh
case "$(uname -s)" in
    FreeBSD*) KEYMAP="$HOME/.config/xkb/keymap-bsd.xkb" ;;
    *)        KEYMAP="$HOME/.config/xkb/keymap-linux.xkb" ;;
esac
STATE="$HOME/.cache/xkb-layout"

if fc-list | grep -qi "JetBrainsMono Nerd Font"; then
    FONT="JetBrainsMono Nerd Font Mono-14"
else
    FONT="monospace-14"
fi

current=$(cat "$STATE" 2>/dev/null || echo "se")

choice=$(printf "custom\nse" | dmenu -p "xkb[$current]" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" \
    -fn "$FONT")

[ -z "$choice" ] && exit 0

case "$choice" in
    custom)
        xkbcomp -w0 "$KEYMAP" "$DISPLAY"
        pkill -x xcape 2>/dev/null || true
        xcape -e 'Control_L=Escape' &
        ;;
    se)
        pkill -x xcape 2>/dev/null || true
        setxkbmap se
        ;;
esac

echo "$choice" > "$STATE"
