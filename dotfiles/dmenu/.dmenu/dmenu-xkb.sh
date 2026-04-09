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


choice=$(printf "custom\nse" | dmenu -i -p "xkb:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT")

case "$choice" in
    custom)
        xkbcomp -w0 "$KEYMAP" "$DISPLAY"
        pkill -x xcape 2>/dev/null || true
        xcape -e 'Control_L=Escape' &
	    echo "$choice" > "$STATE"
        ;;
    se)
        pkill -x xcape 2>/dev/null || true
        setxkbmap se
	    echo "$choice" > "$STATE"
        ;;
esac

