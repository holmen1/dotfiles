#!/bin/sh
case "$(uname -s)" in
    FreeBSD*) KEYMAP="$HOME/.config/xkb/keymap-bsd.xkb" ;;
    *)        KEYMAP="$HOME/.config/xkb/keymap-linux.xkb" ;;
esac
README="$HOME/repos/dotfiles/dotfiles/xkb/README.md"

if fc-list | grep -qi "JetBrainsMono Nerd Font"; then
    FONT="JetBrainsMono Nerd Font Mono-14"
else
    FONT="monospace-14"
fi


choice=$(printf "help\ncustom\nse" | dmenu -p "xkb" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" \
    -fn "$FONT")

[ -z "$choice" ] && exit 0

case "$choice" in
    help)
        sed -n 9,34p "$README" | dmenu -l 26 -i -p "XKB Help" \
-nb "#222222" -nf "#ffffff" -sb "#222222" -sf "#ffffff" \
-fn "$FONT"
        ;;
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

