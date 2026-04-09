#!/bin/sh
# dmenu-menu: Unified menu for system

DOTFILES=$HOME/repos/dotfiles
DSCRIPT=$HOME/.dmenu
XKB_STATE="$HOME/.cache/xkb-layout"
XKB_README="$DOTFILES/dotfiles/xkb/README.md"
XMONAD_README="$DOTFILES/dotfiles/xmonad/README.md"
LF_README="$DOTFILES/dotfiles/lf/README.md"
BASH_README="$DOTFILES/dotfiles/bash/README.md"
MONITOR_SCRIPTS=$DOTFILES/scripts


# Font detection
if fc-list | grep -qi "JetBrainsMono Nerd Font"; then
    FONT="JetBrainsMono Nerd Font Mono-14"
else
    FONT="monospace-14"
fi

current_xkb=$(cat "$XKB_STATE" 2>/dev/null || echo "se")
battery_level=$($MONITOR_SCRIPTS/monitor-battery.sh --get-level)
ssid=$($DSCRIPT/dmenu-wifi.sh --get-ssid)
vpn=$($DSCRIPT/dmenu-mullvad.sh --get-location)

# Main categories
category=$(printf "Help\nNetwork\nXKB" | dmenu -i -p "x[$current_xkb] w[$ssid] v[$vpn] b[$battery_level%]" \
-nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" \
-fn "$FONT")

case "$category" in
  "Help")
    app=$(printf "XKB\nlf\nbash\nXmonad" | dmenu -i -p "App:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT")
    case "$app" in
      "XKB")
        sed -n 9,34p "$XKB_README" | dmenu -l 26 -p "XKB Help" \
		-nb "#222222" -nf "#ffffff" -sb "#222222" -sf "#ffffff" -fn "$FONT" ;;
      "bash")
        sed -n 16,26p "$BASH_README" | dmenu -l 11 -p "bash Help" \
		-nb "#222222" -nf "#ffffff" -sb "#222222" -sf "#ffffff" -fn "$FONT" ;;
      "Xmonad")
        sed -n 12,40p "$XMONAD_README" | dmenu -l 24 -i -p "XMonad Help" \
		-nb "#222222" -nf "#ffffff" -sb "#222222" -sf "#ffffff" -fn "$FONT" ;;
      "lf")
        sed -n 14,40p "$LF_README" | dmenu -l 23 -i -p "lf Help" \
                -nb "#222222" -nf "#ffffff" -sb "#222222" -sf "#ffffff" -fn "$FONT" ;;
    esac ;;
  "Network")
    net=$(printf "WiFi\nVPN" | dmenu -i -p "Net:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT")
    case "$net" in
      "WiFi")
        $DSCRIPT/dmenu-wifi.sh ;;
      "VPN")
        $DSCRIPT/dmenu-mullvad.sh ;;
    esac ;;
  "XKB")
    $DSCRIPT/dmenu-xkb.sh ;;
esac
