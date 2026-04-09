#!/bin/sh
# dmenu-menu: Unified menu for system

DSCRIPT=$HOME/.dmenu
XKB_STATE="$HOME/.cache/xkb-layout"
MONITOR_SCRIPTS=$HOME/repos/dotfiles/scripts

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
category=$(printf "Help\nNetwork\nXKB" | dmenu -i -p "xkb[$current_xkb]:wifi[$ssid]:vpn[$vpn]:bat[$battery_level%]" \
-nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" \
-fn "$FONT")

case "$category" in
  "Help")
    app=$(printf "Xmonad\nlf" | dmenu -i -p "App:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT")
    case "$app" in
      "Xmonad")
        $DSCRIPT/dmenu-help-xmonad.sh ;;
      "lf")
        $DSCRIPT/dmenu-help-lf.sh ;;
    esac
    ;;
  "Network")
    net=$(printf "WiFi\nVPN" | dmenu -i -p "Net:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT")
    case "$net" in
      "WiFi")
        $DSCRIPT/dmenu-wifi.sh ;;
      "VPN")
        $DSCRIPT/dmenu-mullvad.sh ;;
    esac
    ;;
  "XKB")
    $DSCRIPT/dmenu-xkb.sh ;;
esac
