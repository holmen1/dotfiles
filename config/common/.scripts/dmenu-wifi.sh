#!/bin/sh
# dmenu-wifi: WiFi manager via dmenu (Arch/iwd).

MONITOR_WIFI_SCRIPT="$HOME/.scripts/monitor-wifi.sh"

#fc-list | grep -qi "JetBrainsMono Nerd Font" \
FONT="Liberation Mono-16"
MENU_COLORS="-nb #222222 -nf #ffbf00 -sb #ffbf00 -sf #222222"
# NB ignore globbing warning, breaks dmenu if using

menu() { dmenu -i -p "$1" ${2:+-l "$2"} $MENU_COLORS -fn "$FONT"; }

action=$(printf "Scan\nManual\nDisconnect\nRestart" | menu "WiFi:")
case "$action" in
  Disconnect) "$MONITOR_WIFI_SCRIPT" --disconnect ;;
  Scan)
    selected=$("$MONITOR_WIFI_SCRIPT" --scan | menu "Networks:")
    [ -n "$selected" ] && "$MONITOR_WIFI_SCRIPT" --connect "$selected"
    ;;
  Manual)
    ssid=$(echo "" | menu "SSID:")
    if [ -n "$ssid" ]; then
      passwd=$(echo "" | menu "Password for $ssid:")
      "$MONITOR_WIFI_SCRIPT" --connect "$ssid" "$passwd"
    fi
    ;;
  Restart) "$MONITOR_WIFI_SCRIPT" --restart ;;
esac
