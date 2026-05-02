#!/bin/sh
# dmenu-wifi: WiFi manager via dmenu.

DOTFILES="$HOME/repos/dotfiles"
# Auto-select monitor scripts: prefer debian scripts if NM is present, not iwd
if command -v nmcli >/dev/null 2>&1 && ! command -v iwctl >/dev/null 2>&1; then
    MONITOR_SCRIPTS=$DOTFILES/install/debianinstall/scripts
else
    MONITOR_SCRIPTS=$DOTFILES/scripts
fi
MONITOR_WIFI_SCRIPT="$MONITOR_SCRIPTS/monitor-wifi.sh"

fc-list | grep -qi "JetBrainsMono Nerd Font" \
    && FONT="JetBrainsMono Nerd Font Mono-14" \
    || FONT="monospace-14"

menu() { dmenu -i -p "$1" ${2:+-l "$2"} -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT"; }


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
        passwd=$(menu "Password for $ssid:")
          "$MONITOR_WIFI_SCRIPT" --connect "$ssid" "$passwd"
      fi
    ;;
  Restart) "$MONITOR_WIFI_SCRIPT" --restart ;;
esac


