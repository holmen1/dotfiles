#!/bin/sh
# dmenu-vpn: Control Mullvad VPN via dmenu (Arch).

MONITOR_VPN_SCRIPT="$HOME/.scripts/monitor-vpn.sh"

fc-list | grep -qi "JetBrainsMono Nerd Font" \
    && FONT="JetBrainsMono Nerd Font Mono-14" \
    || FONT="monospace-14"

menu() { dmenu -i -p "$1" ${2:+-l "$2"} -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT"; }

choice=$(printf "connect\ndisconnect\nlocation\nstatus\nhelp" | menu "Mullvad:")
case "$choice" in
  status)
    "$MONITOR_VPN_SCRIPT" --status | dmenu -l 5 -fn "$FONT" ;;
  connect)
    "$MONITOR_VPN_SCRIPT" --connect ;;
  disconnect)
    "$MONITOR_VPN_SCRIPT" --disconnect ;;
  help)
    "$MONITOR_VPN_SCRIPT" --help | dmenu -l 26 -fn "$FONT" ;;
  location)
    loc=$(echo "" | menu "Country code (e.g., se, us):")
    if [ -n "$loc" ]; then
      city=$(echo "" | menu "City code (optional, Enter for none):")
      if [ -n "$city" ]; then
        "$MONITOR_VPN_SCRIPT" --set-location "$loc" "$city"
      else
        "$MONITOR_VPN_SCRIPT" --set-location "$loc"
      fi
    fi ;;
esac
