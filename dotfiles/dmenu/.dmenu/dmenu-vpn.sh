#!/bin/sh
# dmenu-vpn: Control Mullvad VPN via dmenu

MONITOR_VPN_SCRIPT="$HOME/repos/dotfiles/scripts/monitor-vpn.sh"

fc-list | grep -qi "JetBrainsMono Nerd Font" && FONT="JetBrainsMono Nerd Font Mono-14" || FONT="monospace-14"

choice=$(printf "connect\ndisconnect\nlocation\nstatus\nhelp" | dmenu -i -p "Mullvad:" \
-nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" \
-fn "$FONT")

case "$choice" in
  "status")
    "$MONITOR_VPN_SCRIPT" --status | dmenu -l 5 -fn "$FONT" ;;
  "connect")
    "$MONITOR_VPN_SCRIPT" --connect ;;
  "disconnect")
    "$MONITOR_VPN_SCRIPT" --disconnect ;;
  "help")
    "$MONITOR_VPN_SCRIPT" --help | dmenu -l 26 -fn "$FONT" ;;
  "location")
    loc=$(echo | dmenu -p "Country code (e.g., se, us):" -fn "$FONT")
    if [ -n "$loc" ]; then
      city=$(echo | dmenu -p "City code (optional, Enter for none):" -fn "$FONT")
      [ -n "$city" ] && "$MONITOR_VPN_SCRIPT" --set-location "$loc" "$city" || "$MONITOR_VPN_SCRIPT" --set-location "$loc"
    else
      echo "No country entered." | dmenu -l 1 -fn "$FONT"
    fi ;;
  *)
    echo "No command selected or unknown option." | dmenu -l 1 -fn "$FONT" ;;
esac
