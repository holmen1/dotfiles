#!/bin/bash
# dmenu-mullvad: Control Mullvad VPN via dmenu

FONT="JetBrainsMono Nerd Font Mono-14"

# List of commands
options="status\nconnect\nreconnect\ndisconnect\nlocation\nhelp"

choice=$(echo -e "$options" | dmenu -i -p "Mullvad:" \
-nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" \
-fn "$FONT")

case "$choice" in
  "status")
    mullvad status | dmenu -l 5 -fn "$FONT" ;;
  "connect")
    mullvad connect ;;
  "reconnect")
    mullvad reconnect ;;
  "disconnect")
    mullvad disconnect ;;
  "help")
    mullvad help | dmenu -l 26 -fn "$FONT" ;;
  "location")
  loc=$(echo | dmenu -p "Country code (e.g., se, us):" -fn "$FONT")
  city=$(echo | dmenu -p "City code (optional, Enter for none):" -fn "$FONT")
    if [ -n "$loc" ]; then
      if [ -n "$city" ]; then
        mullvad relay set location "$loc" "$city"
      else
        mullvad relay set location "$loc"
      fi
    else
      notify-send "Mullvad" "No country entered."
    fi ;;
  *)
    notify-send "Mullvad" "No command selected or unknown option." ;;
esac
