#!/bin/sh
# dmenu-mullvad: Control Mullvad VPN via dmenu

# Use JetBrains Nerd Font if available, otherwise use a generic fallback
if fc-list | grep -qi "JetBrainsMono Nerd Font"; then
    FONT="JetBrainsMono Nerd Font Mono-14"
else
    FONT="monospace-14"
fi

# List of commands
choice=$(printf "status\nconnect\nreconnect\ndisconnect\nlocation\nhelp" | dmenu -i -p "Mullvad:" \
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
      echo "No country entered." | dmenu -l 1 -fn "$FONT"
    fi ;;
  *)
    echo "No command selected or unknown option." | dmenu -l 1 -fn "$FONT" ;;
esac
