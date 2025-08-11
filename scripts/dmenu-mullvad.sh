#!/bin/bash
# dmenu-mullvad: Control Mullvad VPN via dmenu

# List of commands
options="status\nconnect\ndisconnect\naccount login\relay set location"

choice=$(echo -e "$options" | dmenu -i -p "Mullvad:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff")

case "$choice" in
  "status")
    out=$(mullvad status)
    notify-send "Mullvad status" "$out" ;;
  "account login")
    acc=$(echo | dmenu -p "Enter Mullvad account number:")
    [ -n "$acc" ] && mullvad account login "$acc" ;;
  "relay set location")
  loc=$(echo | dmenu -p "Country code (e.g., se, us):")
  city=$(echo | dmenu -p "City code (optional, Enter for none):")
    if [ -n "$loc" ]; then
      if [ -n "$city" ]; then
        mullvad relay set location "$loc" "$city"
      else
        mullvad relay set location "$loc"
      fi
    else
      notify-send "Mullvad" "No country entered."
    fi ;;
  "connect")
    mullvad connect ;;
  "disconnect")
    mullvad disconnect ;;
  *)
    notify-send "Mullvad" "No command selected or unknown option." ;;
esac
