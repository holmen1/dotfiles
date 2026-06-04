#!/bin/sh
# dmenu-logout: Lock, reboot, poweroff (Artix).

#fc-list | grep -qi "JetBrainsMono Nerd Font" 
FONT="Liberation Mono-16"

choice=$(printf "Poweroff\nReboot" | dmenu -i -p "Action:" -nb "#222222" -nf "#bbbbbb" -sb "#A300A3" -sf "#ffffff" \
-fn "$FONT")

case "$choice" in
  Reboot)   sudo reboot ;;
  Poweroff) sudo poweroff ;;
esac
