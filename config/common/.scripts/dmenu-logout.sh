#!/bin/sh
# dmenu-logout: Lock, reboot, poweroff (Artix).

fc-list | grep -qi "JetBrainsMono Nerd Font" \
    && FONT="JetBrainsMono Nerd Font Mono-14" \
    || FONT="monospace-14"

choice=$(printf "Poweroff\nReboot\nLock" | dmenu -i -p "Action:" -nb "#222222" -nf "#bbbbbb" -sb "#A300A3" -sf "#ffffff" \
-fn "$FONT")

case "$choice" in
  Lock)     i3lock -c 000000 ;;
  Reboot)   sudo reboot ;;
  Poweroff) sudo poweroff ;;
esac
