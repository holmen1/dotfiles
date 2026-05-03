#!/bin/sh
# dmenu-logout: Lock, reboot, poweroff (Arch/systemd).

fc-list | grep -qi "JetBrainsMono Nerd Font" \
    && FONT="JetBrainsMono Nerd Font Mono-14" \
    || FONT="monospace-14"

choice=$(printf "Lock\nReboot\nPoweroff" | dmenu -i -p "Action:" -nb "#222222" -nf "#bbbbbb" -sb "#A300A3" -sf "#ffffff" \
-fn "$FONT")

case "$choice" in
  Lock)     i3lock -c 000000 ;;
  Reboot)   sudo systemctl reboot ;;
  Poweroff) sudo systemctl poweroff ;;
esac
