#!/bin/bash
# dmenu logout script
choice=$(echo -e "Lock\nReboot\nPoweroff" | dmenu -i -p "Action:" -nb "#222222" -nf "#bbbbbb" -sb "#A300A3" -sf "#ffffff" \
-fn "JetBrainsMono Nerd Font Mono-14")

case "$choice" in
  *Lock*)
    i3lock -c 000000
    ;;
  *Reboot*)
    sudo systemctl reboot
    ;;
  *Poweroff*)
    sudo systemctl poweroff
    ;;
esac
