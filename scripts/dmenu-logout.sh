#!/bin/bash
# dmenu logout script
choice=$(echo -e "Lock\nReboot\nPoweroff\nLogout" | dmenu -i -p "Action:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff")

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
  *Logout*)
    pkill xmonad
    ;;
esac