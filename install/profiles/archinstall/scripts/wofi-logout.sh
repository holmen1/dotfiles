#!/bin/bash
# hyprland logout script
choice=$(echo -e "Lock\nReboot\nPoweroff\nExit" | \
wofi --dmenu -i -j --location 3 --lines 4 --width 100 --font "JetBrainsMono Nerd Font 14")

case "$choice" in
  *Lock*)
    swaylock -c 000000
    ;;
  *Reboot*)
    systemctl reboot
    ;;
  *Poweroff*)
    systemctl poweroff
    ;;
  *Exit*)
    hyprctl dispatch exit
    ;;
esac