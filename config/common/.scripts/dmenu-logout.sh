#!/bin/sh
# dmenu-logout: Lock, reboot, poweroff (Artix).

#fc-list | grep -qi "JetBrainsMono Nerd Font" 
FONT="Liberation Mono-16"
MENU_COLORS="-nb #222222 -nf #ffbf00 -sb #ffbf00 -sf #222222"
# NB ignore globbing warning, breaks dmenu if using

choice=$(printf "Poweroff\nReboot" | dmenu -i -p "Action:" $MENU_COLORS -fn "$FONT")

case "$choice" in
  Reboot)   sudo reboot ;;
  Poweroff) sudo poweroff ;;
esac
