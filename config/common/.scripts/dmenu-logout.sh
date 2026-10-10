#!/bin/sh
# dmenu-logout: Lock, reboot, poweroff (Artix).

#fc-list | grep -qi "JetBrainsMono Nerd Font" 
FONT="Liberation Mono-18"

# NB ignore LSP globbing warning on color variable
choice=$(printf "Poweroff\nReboot" | dmenu -i -p "Action:" $DMENU_COLORS -fn "$FONT")

case "$choice" in
  Reboot)   sudo reboot ;;
  Poweroff) sudo poweroff ;;
esac
