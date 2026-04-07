#!/bin/sh
# dmenu logout script - supports both Linux (systemd) and FreeBSD

# Use JetBrains Nerd Font if available, otherwise use a generic fallback
if fc-list | grep -qi "JetBrainsMono Nerd Font"; then
    FONT="JetBrainsMono Nerd Font Mono-14"
else
    FONT="monospace-14"
fi

choice=$(printf "Lock\nReboot\nPoweroff" | dmenu -i -p "Action:" -nb "#222222" -nf "#bbbbbb" -sb "#A300A3" -sf "#ffffff" \
-fn "$FONT")

case "$choice" in
  *Lock*)
    i3lock -c 000000
    ;;
  *Reboot*)
    if [ "$(uname)" = "FreeBSD" ]; then
      sudo shutdown -r now
    else
      sudo systemctl reboot
    fi
    ;;
  *Poweroff*)
    if [ "$(uname)" = "FreeBSD" ]; then
      sudo shutdown -p now
    else
      sudo systemctl poweroff
    fi
    ;;
esac
