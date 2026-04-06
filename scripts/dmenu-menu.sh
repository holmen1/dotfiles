#!/bin/sh
# dmenu-menu: Unified menu for system

# Font detection
if fc-list | grep -qi "JetBrainsMono Nerd Font"; then
    FONT="JetBrainsMono Nerd Font Mono-14"
else
    FONT="monospace-14"
fi

# Main categories
category=$(printf "Help\nVPN\nWiFi\nXKB" | dmenu -i -p "Menu:" \
-nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" \
-fn "$FONT")

case "$category" in
  "Help")
    app=$(printf "Xmonad\nlf" | dmenu -i -p "App:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT")
    case "$app" in
      "Xmonad")
        $HOME/repos/dotfiles/scripts/dmenu-help-xmonad.sh ;;
      "lf")
        $HOME/repos/dotfiles/scripts/dmenu-help-lf.sh ;;
    esac
    ;;
  "VPN")
    $HOME/repos/dotfiles/scripts/dmenu-mullvad.sh ;;
  "WiFi")
    $HOME/repos/dotfiles/scripts/dmenu-wifi.sh ;;
  "XKB")
    $HOME/repos/dotfiles/scripts/dmenu-xkb.sh ;;
esac
