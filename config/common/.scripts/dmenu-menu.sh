#!/bin/sh
# dmenu-menu: Unified menu for system (Artix)

SCRIPTS=$HOME/.scripts
CONF_DIR=$HOME/repos/dotfiles/config
XKB_STATE=$HOME/.cache/xkb-layout

# Font detection
#fc-list | grep -qi "JetBrainsMono Nerd Font" \
FONT="Liberation Mono-16"
MENU_COLORS="-nb #222222 -nf #ffbf00 -sb #ffbf00 -sf #222222"
HELP_COLORS="-nb #222222 -nf #ffbf00 -sb #222222 -sf #ffbf00"
# NB ignore globbing warning, breaks dmenu if using

current_xkb=$(cat "$XKB_STATE" 2>/dev/null || echo "se")
battery_level=$("$SCRIPTS"/monitor-battery.sh --get-level)
ssid=$("$SCRIPTS"/monitor-wifi.sh --get-ssid)
vpn=$("$SCRIPTS"/monitor-vpn.sh --get-location)

# Tray: keyboard, wifi, vpn and battery status
# Main menu: Exit, Wifi or Help
category=$(printf "Exit\nNetwork\nHelp" | timeout 4s dmenu -i -p "x[$current_xkb] w[$ssid] v[$vpn] b[$battery_level%]" \
$MENU_COLORS \
-fn "$FONT")

case "$category" in
  "Help")
    app=$(printf "XKB\nlf\nXmonad\nwifi\nbash\nnvim" | dmenu -i -p "App:" $MENU_COLORS -fn "$FONT")
    case "$app" in
      "XKB")
        sed -n 9,37p "$CONF_DIR/xkb/README.md" | timeout 12s dmenu -l 29 -p "XKB Help" \
		$HELP_COLORS -fn "$FONT" ;;
      "Xmonad")
        sed -n 12,41p "$CONF_DIR/xmonad/README.md" | timeout 12s dmenu -l 25 -i -p "XMonad Help" \
		$HELP_COLORS -fn "$FONT" ;;
      "lf")
        sed -n 14,40p "$CONF_DIR/lf/README.md" | timeout 12s dmenu -l 23 -i -p "lf Help" \
		$HELP_COLORS -fn "$FONT" ;;
      "wifi")
        "$SCRIPTS"/monitor-wifi.sh --help | timeout 12s dmenu -l 7 -p "wifi Help" \
		$HELP_COLORS -fn "$FONT" ;;
      "bash")
        sed -n 51,74p "$CONF_DIR/bash/.bashrc" | timeout 12s dmenu -l 25 -p "git Help" \
		$HELP_COLORS -fn "$FONT" ;;
      "nvim")
        echo "<leader>sk" | timeout 2s dmenu -l 7 -p "Search Keymaps" \
		$HELP_COLORS -fn "$FONT" ;;
    esac ;;
  "Network")
    net=$(printf "WiFi\nVPN" | dmenu -i -p "Net:" $MENU_COLORS -fn "$FONT")
    case "$net" in
      "WiFi") "$SCRIPTS"/dmenu-wifi.sh ;;
      "VPN")  "$SCRIPTS"/dmenu-vpn.sh ;;
    esac ;;
  "Exit")
    "$SCRIPTS"/dmenu-logout.sh ;;
esac
