#!/bin/sh
# dmenu-wifi: WiFi manager via dmenu.

MONITOR_WIFI_SCRIPT="$HOME/repos/dotfiles/scripts/monitor-wifi.sh"

fc-list | grep -qi "JetBrainsMono Nerd Font" \
    && FONT="JetBrainsMono Nerd Font Mono-14" \
    || FONT="monospace-14"

menu() { dmenu -p "$1" ${2:+-l "$2"} -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT"; }

# Main
if [ "$1" = "--monitor" ]; then
    "$MONITOR_WIFI_SCRIPT" --monitor
    exit 0
fi

if [ "$1" = "--get-ssid" ]; then
    "$MONITOR_WIFI_SCRIPT" --get-ssid
    exit 0
fi

SSID=$("$MONITOR_WIFI_SCRIPT" --get-ssid)

if [ -n "$SSID" ]; then
    action=$(printf "Disconnect\nScan" | menu "WiFi ($SSID):")
    case "$action" in
        Disconnect) "$MONITOR_WIFI_SCRIPT" --disconnect ;;
        Scan)
            selected=$("$MONITOR_WIFI_SCRIPT" --scan | menu "Networks:")
            [ -n "$selected" ] && "$MONITOR_WIFI_SCRIPT" --connect "$selected"
            ;;
    esac
else
    action=$(printf "Scan\nManual\nHelp" | menu "WiFi:")
    case "$action" in
        Scan)
            selected=$("$MONITOR_WIFI_SCRIPT" --scan | menu "Networks:")
            [ -n "$selected" ] && "$MONITOR_WIFI_SCRIPT" --connect "$selected"
            ;;
        Manual)
            ssid=$(echo "" | menu "SSID:")
            if [ -n "$ssid" ]; then
                passwd=$(menu "Password for $ssid:")
                "$MONITOR_WIFI_SCRIPT" --connect "$ssid" "$passwd"
            fi
            ;;
        Help)
            "$MONITOR_WIFI_SCRIPT" --help | menu "WiFi Help" 7
            ;;
    esac
fi

