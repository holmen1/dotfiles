#!/bin/sh

# Minimal Mullvad VPN monitor

# Check if mullvad is installed
if ! command -v mullvad >/dev/null 2>&1; then
    notify-send -u critical "VPN Error" "Mullvad is not installed" -i dialog-error
    exit 1
fi

if mullvad status | grep -q "Disconnected"; then
     notify-send -u low -t 3000 -h string:fgcolor:#FFFF00 "  VPN DISCONNECTED" -i network-vpn
fi
