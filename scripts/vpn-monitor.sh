#!/bin/sh

# Minimal Mullvad VPN monitor
if mullvad status | grep -q "Disconnected"; then
     notify-send -u low -t 3000 -h string:fgcolor:#FFFF00 "  VPN DISCONNECTED" -i network-vpn
fi
