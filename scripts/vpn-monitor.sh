#!/bin/bash

# Minimal Mullvad VPN monitor
if mullvad status | grep -q "Disconnected"; then
    notify-send -u low -t 2000 "VPN Warning" "Mullvad VPN is disconnected!" -i network-vpn
fi
