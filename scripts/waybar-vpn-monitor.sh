#!/bin/bash
# Minimal Mullvad VPN status for Waybar
if mullvad status | grep -q "Disconnected"; then
    echo '{"text":"⚠️ Not Secure","class":"vpn-disconnected"}'
fi