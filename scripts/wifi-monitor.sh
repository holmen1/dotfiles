#!/bin/sh

# Determine wireless interface name
IFACE=$(iwctl device list | grep 'station' | awk '{print $2}')

# If no interface found, notify and exit
[ -z "$IFACE" ] && { notify-send -u normal "Network" "No wireless interface found" -i network-wireless-offline; exit 1; }

# Check if WiFi is connected
CONNECTION_STATE=$(iwctl station "$IFACE" show | grep "State" | awk '{print $2}')

# If not connected, show notification
if [ "$CONNECTION_STATE" != "connected" ]; then
    # Check if radio is enabled
    RADIO_STATUS=$(iwctl device list | grep "$IFACE" | awk '{print $4}')
    
    if [ "$RADIO_STATUS" = "on" ]; then
        notify-send -u normal -t 150000 "WiFi Help" "WiFi enabled but not connected
iwctl station wlan0 scan
iwctl station wlan0 get-networks
iwctl --passphrase pwd station wlan0 connect network" -i network-wireless-disconnected
    else
        notify-send -u critical "Network" "WiFi is disabled" -i network-wireless-offline
    fi
fi