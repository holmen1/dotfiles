#!/bin/bash

# Determine wireless interface name
IFACE=$(iwctl device list | grep -oP 'station \K\w+')

# If no interface found, notify and exit
if [ -z "$IFACE" ]; then
    notify-send -u normal "Network" "No wireless interface found" -i network-wireless-offline
    exit 1
fi

# Check if WiFi is connected
CONNECTION_STATUS=$(iwctl station "$IFACE" show | grep "Connected" | awk '{print $2}')

# If not connected, show notification
if [ "$CONNECTION_STATUS" != "yes" ]; then
    # Check if radio is enabled
    RADIO_STATUS=$(iwctl device list | grep -A 5 "$IFACE" | grep Powered | awk '{print $2}')
    
    if [ "$RADIO_STATUS" = "on" ]; then
        notify-send -u normal "Network" "WiFi enabled but not connected" -i network-wireless-disconnected
    else
        notify-send -u normal "Network" "WiFi is disabled" -i network-wireless-offline
    fi
fi