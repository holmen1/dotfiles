#!/bin/sh

OS=$(uname -s)

# Determine wireless interface name and check connection status
if [ "$OS" = "Linux" ]; then
    # Linux: Use iwd (iwctl)
    IFACE=$(iwctl device list 2>/dev/null | grep 'station' | awk '{print $2}')
    
    # If no interface found, notify and exit
    [ -z "$IFACE" ] && { notify-send -u normal "Network" "No wireless interface found" -i network-wireless-offline; exit 1; }
    
    # Check if WiFi is connected
    CONNECTION_STATE=$(iwctl station "$IFACE" show 2>/dev/null | grep "State" | awk '{print $2}')
    
    # If not connected, show notification
    if [ "$CONNECTION_STATE" != "connected" ]; then
        # Check if radio is enabled
        RADIO_STATUS=$(iwctl device list 2>/dev/null | grep "$IFACE" | awk '{print $4}')
        
        if [ "$RADIO_STATUS" = "on" ]; then
            notify-send -u normal -t 150000 "WiFi Help" "WiFi enabled but not connected
iwctl station wlan0 scan
iwctl station wlan0 get-networks
iwctl --passphrase pwd station wlan0 connect network" -i network-wireless-disconnected
        else
            notify-send -u critical "Network" "WiFi is disabled" -i network-wireless-offline
        fi
    fi

elif [ "$OS" = "FreeBSD" ]; then
    # FreeBSD: Use ifconfig
    IFACE=$(ifconfig -l 2>/dev/null | tr ' ' '\n' | grep -E '^(wlan|iwm)' | head -1)
    
    [ -z "$IFACE" ] && { notify-send -u normal "Network" "No wireless interface found" -i network-wireless-offline; exit 1; }
    
    # Check if WiFi is connected
    CONNECTION_STATE=$(ifconfig "$IFACE" 2>/dev/null | grep "status:" | awk '{print $2}')
    
    if [ "$CONNECTION_STATE" != "associated" ]; then
        notify-send -u normal -t 150000 "WiFi Help" "WiFi not connected
sudo ifconfig $IFACE up
sudo wpa_cli scan
sudo wpa_cli scan_results" -i network-wireless-disconnected
    fi
fi