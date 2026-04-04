#!/bin/sh
# dmenu-wifi: Minimal WiFi manager

# Detect OS
case "$(uname -s)" in
    FreeBSD*) OS="freebsd" ;;
    Linux*)   OS="linux" ;;
    *)        OS="unknown" ;;
esac

# Font
if fc-list | grep -qi "JetBrainsMono Nerd Font"; then
    FONT="JetBrainsMono Nerd Font Mono-14"
else
    FONT="monospace-14"
fi

# Get interface
get_iface() {
    case "$OS" in
        linux)
            iwctl device list 2>/dev/null | grep 'station' | awk '{print $2}'
            ;;
        freebsd)
            ifconfig -l 2>/dev/null | tr ' ' '\n' | grep '^wlan' | head -1
            ;;
    esac
}

# Get current status
get_status() {
    IFACE=$(get_iface)
    [ -z "$IFACE" ] && echo "no-interface" && return
    
    case "$OS" in
        linux)
            STATE=$(iwctl station "$IFACE" show 2>/dev/null | grep "State" | awk '{print $2}')
            if [ "$STATE" = "connected" ]; then
                SSID=$(iwctl station "$IFACE" show 2>/dev/null | grep "Connected network" | awk '{print $3}')
                echo "connected:$SSID"
            else
                echo "disconnected"
            fi
            ;;
        freebsd)
            STATE=$(ifconfig "$IFACE" 2>/dev/null | grep "status:" | awk '{print $2}')
            SSID=$(ifconfig "$IFACE" 2>/dev/null | grep "ssid" | awk -F'"' '{print $2}')
            if [ "$STATE" = "associated" ] && [ -n "$SSID" ]; then
                echo "connected:$SSID"
            else
                echo "disconnected"
            fi
            ;;
    esac
}

# Scan networks
scan_networks() {
    IFACE=$(get_iface)
    case "$OS" in
        linux)
            iwctl station "$IFACE" scan >/dev/null 2>&1
            sleep 1
            iwctl station "$IFACE" get-networks 2>/dev/null | sed 's/\x1b\[[0-9;]*m//g' | tail -n +3 | grep -v '^ *-' | grep -v '^$' | awk '{print $1}'
            ;;
        freebsd)
            sudo ifconfig "$IFACE" up list scan 2>/dev/null | awk '{print $1}' | grep -v SSID | sort -u
            ;;
    esac
}

# Connect to network
connect_network() {
    local ssid="$1"
    local passwd="$2"
    IFACE=$(get_iface)
    case "$OS" in
        linux)
            if [ -n "$passwd" ]; then
                iwctl --passphrase "$passwd" station "$IFACE" connect "$ssid" 2>&1
            else
                iwctl station "$IFACE" connect "$ssid" 2>&1
            fi
            sleep 1
            STATE=$(iwctl station "$IFACE" show 2>/dev/null | grep "State" | awk '{print $2}')
            if [ "$STATE" = "connected" ]; then
                notify-send "WiFi" "Connected to $ssid"
            fi
            ;;
        freebsd)
            if [ -n "$passwd" ]; then
                sudo iwctl --passphrase "$passwd" station "$IFACE" connect "$ssid" 2>&1
            else
                sudo iwctl station "$IFACE" connect "$ssid" 2>&1
            fi
            sleep 1
            STATE=$(ifconfig "$IFACE" 2>/dev/null | grep "status:" | awk '{print $2}')
            if [ "$STATE" = "associated" ]; then
                notify-send "WiFi" "Connected to $ssid"
            fi
            ;;
    esac
}

# Disconnect
disconnect_network() {
    IFACE=$(get_iface)
    case "$OS" in
        linux)
            iwctl station "$IFACE" disconnect
            notify-send "WiFi" "Disconnected"
            ;;
        freebsd)
            sudo ifconfig "$IFACE" down
            notify-send "WiFi" "Disconnected"
            ;;
    esac
}

# Scan and connect workflow
scan_and_connect() {
    selected=$(scan_networks | dmenu -p "Networks:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT")
    [ -n "$selected" ] && connect_network "$selected" ""
}

# Main flow
status=$(get_status)

if echo "$status" | grep -q "^connected:"; then
    SSID=$(echo "$status" | cut -d: -f2)
    action=$(printf "Disconnect\nScan" | dmenu -p "WiFi [$SSID]:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT")
    case "$action" in
        Disconnect) disconnect_network ;;
        Scan) scan_and_connect ;;
    esac
else
    action=$(printf "Scan\nManual" | dmenu -p "WiFi:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT")
    case "$action" in
        Scan) scan_and_connect ;;
        Manual) ssid=$(echo "" | dmenu -p "SSID:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT")
                if [ -n "$ssid" ]; then
                    passwd=$(dmenu -p "Password for $ssid:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT" < /dev/null)
                    connect_network "$ssid" "$passwd"
                fi ;;
    esac
fi

