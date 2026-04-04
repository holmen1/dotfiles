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

# Get current status
get_status() {
    case "$OS" in
        linux)
            IFACE=$(iwctl device list 2>/dev/null | grep 'station' | awk '{print $2}')
            [ -z "$IFACE" ] && echo "no-interface" && return
            STATE=$(iwctl station "$IFACE" show 2>/dev/null | grep "State" | awk '{print $2}')
            if [ "$STATE" = "connected" ]; then
                SSID=$(iwctl station "$IFACE" show 2>/dev/null | grep "Connected network" | awk '{print $3}')
                echo "connected:$SSID"
            else
                echo "disconnected"
            fi
            ;;
        freebsd)
            IFACE=$(ifconfig -l 2>/dev/null | tr ' ' '\n' | grep '^wlan' | head -1)
            [ -z "$IFACE" ] && echo "no-interface" && return
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
    case "$OS" in
        linux)
            IFACE=$(iwctl device list 2>/dev/null | grep 'station' | awk '{print $2}')
            iwctl station "$IFACE" scan >/dev/null 2>&1
            sleep 1
            iwctl station "$IFACE" get-networks 2>/dev/null | sed 's/\x1b\[[0-9;]*m//g' | tail -n +3 | grep -v '^ *-' | grep -v '^$' | awk '{print $1}'
            ;;
        freebsd)
            IFACE=$(ifconfig -l 2>/dev/null | tr ' ' '\n' | grep '^wlan' | head -1)
            sudo ifconfig "$IFACE" up list scan 2>/dev/null | awk '{print $1}' | grep -v SSID | sort -u
            ;;
    esac
}

# Connect to network
connect_network() {
    local ssid="$1"
    local passwd="$2"
    case "$OS" in
        linux)
            IFACE=$(iwctl device list 2>/dev/null | grep 'station' | awk '{print $2}')
            if [ -n "$passwd" ]; then
                iwctl --passphrase "$passwd" station "$IFACE" connect "$ssid"
            else
                iwctl station "$IFACE" connect "$ssid"
            fi
            ;;
        freebsd)
            IFACE=$(ifconfig -l 2>/dev/null | tr ' ' '\n' | grep '^wlan' | head -1)
            if [ -n "$passwd" ]; then
                sudo iwctl --passphrase "$passwd" station "$IFACE" connect "$ssid"
            else
                sudo iwctl station "$IFACE" connect "$ssid"
            fi
            ;;
    esac
}

# Disconnect
disconnect_network() {
    case "$OS" in
        linux)
            IFACE=$(iwctl device list 2>/dev/null | grep 'station' | awk '{print $2}')
            iwctl station "$IFACE" disconnect
            notify-send "WiFi" "Disconnected"
            ;;
        freebsd)
            IFACE=$(ifconfig -l 2>/dev/null | tr ' ' '\n' | grep '^wlan' | head -1)
            sudo ifconfig "$IFACE" down
            notify-send "WiFi" "Disconnected"
            ;;
    esac
}

# Main flow
status=$(get_status)

if echo "$status" | grep -q "^connected:"; then
    SSID=$(echo "$status" | cut -d: -f2)
    action=$(printf "Disconnect\nScan" | dmenu -p "WiFi [$SSID]:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT")
    case "$action" in
        Disconnect) disconnect_network ;;
        Scan) selected=$(scan_networks | dmenu -p "Networks:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT")
              if [ -n "$selected" ]; then
                  passwd=$(echo "" | dmenu -p "Password for $selected:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT" -mask '*')
                  connect_network "$selected" "$passwd"
              fi ;;
    esac
else
    action=$(printf "Scan\nManual" | dmenu -p "WiFi:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT")
    case "$action" in
        Scan) selected=$(scan_networks | dmenu -p "Networks:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT")
              if [ -n "$selected" ]; then
                  passwd=$(echo "" | dmenu -p "Password for $selected:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT" -mask '*')
                  connect_network "$selected" "$passwd"
              fi ;;
        Manual) ssid=$(echo "" | dmenu -p "SSID:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT")
                if [ -n "$ssid" ]; then
                    passwd=$(echo "" | dmenu -p "Password:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT" -mask '*')
                    connect_network "$ssid" "$passwd"
                fi ;;
    esac
fi

