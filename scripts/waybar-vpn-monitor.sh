#!/bin/bash
# Mullvad VPN status for Waybar with location

# Get Mullvad status
status=$(mullvad status)

if echo "$status" | grep -q "Disconnected"; then
    # Disconnected state with simple tooltip
    cat << 'EOF'
{"text":"⚠️ VPN","class":"vpn-disconnected","tooltip":"mullvad relay set location se sto"}
EOF
else
    # Connected state - extract location from "Visible location" line
    location=$(echo "$status" | grep "Visible location:" | sed 's/.*Visible location:[[:space:]]*//' | cut -d',' -f1 | head -1)
    if [[ -z "$location" ]]; then
        location="Unknown"
    fi
    echo "{\"text\":\"󰦝 $location\",\"class\":\"vpn-connected\"}"
fi