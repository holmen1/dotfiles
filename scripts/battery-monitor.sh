#!/bin/bash
# filepath: /home/holmen1/repos/dotfiles/scripts/battery-monitor.sh

# Get battery percentage
battery_level=$(cat /sys/class/power_supply/BAT*/capacity 2>/dev/null || echo "0")

# Get charging status
charging_status=$(cat /sys/class/power_supply/BAT*/status 2>/dev/null || echo "Unknown")

# Check if battery level is below threshold and not charging
if [ "$battery_level" -le 10 ] && [ "$charging_status" != "Charging" ]; then
    notify-send -u critical "Battery Critical" "Battery level is ${battery_level}%. Connect charger now!" -i battery-caution
fi

# Optional: Medium warning
if [ "$battery_level" -le 20 ] && [ "$charging_status" != "Charging" ] && [ "$battery_level" -gt 10 ]; then
    notify-send -u normal "Battery Low" "Battery level is ${battery_level}%" -i battery-low
fi

# Good status for testing
if [ "$battery_level" -gt 20 ]; then
    status_msg="Battery level is good at ${battery_level}%"
    
    if [ "$charging_status" = "Charging" ]; then
        status_msg+=" and charging"
    fi
    
    notify-send -u low "Battery Status" "$status_msg" -i battery-good
fi