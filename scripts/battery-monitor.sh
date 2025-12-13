#!/bin/sh

THRESHOLD=5  # Set the threshold for critical battery level
WARNING=20   # Set the threshold for warning level

# Get battery percentage
battery_level=$(cat /sys/class/power_supply/BAT*/capacity 2>/dev/null || echo "0")

# Check if battery level is below threshold and not charging
if [ "$battery_level" -le $THRESHOLD ]; then
    notify-send -u critical "Battery Critical" "Battery level is ${battery_level}%. Connect charger now!" -i battery-caution
fi

# Optional: Medium warning
if [ "$battery_level" -le $WARNING ] && [ "$battery_level" -gt $THRESHOLD  ]; then
    notify-send -u normal "Battery Low" "Battery level is ${battery_level}%" -i battery-low
fi
