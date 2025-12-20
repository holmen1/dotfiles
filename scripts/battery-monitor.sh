#!/bin/sh

THRESHOLD=5  # Set the threshold for critical battery level
WARNING=20   # Set the threshold for warning level

OS=$(uname -s)

# Get battery percentage based on OS
if [ "$OS" = "Linux" ]; then
    battery_level=$(cat /sys/class/power_supply/BAT*/capacity 2>/dev/null || echo "0")
elif [ "$OS" = "FreeBSD" ]; then
    # FreeBSD: Get battery percentage from acpiconf
    battery_level=$(acpiconf -i 0 2>/dev/null | grep "Remaining capacity:" | awk '{print $3}' | sed 's/%//')
    battery_level=${battery_level:-0}
else
    battery_level=0
fi

# Check if battery level is below threshold
if [ "$battery_level" -le $THRESHOLD ]; then
    notify-send -u critical "Battery Critical" "Battery level is ${battery_level}%. Connect charger now!" -i battery-caution
fi

# Optional: Medium warning
if [ "$battery_level" -le $WARNING ] && [ "$battery_level" -gt $THRESHOLD  ]; then
    notify-send -u normal "Battery Low" "Battery level is ${battery_level}%" -i battery-low
fi
