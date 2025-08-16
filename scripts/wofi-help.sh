#!/bin/bash
# origin Stephan Raabe (2024) 

config_file=~/.config/hypr/hyprland.conf
echo "Reading from: $config_file"

keybinds=""

# Detect Start String
while read -r line
do
    if [[ "$line" == "bind"* ]]; then

        line="$(echo "$line" | sed 's/$mainMod/SUPER/g')"
        line="$(echo "$line" | sed 's/bind = //g')"
        line="$(echo "$line" | sed 's/bindm = //g')"

        IFS=',' read -r kb_str cm_str <<< "$line"

        IFS=',' read -r -a kbarr <<< "$kb_str"

        item="${kbarr[0]} + ${kbarr[1]}: ${cm_str}"

        keybinds=$keybinds$item$'\n'
    fi 
done < "$config_file"

sleep 0.2
wofi --dmenu --insensitive --monitor --height 30 --prompt "Keybinds" <<< "$keybinds"