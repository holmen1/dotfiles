# debianinstall
"desktop": "Xfce"

# Post-installation

disable cdrom in /etc/apt/sources.list

  
## sudo
```
$ su --login
root@hp:~# adduser holmen1 sudo
```

## packages
```
sudo apt install git curl vim code less neovim build-essential
```


## Xmonad
[Xmonad](https://github.com/holmen1/dotfiles/tree/master/dotfiles/xmonad)
is a tiling window manager for X. Windows are arranged automatically to tile the screen without gaps or overlap, maximizing screen use, here configured with xmobar to provide a status bar  
```
$ sudo apt install xmonad libghc-xmonad-dev  xmobar [libghc-xmonad-contrib-dev]
```
Additional packages for wallpaper, system-tray, d-menu, screen
```
$ sudo apt install suckless-tools trayer feh xscreensaver xcompmgr scrot
```

### dconf
sudo dconf-editor /

### Cleanup
sudo apt autoremove  
sudo apt clean  
sudo flatpak uninstall --unused


### wifi
# Show overall network status
nmcli general status

# Show all network devices
nmcli device status

# Show active connections
nmcli connection show --active

# Scan for available WiFi networks
nmcli device wifi list

# Connect to a WiFi network
nmcli device wifi connect SSID password PASSWORD

# Connect to a saved network
nmcli connection up "Network Name"

# Turn WiFi on/off
nmcli radio wifi on
nmcli radio wifi off

$ nmcli device wifi connect <SSID> password <password> ifname <ifname>


# Battery Warning Script with Dunst Notifications

Let's create a script that monitors your battery and sends urgent notifications when it's below 10%. This fits perfectly with your minimal setup and makes good use of dunst.

## Create the Battery Monitor Script

Create this script in your dotfiles repository:

````bash
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
````

Make it executable:

```bash
chmod +x ~/repos/dotfiles/scripts/battery-monitor.sh
```

## Run Periodically with a Systemd User Timer

Instead of cron, let's use systemd user timers which are more modern and don't require root:

1. Create the service file:

```bash
mkdir -p ~/.config/systemd/user/
```

2. Create the service:

````bash
[Unit]
Description=Check battery level and notify if low

[Service]
Type=oneshot
ExecStart=/home/holmen1/repos/dotfiles/scripts/battery-monitor.sh

[Install]
WantedBy=default.target
````

3. Create the timer:

````bash
[Unit]
Description=Run battery check every 2 minutes

[Timer]
OnBootSec=1min
OnUnitActiveSec=2min
Persistent=true

[Install]
WantedBy=timers.target
````

4. Enable and start the timer:

```bash
systemctl --user daemon-reload
systemctl --user enable battery-monitor.timer
systemctl --user start battery-monitor.timer
```

## Testing Your Setup

To test without waiting for your battery to actually drain:

```bash
# Force a low battery notification
notify-send -u critical "Battery Critical" "Battery level is low!" -i battery-caution
```

## Verify Status

```bash
# Check if timer is running
systemctl --user status battery-monitor.timer
```

This setup will give you urgent notifications when your battery is critically low, completing your minimal but functional XMonad environment.



--
# Setting Up Automatic Battery Monitoring

Your battery monitor script is created correctly, but it's not running automatically because we need to set up the systemd service and timer files. Here's how to set it up:

## 1. Create the Service File

```bash
mkdir -p ~/.config/systemd/user/
```

Create the service file:

```bash
cat > ~/.config/systemd/user/battery-monitor.service << 'EOF'
[Unit]
Description=Check battery level and notify if low

[Service]
Type=oneshot
ExecStart=/home/holmen1/repos/dotfiles/scripts/battery-monitor.sh

[Install]
WantedBy=default.target
EOF
```

## 2. Create the Timer File

```bash
cat > ~/.config/systemd/user/battery-monitor.timer << 'EOF'
[Unit]
Description=Run battery check every 2 minutes

[Timer]
OnBootSec=1min
OnUnitActiveSec=2min
Persistent=true

[Install]
WantedBy=timers.target
EOF
```

## 3. Enable and Start the Timer

```bash
systemctl --user daemon-reload
systemctl --user enable battery-monitor.timer
systemctl --user start battery-monitor.timer
```

## 4. Verify It's Running

```bash
systemctl --user status battery-monitor.timer
```

You should see output indicating that the timer is active (running).

To check if the timer is properly activating the service:

```bash
systemctl --user list-timers
```

This will show all active timers and when they'll next trigger.

Remember to remove the "Good status" notification from your script once you've confirmed everything is working, unless you want to keep getting battery status notifications.



----

# Setting Brightness Keybindings in .xinitrc

While you technically *can* set keybindings in your .xinitrc file, it's not the best approach. However, you can use a dedicated hotkey daemon that's launched from .xinitrc.

## Best Solution: Add xbindkeys

1. First, install xbindkeys:
   ```bash
   sudo apt install xbindkeys
   ```

2. Create a config file:
   ```bash
   touch ~/.xbindkeysrc
   ```

3. Add brightness controls to ~/.xbindkeysrc:
   ```
   # Decrease brightness
   "brightnessctl set 10%-"
     XF86MonBrightnessDown

   # Increase brightness
   "brightnessctl set +10%"
     XF86MonBrightnessUp
   ```

4. Add xbindkeys to your .xinitrc:
   ```bash
   # Launch key binding daemon
   xbindkeys &
   ```

## Why This Approach Is Better

1. **Separation of concerns**: Window manager handles window management, xbindkeys handles global hotkeys
2. **More reliable**: These hotkeys will work even if XMonad is unresponsive
3. **Easy to maintain**: All global hotkeys in one config file

This approach gives you brightness controls that work regardless of which window has focus, and are separate from your XMonad configuration.

