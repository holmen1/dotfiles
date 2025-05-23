# install
Guides for setting up Linux environments quickly and consistently

## arch
Go to [Arch Linux installation guide](./archinstall/README.md)

## debian
Go to [Debian installation guide](./debianinstall/README.md)


## Post-installation

### Configure Git
git config --global user.name "$git_username"
git config --global user.email "$git_email"

### SSH
Generate SSH key
```
ssh-keygen -t ed25519 -C "$git_email" -f ~/.ssh/id_ed25519 -N ""
```
Start the ssh-agent in the background
```
eval "$(ssh-agent -s)"
```
Add the SSH key to the ssh-agent
```
ssh-add ~/.ssh/id_ed25519
```

If reusing copy, set correct permissions for your private key
```
chmod 600 ~/.ssh/id_ed25519
```
After you generate an SSH key pair, you must add the public key to GitHub.com to enable SSH access for your account

### System Monitoring (Battery & WiFi) with Dunst Notifications

```bash
# Create symlinks
ln -s ~/repos/dotfiles/scripts/system-monitor.service ~/.config/systemd/user/system-monitor.service
ln -s ~/repos/dotfiles/scripts/system-monitor.timer ~/.config/systemd/user/system-monitor.timer

# Enable and start
systemctl --user daemon-reload
systemctl --user enable system-monitor.timer
systemctl --user start system-monitor.timer
```

To test without waiting for your battery to actually drain
```bash
# Force a low battery notification
notify-send -u critical "Battery Critical" "Battery level is low!" -i battery-caution
```

Verify Status
```bash
# Check if timer is running
systemctl --user status battery-monitor.timer
```

This setup will give you urgent notifications when your battery is critically low or wifi not connected

### Setting Brightness and Volume Keybindings with xbindkeys

1. First, install xbindkeys
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

   # Simple ALSA Volume Controls
   "amixer -q set Master 5%+ unmute"
    XF86AudioRaiseVolume

   "amixer -q set Master 5%- unmute"
    XF86AudioLowerVolume

   "amixer -q set Master toggle"
    XF86AudioMute
   ```
4. Add xbindkeys to your .xinitrc:
   ```bash
   # Launch key binding daemon
   xbindkeys &
   ```

### Link dotfiles
Edit and run
[./link_x_config.sh](./link_x_config.sh)

### startx
Enter tty via C-A-F2 or disable display manager (lightdm)  
```$ sudo systemctl disable lightdm```

login, then startx  

If fail, try
```
$ xmonad --recompile 
```
Start xmonad with: startx  
start xfce: startx /usr/bin/startxfce4

leave x session
```
$ pkill x 
```

### Display manager  

To enable display manager, confirm ```.config/xmonad/xmonad-session-rc```linked   
Edit xmonad-session like so
```
$ diff /usr/bin/xmonad-session /usr/bin/xmonad-session.bak 
3c3
< if [ -r ".config/xmonad/xmonad-session-rc" ]
---
> if [ -r ".xmonad/xmonad-session-rc" ]
5c5
<   . .config/xmonad/xmonad-session-rc
---
>   . .xmonad/xmonad-session-rc
```


## Nice to have

### VSCode binary
Download tar.gz

```
$ tar -xvzf code-stable-x64-1734606550.tar.gz 
```
Move
```
$ sudo mv Downloads/VSCode-linux-x64/ /opt/
```

Link
```
$ sudo ln -s /opt/VSCode-linux-x64/bin/code /usr/local/bin/code
```

### Useful Keyboard Shortcuts
- `Ctrl+Alt+F2` - Switch to TTY2 console (F1-F6 for different TTY sessions)
- `Ctrl+Alt+F7` - Return to GUI session
- `Ctrl+Alt+T` - Open terminal (in most desktop environments)

### Mounting USB Drives
Identify the USB drive:
```
lsblk
```
Create a mount point:
```
sudo mkdir -p /mnt/usb
```
Mount the USB drive:
```
sudo mount /dev/sdX1 /mnt/usb
# Replace sdX1 with your device identifier from lsblk (e.g., sdb1)
```
Unmount when finished:
```
sudo umount /mnt/usb
```

### Find files by name
```
find /path/to/search -name "filename.txt" 2> /dev/null
```

Find and execute a command on each result:
```
find /path/to/search -name "*.txt" -exec grep "search term" {} \;
```

### Repo behind remote

```bash
git stash [-u]
git pull
git stash apply
```
