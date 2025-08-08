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

### sudo without password

To allow all members of the `wheel` group to use `sudo` without a password, edit `/etc/sudoers` using `visudo`:

Uncomment this line:
```
%wheel ALL=(ALL:ALL) NOPASSWD: ALL
```

If your installer or manual setup created a user-specific file like `/etc/sudoers.d/00_user` with:
```
user ALL=(ALL) ALL
```
this can conflict with the wheel group NOPASSWD rule and override your intended permissions. If your user is in the `wheel` group and the group rule above is present, you should delete the user-specific file to avoid conflicts.

Make sure your user is in the `wheel` group!

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
systemctl --user status system-monitor.timer
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
## LESSONS LEARNED
### ⚠️ NEVER Edit /etc/sudoers with vim (or any editor that isn't visudo)

**What I did:** "I'm a rebel, I don't need `visudo`! I'll just vim that file real quick..."

**What happened:** Created a rogue `/etc/sudoers.d/00_holmen1` file that overrode my carefully crafted NOPASSWD rules. Spent an embarrassing amount of time troubleshooting why `sudo poweroff` kept asking for passwords that didn't work.

**The fix:** Had to `su -` to root, delete the terrorist file, reset my password, and remember that `visudo` exists for very good reasons.

**The moral:** `visudo` checks syntax, handles file locking, and prevents you from accidentally becoming your own worst enemy. Use it. Always. Your future self will thank you instead of cursing your name.

**Acceptable excuses for bypassing visudo:** None. Zero. Not even "but I'm in a hurry."

## Nice to have

### VPN

```
mullvad status
mullvad account login <your_account_number>
mullvad relay list
mullvad relay set location se sto
mullvad connect
mullvad disconnect
```


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
