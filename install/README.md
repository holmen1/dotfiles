# install
Guides for setting up environments quickly and consistently — Linux, BSD, macOS

## ISO

```bash
# Download ISO and signature
wget image.iso
wget image.iso.sig

# Import distributor's key (one-time)
gpg --recv-keys DISTRIBUTOR_KEYID

# Verify signature
gpg --verify image.iso.sig image.iso

gpg --auto-key-retrieve --verify artix-base-openrc-20260402-x86_64.iso.sig artix-base-openrc-20260402-x86_64.iso
```

```bash
# macos
diskutil list
diskutil unmountDisk /dev/disk4                  
```

```bash                
sudo dd bs=4M if=artix-base-openrc-20260402-x86_64.iso of=/dev/disk4 status=progress oflag=sync
```

## Base install

- **artix (full install)** Go to [Artix Linux installation guide](./profiles/artixinstall/README.md)

- **arch (using archinstall)** Go to [Arch Linux installation guide](./profiles/archinstall/README.md)

- **debian** Go to [Debian installation guide](./profiles/debianinstall/README.md)

- **bsd** Go to [FreeBSD installation guide](./profiles/bsdinstall/README.md)

- **mac** Go to [MacOS config guide](./profiles/macinstall/README.md)

## Post-install

### Confirm WiFi

Verify connectivity:
```
ping -c 3 artixlinux.org
```

Troubleshooting:
```
ip addr show wlan0        # should show inet address
ping -c 3 1.1.1.1         # tests IP routing
ping -c 3 artixlinux.org  # tests DNS
```
If `/etc/resolv.conf` empty, add:
```
nameserver 8.8.8.8 # Primary DNS
nameserver 1.1.1.1
```
`chmod 444` so not overwritten, `chmod 644` if need edit 

### Add user

```bash
useradd -m -G wheel[,video] username
#passwd username
```

To add when already in group
```
sudo usermod -aG input $USER
```
or `audio`

#### Passwordless

This is generally not recommended for regular users due to security risks

The passwd -d command deletes (removes) a user's password if set, allowing passwordless login for that account

```bash
passwd -d username
```

#### Autologin

Artix:
```
#/etc/conf.d/agetty.tty1

agetty_options="-a yourusername"
```   

Arch:
```
#/etc/systemd/system/getty.target.wants/getty@tty1.service

ExecStart=-/usr/bin/agetty --noreset --autologin yourusername --noclear - ${TERM}
```

Also need add
```bash
echo -n ""
```
in `.bash_profile` in my case [WHY?]


### sudo without password


To allow all members of the `wheel` group to use `sudo` without a password, edit `/etc/sudoers` using:
`EDITOR=vim visudo` and uncomment this line:
```
%wheel ALL=(ALL:ALL) NOPASSWD: ALL
```

If your installer or manual setup created a user-specific file like `/etc/sudoers.d/00_user` with:
```
user ALL=(ALL) ALL
```
this can conflict with the wheel group NOPASSWD rule and override your intended permissions. If your user is in the `wheel` group and the group rule above is present, you should delete the user-specific file to avoid conflicts.

Make sure your user is in the `wheel` group!
```bash
$ groups
holmen1 wheel
```

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

### Clone dotfiles

```
mkdir repos && cd repos
git clone https://github.com/holmen1/dotfiles.git
```

### Build from Source
For software not available via package managers (see [`install/build/`](./build/README.md)):


### startx
Enter tty via C-A-F2 or disable display manager (Arch: lightdm, FreeBSD: not applicable)  
```$ sudo systemctl disable lightdm```  *(Arch only)*

Start xmonad with: ```startx```  
or something else, like  
start xfce: ```startx /usr/bin/startxfce4```

leave x session
```
$ pkill x 
```

### Enable SSH Server (remote login)

1. Install and start the SSH server:
   ```bash
   # Arch
   sudo pacman -S openssh
   sudo systemctl enable sshd && sudo systemctl start sshd
   # FreeBSD
   sudo sysrc sshd_enable=YES && sudo service sshd start
   ```

2. Check SSH server status and logs:
   ```bash
   sudo systemctl status sshd
   sudo journalctl -u sshd
   # Or view recent connection attempts:
   sudo grep sshd /var/log/auth.log
   ```

3. Allow SSH through the firewall (if using UFW):
   ```bash
   sudo pacman -S ufw
   sudo ufw allow ssh
   sudo ufw enable
   sudo ufw status
   ```
   *(If using another firewall, open port 22/TCP.)*

4. Connect from another machine:
   ```bash
   ssh username@<your_ip_address>
   ```

> Tip:* For extra security, use SSH keys and disable password login in `/etc/ssh/sshd_config`.


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

See [LESSONS_LEARNED.md](LESSONS_LEARNED.md) for lessons learned.


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

### Keyboard Shortcuts — TTY & X Session Access

#### Linux (Arch/Artix)
- `Ctrl+Alt+F2` through `Ctrl+Alt+F6` — Switch to TTY console
- `Ctrl+Alt+F7` — Return to GUI/X session
- `Ctrl+Alt+T` — Open terminal (desktop environment dependent)

#### FreeBSD (Xlibre) — Black Screen Recovery
- `Alt+F1`, `Alt+F2`, etc. — Cycle virtual consoles (if keyboard responds)
- `Scroll Lock` then arrow keys — Alternative TTY switching
- `Ctrl+Alt+Delete` or power cycle — Last resort reboot

Once at TTY:
```bash
ps aux | grep X           # Check if X is running
pkill -9 X                # Kill stuck Xlibre
startx                    # Restart X session
```

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

Find and execute a command on each result:
```
find /path/to/search -name "*.txt" -exec grep "search term" {} \;
```


### X11

Debug key bindings:
```sh
xbindkeys -k          # Press key to see keycode
xev                   # Show all X events (key press, window focus, etc.)
```

Debug windows:
```sh
xprop                 # Click window to see properties
xwininfo              # Click window to see geometry and ID
xdotool search --name "window title"  # Find window by name
```

