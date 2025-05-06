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
Show overall network status
```
nmcli general status
```
Show all network devices
```
nmcli device status
```
Show active connections
```
nmcli connection show --active
```
Scan for available WiFi networks
```
nmcli device wifi list
```
Connect to a WiFi network
```
nmcli device wifi connect SSID password PASSWORD
```
Connect to a saved network
```
nmcli connection up "Network Name"
```
Turn WiFi on/off
```
nmcli radio wifi on
nmcli radio wifi off
```
```
nmcli device wifi connect <SSID> password <password> ifname <ifname>
```

