# FreeBSD installation guide

Similar artixinstall, using dwm, less features

## Stack

* Display server: XLibre (X11)
* Window manager: dwm
* Menus: dmenu
* Editor: Neovim
* Terminal: st
* File manager: lf
* Dotfile linking: stow
* Keymaps: setxkbmap / XKB

## Pre-installation

FreeBSD installation files are available at the [FreeBSD](https://www.freebsd.org/where/)
download page


```bash
dd if=FreeBSD-{rel-latest}-RELEASE-amd64-memstick.img of=/dev/da0 bs=1M conv=sync
```
`-memstick.img` contains all of the files needed to install FreeBSD, its source, and the Ports

## Guided installation

Once the system boots from the installation media, 
a text-based installation program named `bsdinstall` starts, chose defaults

### Wifi

Advice: setup here, ensure it work before continue

regdomain: ETSI2, SE  
Install driver  
IPv6 resolver (may not need)

Once the scanning of the wireless networks has been carried out, that information will be added to the file ```/etc/wpa_supplicant.conf```

```bash
network={
        scan_ssid=1 
        ssid="FreeBSD" 
        psk="12345678" 
}
```


### Add user

Add user to groups: wheel video (one at a time)

otherwise my choice, see [bsdinstall_log](../install/profiles/bsdinstall/log/bsdinstall_log)  


After completing the base installation, continue with the [FreeBSD post-install guide](bsd-post-install.md).
