# XLibre from AUR

Package Base Details, see [AUR](https://aur.archlinux.org/pkgbase/xlibre-xserver)

## Build and installation Procedure
### Build and install xlibre-xserver-bootstrap

```bash
yay -S xlibre-xserver-bootstrap
```

### Build and install xlibre-input-libinput
```bash
yay -S xlibre-input-libinput
```
### Build and install xlibre-xserver
```bash
yay -S xlibre-xserver
```
and confirm pacman's request to remove xlibre-xserver-bootstap


 

Perform checks
--------------

- check there are no more xorg packages that need to be replaced:
   pacman -Q | grep 'xorg-server\|xf86-'
- check that all required xlibre packages are installed:
   pacman -Q | grep 'xlibre-'
- make SURE xlibre-xf86-input-libinput-1.5.0.1 or higher is installed!

