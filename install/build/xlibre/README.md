# XLibre Build Factory

This repository provides a streamlined approach for building and distributing XLibre binaries.


## Build and installation Procedure
### Build and install xlibre-xserver-bootstrap

```bash
git clone https://aur.archlinux.org/xlibre-xserver.git
cd xlibre-server
makepkg -si
```

### Build and install xlibre-input-libinput
```bash
git clone https://aur.archlinux.org/xlibre-input-libinput.git
cd xlibre-input-libinput
makepkg -si
```
### Build and install xlibre-xserver and confirm pacman's request to remove xlibre-xserver-bootstap
```
diff --git a/PKGBUILD b/PKGBUILD
index de2ee40..f9412b5 100644
--- a/PKGBUILD
+++ b/PKGBUILD
@@ -1,7 +1,7 @@
 # Maintainer:  Vitalii Kuzhdin <vitaliikuzhdin@gmail.com>

 pkgbase="xlibre-server"
-pkgname=("${pkgbase}"{,-bootstrap,-common,-devel,-xephyr,-xnest,-xvfb})
+pkgname=("${pkgbase}"{,-common,-devel,-xephyr,-xnest,-xvfb})
```

```bash
makepkg -si
```

### Build and install all xlibre packages of which the corresponding xorg one has been removed
This is done with the same procedure as in the previous steps.
These packages and others might have to be installed when building:
- xlibre-xf86-input-wacom: gobject-introspection
- xlibre-xf86-video-qxl: spice-protocol libcacard spice
- xlibre-xf86-video-intel: libxvmc
 

Perform checks
--------------

- check there are no more xorg packages that need to be replaced:
   pacman -Q | grep 'xorg-server\|xf86-'
- check that all required xlibre packages are installed:
   pacman -Q | grep 'xlibre-'
- make SURE xlibre-xf86-input-libinput-1.5.0.1 or higher is installed!

