# XLibre


## Build and installation Procedure

### Artix

XLibre is available as a binary in the Artix **world** repo — use `pacman`, not AUR/yay:
```
sudo pacman -S xlibre-xserver xlibre-input-libinput
```
- check there are no more xorg packages that need to be replaced:
   pacman -Q | grep 'xorg-server\|xf86-'

### FreeBSD
```
sudo pkg install xlibre [xlibre-xf86-video-intel]
```
---
## TROUBLESHOOTING

**Remove files created during debugging:**
```bash
sudo rm -f /etc/X11/Xwrapper.config   # Debian convention, has no effect on Artix/xlibre
```

**Stale X locks (clean after every failed startx):**
```bash
rm -f /tmp/.X*-lock /tmp/.X11-unix/X*
```
### startx freeze — xterm appears but system unresponsive (Artix OpenRC, AMD Radeon 780M, ThinkPad) SOLVED

**Setup:** `xorg-xinit` + `xterm` + `xlibre-xserver` + `xlibre-input-libinput` + `xlibre-video-amdgpu`; `~/.xinitrc` = `exec xterm`; ran `startx` — xterm window appeared, could TTY-switch with `Ctrl+Alt+F2` (so kernel input alive), but X appeared frozen.

**Confirmed NOT the cause:**
- Video driver: modesetting and amdgpu both work; glamoregl + OpenGL 4.6 initialized fine.
- Input driver: all devices registered correctly — keyboard (AT Translated Set 2), touchpad (ELAN0688), TrackPoint (TPPS/2), ThinkPad Extra Buttons all added to XINPUT. Adding user to `input` group made no difference.
- udev service: `rc-service udev status` → started. Not the issue.
- Fonts: empty FontPath (missing `fonts.dir`) was a red herring — installing `xorg-fonts-misc` + `xorg-mkfontscale` fixed font errors but freeze persists.

**Harmless errors in log (not the cause):**
- `EE Failed to load module "ati"` — no xlibre-video-ati, not needed
- `EE Failed to load module "fbdev"` / `"vesa"` — fallback drivers, not needed
- `EE open /dev/dri/card0: No such file or directory` — AMD GPU is on card1, modesetting probes card0 first and fails gracefully; amdgpu uses card1 correctly

**Confirmed actually needed:**
- `xorg-fonts-misc` — FontPath was genuinely empty; xterm requires the `fixed` bitmap font
- `xorg-mkfontscale` — provides `mkfontdir` if needed to rebuild `fonts.dir`

**Confirmed bullshit / wasted time:**
- Adding user to `input` group — unnecessary, elogind handles device access
- `rc-update add udev default` — udev was already running
- `rc-service eudev` — service name is `udev` on Artix, and it was already running
- Xwrapper.config `needs_root_rights=yes` — Debian convention, likely ignored by xlibre on Artix
- `sx` — not in Artix repos
- Multiple stale X lock/socket removals — symptom of repeated failed startx attempts, not a cause

**Status: UNSOLVED (paused)**

Core symptom: cursor does NOT move despite all input devices registered in XINPUT with `paused 0` (active). X log is clean. Video, fonts, input enumeration all correct. Mouse/keyboard unresponsive in X.

**Most likely remaining cause:**
`startx` from a TTY shell on Artix OpenRC + elogind does not properly transfer device seat ownership. Elogind shows it handed fds to X (`paused 0`), but events never flow. This is a known OpenRC + elogind + startx interaction issue.

**Leads to investigate next session:**
1. Does bare `X :1 vt2 -keeptty` (not startx) give a movable cursor?
2. Install xmonad and run full WM — does input work there?
3. Check `loginctl show-session self` before vs after startx — does `Active` stay `yes`?
4. Try `ly` or `emptty` display manager as the proper fix for elogind session activation

---

###  First startx run fail (FreeBSD) 2026-8 

When enabling backlight via acpi

Installed: drm-kmod and xlibre-xf86-video-intel

`/boot/loader.conf`: acpi_video_load="YES"

`/usr/local/etc/X11/xorg.conf.d/`
[10-device.conf](10-device.conf)


fastfetch:
```
Host: 20G9S00N00 (ThinkPad 11e 3rd Gen)
Kernel: FreeBSD 15.1-RELEASE-p2
Uptime: 35 mins
Shell: nvim
Display (BOE0608): 1366x768 in 12", 60 Hz [Built-in]
WM: dwm (XLibre)
Terminal: nvim
CPU: Intel(R) Celeron(R) N3150 (4) @ 1.60 GHz
GPU: Intel Atom/Celeron/Pentium Processor x5-E8000/J3xxx/N3xxx Integrated Graphics Controller [Integrated]
Memory: 649.44 MiB / 7.78 GiB (8%)
Swap: 0 B / 2.00 GiB (0%)
Disk (/): 5.78 GiB / 112.27 GiB (5%) - zfs
Disk (/zroot): 96.00 KiB / 106.49 GiB (0%) - zfs
Local IP (wlan0): 192.168.1.114/24
Battery (LNV-00HW043): 35% (1 hour, 19 mins remaining) [Discharging]
Locale: C.UTF-8
```

Logs:
[Xorg.0.first.log](Xorg.0.first.log)
[Xorg.0.second.log](Xorg.0.second.log)


