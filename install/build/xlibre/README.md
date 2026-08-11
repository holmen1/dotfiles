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
startx -- -keeptty
```
If you are testing the server directly, `X :1 vt8 -keeptty` is the same idea without `startx`.

### FreeBSD: first startx only works with `-keeptty`
On FreeBSD, XLibre can start cleanly but not receive input on the first `startx` attempt unless it keeps the controlling tty. Without `-keeptty`, libseat is disabled and seat ownership may not settle until a second launch, which looks like "run startx twice".

Use `startx -- -keeptty` so X keeps the tty and libseat can hand input devices to the server correctly from the first launch.
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

If `/etc/X11` is empty, that is fine on FreeBSD. XLibre still reads system snippets from `/usr/local/share/X11/xorg.conf.d/` and local overrides from `/usr/local/etc/X11/xorg.conf.d/`. If there are any `.conf` files under `/usr/local/share/X11/xorg.conf.d/` that mention GPU drivers, remove or rename the conflicting ones first; those are part of the active config search path and can override your intent.

If you found NVIDIA snippets there but the logs never mention `nvidia`, they are probably not in play for this machine. The current logs only show Intel/modesetting/scfb/vesa, so those NVIDIA files are likely harmless leftovers rather than the cause.


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

**Why `-keeptty` matters here:** the first launch starts XLibre, but without keeping the controlling tty FreeBSD can leave libseat inactive during that session, so input does not arrive until a second `startx`. Passing `startx -- -keeptty` keeps the tty attached and lets XLibre claim the seat on the first try.

**How this showed up in the logs:** the first log stops right after XLibre initializes, then shows `seat-libseat: libseat integration requires -keeptty`, followed by a GPU re-probe and a segfault while udev is still reshuffling `/dev/dri/card0`. The second log is the “already warmed up” path: X gets through device setup cleanly and stays up long enough to hand control to the session.

**Why it worked before the backlight changes:** enabling ACPI/backlight support changed the kernel and device mix enough to expose the timing bug. With the older setup, XLibre likely stayed on the simpler path and never hit the bad first-launch seat/DRM rebind window; after the backlight tweak, the first launch became sensitive to the exact tty/seat handoff, so it started needing the second try.
**Backlight setup verdict:** the FreeBSD backlight changes are sound. `acpi_video_load="YES"` is the right kind of change for brightness keys, and it is separate from XLibre itself. You do not need the whole `install/profiles/bsdinstall/packages/besk` package set for backlight; most of it is normal desktop tooling. For graphics you only need the X stack pieces (`drm-kmod` plus the XLibre server, and `xlibre-xf86-video-intel` if you want the Intel DDX).

**About `10-device.conf`:** it is not required for brightness. It only forces XLibre to use the Intel driver on this machine. The logs show XLibre can discover `/dev/dri/card0` and the Intel GPU by itself, so the file is a preference/override, not a backlight fix. Keep it only if you want to pin the Intel driver; otherwise XLibre should be able to auto-configure from the hardware.

Fun fact: pinning `modesetting` failed, which is a good sign that the Intel DDX is the safer choice here.

New `-keeptty` logs show the seat problem is no longer the blocker. The first run now dies later with `modeset(1): drmSetMaster failed: Device busy` and `AddScreen/ScreenInit failed for driver 1`. That means XLibre is successfully getting far enough to initialize the Intel screen, but then a second autoconfigured driver (`modesetting`) tries to take DRM master on the same card and loses.

So the remaining issue is driver collision during XLibre autoconfig, not the backlight change. `10-device.conf` only nudges the Intel driver; it does not stop XLibre from also probing modesetting/scfb/vesa. The backlight setup itself is still sound.

You removing `10-device.conf` makes sense: the behavior is the same either way, so it was not the cause. The wallpaper flash before the crash is also a useful clue — XLibre is getting far enough to set the framebuffer and briefly present the root window, then dying after the screen/DRM handoff. That matches a late driver/DRM-master failure, not a pure backlight or font problem.

**Next step:** stop XLibre from autoloading extra GPU paths and retest with one driver only. The log already shows Intel is viable and `modesetting` is the one losing DRM master, so the cleanest experiment is a temporary config that disables GPU auto-add and leaves only the Intel path. If that boots, the bug is confirmed as multi-driver autoconfig; if it still fails, the remaining candidate is the Intel DDX itself or FreeBSD's DRM handoff.

This is not just you: similar `drmSetMaster failed: Device or resource busy` reports exist upstream in XLibre (`X11Libre/xserver` issues #1565 and #3332), and there is also a FreeBSD/libseat-related thread (#1753) about drivers breaking under seatd.

**Workaround split:** `startx -- -keeptty` and the temporary `ServerFlags` tweak solve different layers. `-keeptty` keeps the tty/seat path sane; `ServerFlags` is only for GPU probing/collision. If `ServerFlags` does not change behavior on this box, drop it. If `-keeptty` is required for the first session to even reach the screen, keep it.
