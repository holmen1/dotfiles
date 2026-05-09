# LESSONS LEARNED

## XLibre on Artix Linux

XLibre is available as a binary in the Artix **world** repo — use `pacman`, not AUR/yay:
```
sudo pacman -S xlibre-xserver xlibre-input-libinput
```

---

## startx freeze — xterm appears but system unresponsive (Artix OpenRC, AMD Radeon 780M, ThinkPad)

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

## Cleanup — undo debugging residue on gadsden

**Remove unnecessary packages:**
```bash
sudo pacman -Rns xorg-mkfontscale   # only needed if fonts.dir is corrupt; not needed normally
sudo pacman -Rns xterm              # replace with st or alacritty eventually
```
Keep: `xorg-fonts-misc` (needed for bitmap font support)

**Remove from groups:**
```bash
gpasswd -d holmen1 input   # unnecessary, elogind handles device access
# tty group: check if added, remove if so:
gpasswd -d holmen1 tty
```

**Remove files created during debugging:**
```bash
sudo rm -f /etc/X11/Xwrapper.config   # Debian convention, has no effect on Artix/xlibre
```

**Stale X locks (clean after every failed startx):**
```bash
rm -f /tmp/.X*-lock /tmp/.X11-unix/X*
```