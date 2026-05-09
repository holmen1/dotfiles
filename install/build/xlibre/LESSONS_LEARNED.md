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

**Status: UNSOLVED** — X video and input both initialize correctly per logs, yet xterm is unresponsive.

**Next steps to try:**
- Run `X :1 vt2 -keeptty` bare (no xinitrc) — if cursor moves, X + input work and problem is xterm/xinitrc
- Try `st` or `alacritty` instead of xterm to rule out xterm-specific hang
- Check if xterm hangs on font lookup despite fonts installed: `xterm -fn fixed`