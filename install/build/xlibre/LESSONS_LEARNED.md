# LESSONS LEARNED

## XLibre on Artix Linux

XLibre is available as a binary in the Artix **world** repo — use `pacman`, not AUR/yay:
```
sudo pacman -S xlibre-xserver xlibre-input-libinput
```

---

## startx freeze — xterm starts but everything locks (Artix, fresh install)

**Test:** `xorg-xinit` + `xterm` + `xlibre-xserver` + `xlibre-input-libinput` installed; `~/.xinitrc` = `exec xterm`; ran `startx` — xterm window appeared but system froze, had to switch to TTY with `Ctrl+Alt+F2`.

**Confirmed not the cause:** `xlibre-input-libinput` was installed.

**Likely cause:** missing `mesa` or `mesa-utils`, or xlibre rendering via software fallback causing a hang on this Intel GPU. Could also be a missing `xf86-video-*` or `xlibre-video-*` driver for the specific GPU.

**Diagnose:**
1. Check TTY response time after freeze — instant = X alive but input dead (driver issue); slow = deeper freeze
2. Find the log:
   ```
   ls -lt /var/log/Xorg* ~/.local/share/xorg/Xorg* 2>/dev/null
   ```
3. Run with explicit log:
   ```
   startx -- -logfile /tmp/xorg.log -logverbose 6
   ```
   Then TTY out and read `/tmp/xorg.log`.
4. Verify input driver is installed:
   ```
   pacman -Q | grep xlibre-input
   ```