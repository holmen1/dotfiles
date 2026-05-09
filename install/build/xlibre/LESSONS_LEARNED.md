# LESSONS LEARNED

## XLibre on Artix Linux

XLibre is available as a binary in the Artix **world** repo — use `pacman`, not AUR/yay:
```
sudo pacman -S xlibre-xserver xlibre-input-libinput
```

---

## startx freeze — xterm starts but input dead (Artix OpenRC, AMD Radeon 780M)

**Setup:** `xorg-xinit` + `xterm` + `xlibre-xserver` + `xlibre-input-libinput` + `xlibre-video-amdgpu`; `~/.xinitrc` = `exec xterm`; ran `startx` — xterm window appeared but keyboard/mouse dead; could TTY-switch with `Ctrl+Alt+F2`.

**Confirmed not the cause:**
- `xlibre-video-amdgpu` not installed (Xorg.0.log): modesetting driver used, still froze.
- `xlibre-video-amdgpu` installed (Xorg.1.log): amdgpu driver used, glamoregl + OpenGL 4.6 working, still froze.
- → Video driver is NOT the issue. X server renders fine. Freeze is **input-only**.

**Root cause hypothesis:** `eudev` service not running → udev device enumeration fails → no input devices added to X.

Both logs show:
```
(II) The server relies on udev to provide the list of input devices.
     If no devices become available, reconfigure udev or disable AutoAddDevices.
```

On Artix OpenRC, eudev must be running for X to detect `/dev/input` devices.

**Diagnose:**
```bash
rc-service eudev status
grep -i "input\|libinput\|device" ~/.local/share/xorg/Xorg.1.log | head -40
libinput list-devices
```

**Fix if eudev not running:**
```bash
rc-update add eudev default
rc-service eudev start
startx
```