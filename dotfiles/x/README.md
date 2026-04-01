# X

Portable X11 keyboard customisation using **XKB** and **xcape**.  
Works on Arch Linux and FreeBSD (Xlibre/Xorg) without any daemon or root config.

---

## Key behaviour

| Key      | Tap   | Hold                     |
|----------|-------|--------------------------|
| CapsLock | Esc   | Control                  |
| Space    | Space | Num-layer (see below)    |

### Num-layer (Space held)

```
  u  i  o  →  7  8  9
  j  k  l  →  4  5  6
  m  ,  .  →  1  2  3
     n     →  0
```

---

## Files

| Path (relative to `$HOME` after stow)   | Purpose                                 |
|------------------------------------------|-----------------------------------------|
| `.config/xkb/symbols/local`             | XKB partial symbols — num-layer mapping |
| `.local/bin/apply-keys.sh`              | Setup script — applies XKB + xcape      |
| `.xinitrc`                              | X startup script — calls apply-keys.sh  |

Source locations in this repo mirror the stow layout under `dotfiles/x/`.

---

## Installation

### 1. Install dependencies

**Arch Linux**
```sh
sudo pacman -S xorg-setxkbmap xorg-xkbcomp xcape
```

**FreeBSD**
```sh
pkg install xkbcomp xcape   # setxkbmap ships with xlibre/xorg-minimal
```

### 2. Stow the `x` package

```sh
cd ~/repos/dotfiles
stow -d dotfiles -t ~ x
```

This creates the symlinks:
```
~/.config/xkb/symbols/local   → dotfiles/x/.config/xkb/symbols/local
~/.local/bin/apply-keys.sh    → dotfiles/x/.local/bin/apply-keys.sh
~/.xinitrc                    → dotfiles/x/.xinitrc
```

### 3. Start X

```sh
startx          # .xinitrc calls apply-keys.sh automatically
```

To reload without restarting X:

```sh
~/.local/bin/apply-keys.sh
```

---

## How it works

### CapsLock → Control / Escape

```sh
setxkbmap se -option ctrl:nocaps
```

The standard XKB option `ctrl:nocaps` replaces the `CapsLock` key with
`Control_L`.

```sh
xcape -e 'Control_L=Escape'
```

xcape watches for `Control_L` being released without another key having been
pressed in between; when it is, xcape synthesises an `Escape` keystroke.

> **Note:** `ctrl:nocaps` maps CapsLock to the same symbol (`Control_L`) as
> the physical left Ctrl key. xcape therefore also fires `Escape` when the
> real left Ctrl is tapped alone. In practice this is harmless — Ctrl is
> almost always used in combination with another key.

---

### Space → Space / Num-layer

#### XKB symbols — `~/.config/xkb/symbols/local`

The `numspace` section does two things:

1. **Remaps Space to `ISO_Level3_Shift`.**  
   `ISO_Level3_Shift` is the standard XKB level-3 modifier — the same
   mechanism used by AltGr. Holding Space now activates level 3 for all
   keys, exactly like holding AltGr.

2. **Assigns digits at level 3 for the numpad cluster keys.**  
   All other keys keep their existing level-3 symbols (Swedish AltGr
   characters) unchanged.

#### xcape restores tap = space

```sh
xcape -e 'ISO_Level3_Shift=space'
```

When Space is tapped without another key, xcape synthesises a `space`
keystroke, giving back normal space-bar behaviour.

---

### How `apply-keys.sh` merges the keymap

```sh
setxkbmap se -option ctrl:nocaps -print \
    | sed '/xkb_symbols/s|include "\(.*\)"|include "\1+local(numspace)"|' \
    | xkbcomp -w0 -I"$HOME/.config/xkb" - "$DISPLAY"
```

1. `setxkbmap … -print` emits the XKB keymap description without applying it.
2. `sed` appends `+local(numspace)` to the `xkb_symbols` include line, e.g.:
   ```
   include "pc+se+inet(evdev)+ctrl(nocaps)+local(numspace)"
   ```
3. `xkbcomp` compiles the merged keymap and pushes it to the X server.  
   `-I"$HOME/.config/xkb"` tells xkbcomp where to find `symbols/local`.

If the merge fails (symbol file missing, xkbcomp error), the script falls
back to `setxkbmap se -option ctrl:nocaps` so CapsLock remapping still works.

---

## Debugging

```sh
# Show current XKB settings
setxkbmap -query

# Dump the compiled keymap; grep for a key definition
xkbcomp -xkb "$DISPLAY" - | grep -A5 'key <CAPS>'
xkbcomp -xkb "$DISPLAY" - | grep -A5 'key <SPCE>'

# Reload keyboard at any time
~/.local/bin/apply-keys.sh

# Watch raw key events (press keys in the xev window)
xev | grep -A2 'KeyPress'
```

---

## Comparison with keyd

| Feature                  | keyd (Linux-only)    | XKB + xcape (portable) |
|--------------------------|----------------------|------------------------|
| CapsLock → Ctrl / Esc    | ✓                    | ✓                      |
| Space num-layer          | ✓                    | ✓                      |
| FreeBSD support          | ✗                    | ✓                      |
| Requires daemon          | ✓ (systemd service)  | ✗                      |
| Requires root config     | ✓ (`/etc/keyd/`)     | ✗ (user `~/.config/`)  |
| Dependency count         | 1 (keyd)             | 3 (setxkbmap, xkbcomp, xcape) |

