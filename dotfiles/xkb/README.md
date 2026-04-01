# XKB

X11 keyboard customisation using **XKB** and **xcape**.

---

## Key behaviour

| Key      | Tap     | Hold        |
|----------|---------|-------------|
| CapsLock | Escape  | Control     |
| Super    | —       | Num-layer   |

### Num-layer (Super held)

```
  u  i  o  →  7  8  9
  j  k  l  →  4  5  6
  m  ,  .  →  1  2  3
     n     →  0
```

---

## Files

| Path                          | Purpose                                |
|-------------------------------|----------------------------------------|
| `.config/xkb/symbols/local`  | XKB partial symbols — num-layer digits |
| `.local/bin/apply-keys.sh`   | Applies XKB + starts xcape             |

---

## Installation (Arch Linux)

```sh
sudo pacman -S xorg-setxkbmap xorg-xkbcomp xcape
```

Symlink the files to `$HOME`, then apply manually:

```sh
~/.local/bin/apply-keys.sh
```

> `~/.xinitrc` applies only `setxkbmap se` on startup. Run `apply-keys.sh`
> manually after login to get the full key behaviour.

---

## How it works

### CapsLock → Control / Escape

`ctrl:nocaps` (built-in XKB option) replaces CapsLock with `Control_L`.  
xcape then synthesises `Escape` when `Control_L` is released without a chord:

```sh
xcape -e 'Control_L=Escape'
```

### Super → Num-layer

`lv3:lwin_switch` (built-in XKB option) makes the left Super key the
**level-3 activator** — the same mechanism as AltGr.  
`~/.config/xkb/symbols/local` assigns digits at level 3 for the numpad cluster.

### How `apply-keys.sh` merges the keymap

`setxkbmap -print` emits the XKB description without applying it:

```
xkb_symbols { include "pc+se+inet(evdev)+ctrl(nocaps)+level3(lwin_switch)+..." };
```

`sed` appends `+local(numpad)` to that include line:

```
xkb_symbols { include "...+level3(lwin_switch)+local(numpad)" };
```

`xkbcomp` compiles the result and loads it into the X server.  
`-I"$HOME/.config/xkb"` tells xkbcomp where to find `symbols/local`.

---

## Debugging

```sh
setxkbmap -query                                   # current options
xkbcomp -xkb "$DISPLAY" - | grep -A4 'key <LWIN>' # check Super mapping
xkbcomp -xkb "$DISPLAY" - | grep -A4 'key <AD07>' # check u/7 mapping
xev | grep -A2 KeyPress                            # watch raw events
```
