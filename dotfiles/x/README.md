# X

X11 session startup via `~/.xinitrc`.

---

## Files

| Path       | Purpose                          |
|------------|----------------------------------|
| `.xinitrc` | X session startup — env, apps    |

---

## Startup sequence

1. Check essential programs (`xmonad`, `xterm`)
2. Set `EDITOR`, `TERMINAL`, `BROWSER`
3. Apply base keyboard layout (`setxkbmap se`)
4. Start `xsetroot`, `feh`, `dunst`, `xcompmgr`, `xbindkeys`
5. `exec xmonad`

---

## Keyboard customisation

Extended key behaviour (CapsLock→Control/Escape, Super→num-layer) lives in
`dotfiles/xkb/` and is applied by running `~/.local/bin/apply-keys.sh` manually.  
See [xkb/README.md](../xkb/README.md) for details.
