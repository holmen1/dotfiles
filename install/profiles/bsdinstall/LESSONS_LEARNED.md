# LESSONS LEARNED

## xlibre-1.0

Fresh install that work fine.

Brightness dont work using backlight

Tried install both drm-kmod and xlibre-xf86-video-intel, got black screen after startx




## Migrating from Full Font Collections to Individual Fonts

The `nerd-fonts` package contains hundreds of patched fonts and takes a very long time to upgrade (90%+ of `pkg upgrade` time). If you only use specific fonts like JetBrains Mono:

1. **Identify what you actually use:**
   ```bash
   grep -r "JetBrainsMono" ~/.config/ ~/.xmonad/  # Search your dotfiles
   ```

2. **Replace with individual packages:**
   ```bash
   sudo pkg delete nerd-fonts
   sudo pkg install jetbrains-mono-nerd-font
   ```

3. **Update your package list:**
   Replace `x11-fonts/nerd-fonts` with `x11-fonts/jetbrains-mono-nerd-font` in your `pkglist.txt`.

**Result:** `pkg upgrade` becomes dramatically faster since it only updates one font instead of hundreds.
