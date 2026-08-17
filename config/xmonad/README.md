# XMonad Configuration

xmonad is a tiling window manager for X. Windows are arranged automatically to tile the screen without gaps or overlap, maximizing screen use, here configured with xmobar to provide a status bar  
This repository contains a custom configuration for the [XMonad](https://xmonad.org/) tiling window manager, designed to enhance productivity and aesthetics. Below, you'll find details about the setup and key features



## Shortcuts

| Key Combination        | Action                                |
|------------------------|---------------------------------------|
| `Mod + Enter`          | Launch terminal                       |
| `Mod + Q`              | Close the focused window              |
| `Mod + J`              | Move focus to the next window         |
| `Mod + K`              | Move focus to the previous window     |
| `Mod + Tab`            | Next workspace                        |
| `Mod + Shift + Tab`    | Previous workspace                    |
| `Mod + [1..4]`         | Switch to workspace N                 |
| `Mod + Shift + [1..4]` | Move window to workspace N and follow |
| `Mod + E`              | Launch file manager (lf)              |
| `Mod + B`              | Launch browser                        |
| `Mod + Space`          | Rotate through available layouts      |
| `Mod + H`              | Shrink the master area                |
| `Mod + L`              | Expand the master area                |
| `Mod + ,`              | Increment master windows              |
| `Mod + .`              | Decrement master windows              |
| `Mod + Shift + Return` | Swap the focused window with master   |
| `Mod + S`              | Screenshot to `~/Downloads`           |
| `Mod + Shift + S`      | Screenshot selected area              |
| `Mod + W`              | Browser scratchpad toggle             |
| `Mod + P`              | htop scratchpad toggle                |
| `Mod + A`              | Launch dmenu                          |
| `Mod + Shift + Q`      | Quit xmonad                           |
| `Mod + M`              | dmenu: Help Network Exit              |
| `Mod + X`              | XKB: toggle keyboard layout           |


## Requirements

- [Xmobar](https://xmobar.org/) NOT USING
- [st - simple terminal](https://st.suckless.org/)
- [Brave](https://brave.com/linux/)
- thunar fileexplorer
- scrot for screenshots
- xscreensaver
- xcompmgr for opacity
- feh wallpaper


## Post install

Edit ```link_config.sh```, link configuration
```
$ ~/dotfiles/scripts/link_config.sh ~/repos/dotfiles
```

Disable display manager, startx  

To enable display manager, add .config/xmonadxmonad-session-rc  
Edit xmonad-session like so
```
$ diff /usr/bin/xmonad-session /usr/bin/xmonad-session.bak 
3c3
< if [ -r ".config/xmonad/xmonad-session-rc" ]
---
> if [ -r ".xmonad/xmonad-session-rc" ]
5c5
<   . .config/xmonad/xmonad-session-rc
---
>   . .xmonad/xmonad-session-rc
```


## LL
LightDM does not use .xinitrc because it directly starts sessions based on .desktop files in xsessions. The .xinitrc file is only used when starting X sessions manually with startx or xinit.

By creating a custom .desktop file and session script, you ensure that your XMonad setup works seamlessly with LightDM

## Debug log
```haskell
machineSpecificKeys :: IO [((KeyMask, KeySym), X ())]
machineSpecificKeys = do
    exePath <- getExecutablePath
    let logFilePath = takeDirectory exePath </> "xmonad-debug.log"
    hostname <- lookupEnv "HOSTNAME"
    appendFile logFilePath $ case hostname of
        Nothing -> "Error: HOSTNAME not found\n"
        Just h  -> "Hostname: " ++ h ++ "\n"
...
```
Writes to ```~/.cache/xmonad/xmonad-debug.log```

## TODOs

- [ ] **Screenshot directory safety** (xmonad.hs:60-61) — Hardcoded `~/Downloads` may not exist. Add check in `.xinitrc` startup hook or create directory on demand. Currently screenshots fail silently if directory missing.

- [ ] **Fade hook transparency conflict** (xmonad.hs:45-49) — Brave-browser set to 15% opacity unconditionally, conflicts with `opaque` rule for focused windows. Reorder rules or exclude Brave from opaque rule to clarify behavior.

- [ ] **Workspace keybinding clarity** (xmonad.hs:72-75) — Lambda composition `(\i w -> W.greedyView i (W.shift i w), shiftMask)` works but is non-obvious. Extract into named functions `viewWS` and `shiftWS` for maintainability and to prevent API breakage.

  **How to clean up complex lambdas:**
  
  Current (hard to read):
  ```haskell
  [ ((m .|. myModMask, k), windows $ f i)
    | (i, k) <- zip myWorkspaces [xK_1 .. xK_9],
      (f, m) <- [(W.greedyView, 0), (\i w -> W.greedyView i (W.shift i w), shiftMask)]
  ]
  ```

  Better (extract named functions first):
  ```haskell
  let viewWS i = W.greedyView i
      shiftWS i = W.greedyView i . W.shift i
  in [ ((m .|. myModMask, k), windows $ f i)
       | (i, k) <- zip myWorkspaces [xK_1 .. xK_9],
         (f, m) <- [(viewWS, 0), (shiftWS, shiftMask)]
       ]
  ```

  Benefits:
  - Function intent is explicit (shift moves AND views, view only views)
  - Easier to test each function independently
  - Less susceptible to API changes in XMonad
  - Readable at a glance

- [ ] **XMonad compilation verification** — Sanity checks verify xmonad binary exists but don't test that config compiles or loads. Add integration test that runs `xmonad --version` and verifies fade hook, scratchpads, key bindings respond as expected.

- [ ] **Type annotation** (xmonad.hs:8) — Add explicit type to `myModMask :: KeyMask` for code clarity and early error detection.

- [ ] **Lightweight optimization** — Remove unused/cosmetic features to keep binary thin (trade ~2-3% binary size for fewer runtime hooks):
  - Remove `myMagenta = "#A300A3"` (line 16) — defined but never used
  - Remove `XMonad.Hooks.FadeWindows` (line 6) + `myFadeHook` + `logHook` — transparency is cosmetic, adds ~1-2 KB
  - Remove `XMonad.Util.NamedScratchpad` (line 10) + `myScratchpads` — browser/htop scratchpads; keep if frequently used
  - Remove `XMonad.Hooks.EwmhDesktops` (line 5) — desktop environment integration; only needed if using taskbar/panel
  
  **Keep:** `XMonad.Util.SpawnOnce` (efficient one-time startup for terminal)

---

## Why Export HOSTNAME is Necessary for XMonad
When lookupEnv "HOSTNAME" returns Nothing inside your xmonad.hs (line 54), it means the HOSTNAME environment variable isn't available to XMonad. This happens due to how environment variables are handled in desktop environments.

Environment Variable Inheritance
Environment variables are passed from parent processes to child processes. However, this inheritance chain is affected by how window managers like XMonad are launched:

When Using LightDM
LightDM (and other display managers) start X sessions in a controlled, sanitized environment:

LightDM doesn't automatically import all variables from your shell configuration files (.bashrc, .profile, etc.)
It uses its own configuration files to set up the environment
Variables you define in your shell aren't automatically available to XMonad
When Using startx
With startx, a similar issue occurs:

The X server starts with a minimal environment
Only variables explicitly exported in .xinitrc or session scripts are available to XMonad
Even though HOSTNAME might be set in your shell, it doesn't automatically propagate
Solution
This is why you need to explicitly export HOSTNAME="xps" in:

Your .xinitrc file (for startx)
Your xmonad-session-rc script (for LightDM)
By explicitly exporting the variable in these startup scripts, you ensure it's available in XMonad's environment when lookupEnv "HOSTNAME" is called.

Without this export, the variable doesn't exist in XMonad's process environment, causing lookupEnv to return Nothing.
