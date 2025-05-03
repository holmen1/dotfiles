# XMonad Configuration

xmonad is a tiling window manager for X. Windows are arranged automatically to tile the screen without gaps or overlap, maximizing screen use, here configured with xmobar to provide a status bar  
This repository contains a custom configuration for the [XMonad](https://xmonad.org/) tiling window manager, designed to enhance productivity and aesthetics. Below, you'll find details about the setup and key features



## Shortcuts

| Key Combination       | Action                                 |
|------------------------|----------------------------------------|
| `Mod + Shift + Enter`  | Launch terminal                       |
| `Mod + A`              | Launch dmenu                          |
| `Mod + C`              | Close the focused window              |
| `Mod + Space`          | Rotate through available layouts      |
| `Mod + Shift + Space`  | Reset the layout to default           |
| `Mod + N`              | Resize/refresh windows                |
| `Mod + Tab`            | Move focus to the next window         |
| `Mod + Shift + Tab`    | Move focus to the previous window      |
| `Mod + J`              | Move focus to the next window         |
| `Mod + K`              | Move focus to the previous window     |
| `Mod + M`              | Move focus to the master window       |
| `Mod + Return`         | Swap the focused window with master   |
| `Mod + H`              | Shrink the master area                |
| `Mod + L`              | Expand the master area                |
| `Mod + T`              | Push window back into tiling          |
| `Mod + ,`              | Increment master windows              |
| `Mod + .`              | Decrement master windows              |
| `Mod + Q`              | Restart XMonad                        |
| `Mod + Shift + Q`      | Quit XMonad                           |
| `Mod + Shift + Z`      | Lock the screen using `xscreensaver`  |
| `Mod + Ctrl + S`       | Capture a selected area screenshot to `~/Downloads` |
| `Mod + F`              | Launch browser                        |


## Requirements

- [Xmobar](https://xmobar.org/)
- [Kitty Terminal](https://sw.kovidgoyal.net/kitty/)
- [Brave](https://brave.com/linux/)
- thunar fileexplorer
- scrot for screenshots
- xscreensaver
- xcompmgr for opacity
- trayer
- feh wallpaper
- xfce4-power-manager


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
