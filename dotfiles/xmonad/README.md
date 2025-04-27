# XMonad Configuration

xmonad is a tiling window manager for X. Windows are arranged automatically to tile the screen without gaps or overlap, maximizing screen use, here configured with xmobar to provide a status bar  
This repository contains a custom configuration for the [XMonad](https://xmonad.org/) tiling window manager, designed to enhance productivity and aesthetics. Below, you'll find details about the setup and key features.

## Features

## Requirements

- [XMonad](https://xmonad.org/)
- [Xmobar](https://xmobar.org/)
- [Kitty Terminal](https://sw.kovidgoyal.net/kitty/)
- [xscreensaver](https://www.jwz.org/xscreensaver/)
- [scrot](https://github.com/resurrecting-open-source-projects/scrot) (for screenshots)
- [Firefox](https://www.mozilla.org/en-US/firefox/new/) (as the default browser)


## Debian install
```
$ sudo apt install xmonad libghc-xmonad-dev libghc-xmonad-contrib-dev xmobar
```
Additional packages for wallpaper, system-tray, d-menu
```
$ sudo apt install suckless-tools kitty trayer feh xscreensaver xcompmgr
```

Screen brightness
```
$ sudo apt install brightnessctl 
```

Link configuration
```
$ ./scripts/link_config.sh ~/repos/debian13
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


## Key Bindings

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
| `Mod + Ctrl + S`       | Capture a selected area screenshot to `~/Downloads`.  |
| `Mod + F`              | Launch browser                        |


## LL
LightDM does not use .xinitrc because it directly starts sessions based on .desktop files in xsessions. The .xinitrc file is only used when starting X sessions manually with startx or xinit.

By creating a custom .desktop file and session script, you ensure that your XMonad setup works seamlessly with LightDM.

