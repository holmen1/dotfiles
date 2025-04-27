# XMonad Configuration

This repository contains a custom configuration for the [XMonad](https://xmonad.org/) tiling window manager, designed to enhance productivity and aesthetics. Below, you'll find details about the setup and key features.

## Features

- **Custom Layouts**: Includes `Tall`, `Mirror Tall`, `Full`, and `ThreeColumn` layouts with magnification.
- **Integrated Status Bar**: Uses `xmobar` with customized appearance and information display.
- **Key Bindings**: Configured for launching applications, locking the screen, and taking screenshots.
- **Super Key (Mod)**: Rebinds the modifier key to the Super key (`Windows` key) for easier access.

## Requirements

- [XMonad](https://xmonad.org/)
- [Xmobar](https://xmobar.org/)
- [Kitty Terminal](https://sw.kovidgoyal.net/kitty/)
- [xscreensaver](https://www.jwz.org/xscreensaver/)
- [scrot](https://github.com/resurrecting-open-source-projects/scrot) (for screenshots)
- [Firefox](https://www.mozilla.org/en-US/firefox/new/) (as the default browser)


## Default Key Bindings

XMonad comes with default key bindings, which can be extended or overridden. Below are the default key bindings:

| Key Combination       | Action                                 |
|------------------------|----------------------------------------|
| `Mod + Shift + Enter`  | Launch terminal                       |
| `Mod + P`              | Launch dmenu                          |
| `Mod + Shift + P`      | Launch gmrun                          |
| `Mod + Shift + C`      | Close the focused window              |
| `Mod + Space`          | Rotate through available layouts      |
| `Mod + Shift + Space`  | Reset the layout to default           |
| `Mod + N`              | Resize/refresh windows                |
| `Mod + Tab`            | Move focus to the next window         |
| `Mod + Shift + Tab`    | Move focus to the previous window      |
| `Mod + J`              | Move focus to the next window         |
| `Mod + K`              | Move focus to the previous window     |
| `Mod + M`              | Move focus to the master window       |
| `Mod + Return`         | Swap the focused window with master   |
| `Mod + Shift + J`      | Swap the focused window with the next |
| `Mod + Shift + K`      | Swap the focused window with the prev |
| `Mod + H`              | Shrink the master area                |
| `Mod + L`              | Expand the master area                |
| `Mod + T`              | Push window back into tiling          |
| `Mod + ,`              | Increment master windows              |
| `Mod + .`              | Decrement master windows              |
| `Mod + Q`              | Restart XMonad                        |
| `Mod + Shift + Q`      | Quit XMonad                           |

## Key Bindings

| Key Combination       | Action                                                |
|------------------------|-------------------------------------------------------|
| `Mod + Shift + Z`      | Lock the screen using `xscreensaver`.                 |
| `Mod + Ctrl + S`       | Capture a selected area screenshot to `~/Downloads`.  |
| `Mod + F`              | Launch Firefox browser.                               |

## Layouts

- **Tall**: Default layout with a master pane and stack.
- **Mirror Tall**: Horizontal version of the Tall layout.
- **Full**: Single window maximized.
- **ThreeColumn**: A three-column layout with magnification.

## Appearance

The configuration customizes `xmobar` with the following visual elements:

- **Separator**: `•` (magenta color)
- **Focused Window Titles**: Highlighted with magenta borders.
- **Hidden Windows**: Displayed in white or low-white.
- **Urgent Windows**: Highlighted in red with yellow exclamation marks.

## LL
LightDM does not use .xinitrc because it directly starts sessions based on .desktop files in xsessions. The .xinitrc file is only used when starting X sessions manually with startx or xinit.

By creating a custom .desktop file and session script, you ensure that your XMonad setup works seamlessly with LightDM.

