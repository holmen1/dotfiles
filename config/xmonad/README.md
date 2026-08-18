# XMonad Configuration

xmonad is a tiling window manager for X. Windows are arranged automatically to tile the screen without gaps or overlap, maximizing screen use, here configured without xmobar using custom dmenu instead    
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
| `Mod + E`              | Launch file manager                   |
| `Mod + Space`          | Rotate through available layouts      |
| `Mod + H`              | Shrink the master area                |
| `Mod + L`              | Expand the master area                |
| `Mod + ,`              | Increment master windows              |
| `Mod + .`              | Decrement master windows              |
| `Mod + Shift + Return` | Swap the focused window with master   |
| `Mod + S`              | Screenshot to `~/Downloads`           |
| `Mod + Shift + S`      | Screenshot selected area              |
| `Mod + W`              | Launch browser                        |
| `Mod + P`              | Launch htop                           |
| `Mod + A`              | Launch dmenu                          |
| `Mod + Shift + Q`      | Quit xmonad                           |
| `Mod + M`              | dmenu: Help Network Exit              |
| `Mod + X`              | XKB: toggle keyboard layout           |


## Requirements

- [st - simple terminal](https://st.suckless.org/)
- lf fileexplorer
- scrot for screenshots
- xscreensaver
- xcompmgr for opacity


---

## TODOs

- [ ] **Investigate build warnings**
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

