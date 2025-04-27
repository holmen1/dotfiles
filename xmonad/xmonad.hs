import XMonad
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.FadeWindows
import XMonad.Actions.CycleWS
import XMonad.Hooks.EwmhDesktops (ewmhFullscreen, ewmh)

myModMask       = mod4Mask -- Rebind Mod to the Super key
myTerminal      = "kitty"
myBrowser       = "brave-browser"
myAppLauncher   = "dmenu_run"
myMagenta       = "#FF00FF"
myCyan          = "#00FFFF"

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) toggleStrutsKey
     $ myConfig
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)

myConfig = def
    { modMask    = myModMask,
      terminal   = myTerminal,
      workspaces = myWorkspaces,
      layoutHook = myLayoutHook,
      logHook = fadeWindowsLogHook myFadeHook,
      focusedBorderColor = myMagenta
    }
  `additionalKeys`
    [ ((myModMask, xK_a                   ), spawn myAppLauncher)
    , ((myModMask, xK_f                   ), spawn myBrowser)
    , ((myModMask .|. shiftMask, xK_Return), spawn myTerminal)
    , ((myModMask, xK_c                   ), kill)
    , ((0, xK_F9 ), spawn "brightnessctl set 10%-")  -- Decrease brightness
    , ((0, xK_F10), spawn "brightnessctl set +10%")  -- Increase brightness
    , ((myModMask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    , ((myModMask, xK_Tab), nextWS)  -- Cycle to the next workspace
    -- , ((myModMask .|. shiftMask, xK_s), unGrab *> spawn "scrot -s ~/Downloads/screenshot_%Y-%m-%d_%H-%M-%S.png")
    ]
myWorkspaces = map show [1..5]
myLayoutHook = tiled ||| Mirror tiled ||| Full
  where
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 5/8    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes
myFadeHook = composeAll [ opaque, isUnfocused --> transparency 0.8 ]

myXmobarPP :: PP
myXmobarPP = def
    { ppSep   = xmobarColor myMagenta "" $ ppSep def -- Apply magenta to the default separators
     
     , ppTitleSanitize   = xmobarStrip
     , ppCurrent         = xmobarColor myCyan ""
    , ppOrder = \[ws, l, wins] -> [l, ws, wins]
    }
