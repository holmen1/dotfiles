import XMonad
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.EwmhDesktops (ewmhFullscreen, ewmh)
import XMonad.Actions.CycleWS
import qualified XMonad.StackSet as W

myModMask       = mod4Mask -- Rebind Mod to the Super key
myFileManager   = "thunar"
myTerminal      = "kitty"
myBrowser       = "brave"
myAppLauncher   = "dmenu_run"
myMagenta       = "#FF00FF"
myCyan          = "#00FFFF"

main :: IO ()
main = xmonad
      . ewmhFullscreen
      . ewmh
      $ myConfig

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
    , ((myModMask, xK_w                   ), spawn myBrowser)
    , ((myModMask, xK_f                   ), spawn "firefox")
    , ((myModMask, xK_e                   ), spawn myFileManager)
    , ((myModMask .|. shiftMask, xK_Return), spawn myTerminal)
    , ((myModMask, xK_c                   ), kill)
    , ((myModMask, xK_Tab                 ), nextWS)  -- Cycle to the next workspace
    , ((myModMask, xK_s                   ), spawn "scrot ~/Downloads/screenshot_%Y-%m-%d_%H-%M-%S.png")
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
