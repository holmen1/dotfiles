import XMonad
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Operations (unGrab)
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.EwmhDesktops (ewmhFullscreen, ewmh)
import XMonad.Actions.CycleWS
import qualified XMonad.StackSet as W

myModMask       = mod4Mask -- Rebind Mod to the Super key
myFileManager   = "thunar"
myTerminal      = "st"
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
    ([((myModMask, xK_a                   ), spawn myAppLauncher)
    , ((myModMask, xK_w                   ), spawn myBrowser)
    , ((myModMask, xK_f                   ), spawn "firefox")
    , ((myModMask, xK_e                   ), spawn myFileManager)
    , ((myModMask, xK_Return              ), spawn myTerminal)
    , ((myModMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((myModMask, xK_c                   ), kill)
    , ((myModMask, xK_Tab                 ), nextWS)  -- Cycle to the next workspace
    , ((myModMask, xK_s                   ), spawn "scrot ~/Downloads/screenshot_%Y-%m-%d_%H-%M-%S.png")
    , ((myModMask .|. shiftMask, xK_s     ), unGrab >> spawn "scrot -s ~/Downloads/screenshot_%Y-%m-%d_%H-%M-%S.png")
    ]
    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N and follow it
    [((m .|. myModMask, k), windows $ f i)
        | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (\i -> \w -> W.greedyView i (W.shift i w), shiftMask)]
    ])

myWorkspaces = map show [1..6]
myLayoutHook = tiled ||| Mirror tiled ||| Full
  where
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 5/8    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes
myFadeHook = composeAll [ opaque, isUnfocused --> transparency 0.8 ]
