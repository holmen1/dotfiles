import XMonad
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Operations (unGrab)
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.EwmhDesktops (ewmhFullscreen, ewmh)
import XMonad.Actions.CycleWS
import qualified XMonad.StackSet as W
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

myModMask       = mod4Mask -- Rebind Mod to the Super key
myFileManager   = "thunar"
myAppLauncher   = "dmenu_run"
myMagenta       = "#FF00FF"
myCyan          = "#00FFFF"

main :: IO ()
main = do
  -- Read terminal and browser from environment variables with fallbacks
  myTerminal <- fromMaybe "xterm" <$> lookupEnv "TERMINAL"
  myBrowser  <- fromMaybe "firefox" <$> lookupEnv "BROWSER"
  xmonad
    . ewmhFullscreen
    . ewmh
    $ myConfig myTerminal myBrowser

myConfig terminal browser = def
    { modMask    = myModMask,
      terminal   = terminal,
      workspaces = myWorkspaces,
      logHook = fadeWindowsLogHook myFadeHook,
      focusedBorderColor = myMagenta
    }
  `additionalKeys`
    ([((myModMask, xK_a                   ), spawn myAppLauncher)
    , ((myModMask, xK_w                   ), spawn browser)
    , ((myModMask, xK_e                   ), spawn myFileManager)
    , ((myModMask, xK_Return              ), spawn terminal)
    , ((myModMask, xK_q                   ), spawn "~/repos/dotfiles/scripts/dmenu-logout.sh")
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
myFadeHook = composeAll [ opaque, isUnfocused --> transparency 0.8 ]
