import XMonad
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Operations (unGrab)
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.EwmhDesktops (ewmhFullscreen, ewmh)
import XMonad.Actions.CycleWS
import qualified XMonad.StackSet as W
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import XMonad.Layout.NoBorders
import XMonad.ManageHook
import XMonad.Util.NamedScratchpad

myModMask       = mod4Mask -- Rebind Mod to the Super key
myFileManager   = "thunar"
myAppLauncher   = "dmenu_run"
myMagenta       = "#A300A3"

main :: IO ()
main = do
  -- Read terminal and browser from environment variables with fallbacks
  myTerminal <- fromMaybe "xterm" <$> lookupEnv "TERMINAL"
  myBrowser  <- fromMaybe "firefox" <$> lookupEnv "BROWSER"
  xmonad
    . ewmhFullscreen
    . ewmh
    $ myConfig myTerminal myBrowser

-- Scratchpads
-- Note, find class name by running `xprop` in the terminal and clicking on the window
-- WM_CLASS(STRING) = "brave-browser", "Brave-browser"
myScratchpads browser= [NS "browser" browser (className =? "brave-browser") defaultFloating]

myConfig terminal browser = def
    { 
      manageHook = namedScratchpadManageHook scratchpads,
      modMask    = myModMask,
      terminal   = terminal,
      workspaces = myWorkspaces,
      logHook = fadeWindowsLogHook myFadeHook,
      focusedBorderColor = myMagenta,
      layoutHook = myLayout
    }
  `additionalKeys`
    ([((myModMask, xK_a                   ), spawn myAppLauncher)
    , ((myModMask, xK_w                   ), spawn browser)
    , ((myModMask, xK_e                   ), spawn myFileManager)
    , ((myModMask, xK_Return              ), spawn terminal)
    , ((myModMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((myModMask, xK_q                   ), kill)
    , ((myModMask .|. shiftMask, xK_h     ), sendMessage Shrink) -- Shrink the master area
    , ((myModMask .|. shiftMask, xK_l     ), sendMessage Expand) -- Expand the master area
    , ((myModMask, xK_Tab                 ), nextWS)  -- Cycle to the next workspace
    -- screenshots
    , ((myModMask, xK_s                   ), spawn "scrot ~/Downloads/screenshot_%Y-%m-%d_%H-%M-%S.png")
    , ((myModMask .|. shiftMask, xK_s     ), unGrab >> spawn "scrot -s ~/Downloads/screenshot_%Y-%m-%d_%H-%M-%S.png")
    -- dmenu scripts
    , ((myModMask, xK_x                   ), spawn "~/repos/dotfiles/scripts/dmenu-logout.sh")
    , ((myModMask, xK_m                   ), spawn "~/repos/dotfiles/scripts/dmenu-mullvad.sh")
    , ((myModMask, xK_h                   ), spawn "~/repos/dotfiles/scripts/dmenu-help.sh")
    , ((myModMask, xK_z                   ), namedScratchpadAction scratchpads "browser")
    ]
    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N and follow it
    [((m .|. myModMask, k), windows $ f i)
        | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (\i -> \w -> W.greedyView i (W.shift i w), shiftMask)]
    ])
    where
      scratchpads = myScratchpads browser

myWorkspaces = map show [1..6]
myFadeHook = composeAll [ opaque, isUnfocused --> transparency 0.8 ]

-- Removes the borders from a window under one of the following conditions:
-- There is only one screen and only one window
-- A floating window covers the entire screen
myLayout = smartBorders $ layoutHook def
