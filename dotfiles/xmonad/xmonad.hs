import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.FadeWindows
import XMonad.Layout.NoBorders
import XMonad.ManageHook
import XMonad.Operations (unGrab)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce

myModMask = mod4Mask -- Rebind Mod to the Super key
myAppLauncher = "dmenu_run -fn 'JetBrainsMono Nerd Font:size=14' -nb '#222222' -nf '#bbbbbb' -sb '#A300A3' -sf '#eeeeee'"
myMagenta = "#A300A3"

myWorkspaces = map show [1 .. 4]

-- Scratchpads
-- Note, find class name by running `xprop` in the terminal and clicking on the window
-- WM_CLASS(STRING) = "brave-browser", "Brave-browser"
myScratchpads terminal browser =
  [ NS
      "htop"
      (terminal ++ " -e htop")
      (title =? "htop")
      (customFloating $ W.RationalRect 0.5 0 1 0.4),
    NS
      "browser"
      (browser ++ " --class=MyScratchpad")
      (className =? "MyScratchpad")
      (customFloating $ W.RationalRect 0 0 1 1)
  ]

-- Removes the borders from a window under one of the following conditions:
-- There is only one screen and only one window
-- A floating window covers the entire screen
myLayout = smartBorders $ layoutHook def

myManageHook terminal browser = namedScratchpadManageHook (myScratchpads terminal browser)

myStartupHook terminal = do
  spawnOnce terminal  -- Start terminal on first launch only

myFadeHook =
  composeAll
    [ opaque,
      isUnfocused --> transparency 0.8,
      className =? "brave-browser" --> transparency 0.15
    ]

myKeys terminal browser =
  [ ((myModMask, xK_a), spawn myAppLauncher),
    ((myModMask, xK_e), spawn $ terminal ++ " -e lf"),
    ((myModMask, xK_Return), spawn terminal),
    ((myModMask .|. shiftMask, xK_Return), windows W.swapMaster),
    ((myModMask, xK_q), kill),
    ((myModMask .|. shiftMask, xK_h), sendMessage Shrink), -- Shrink the master area
    ((myModMask .|. shiftMask, xK_l), sendMessage Expand), -- Expand the master area
    ((myModMask, xK_Tab), nextWS), -- Cycle to the next workspace
    -- screenshots
    ((myModMask, xK_s), spawn "scrot ~/Downloads/screenshot_%Y-%m-%d_%H-%M-%S.png"),
    ((myModMask .|. shiftMask, xK_s), unGrab >> spawn "scrot -s ~/Downloads/screenshot_%Y-%m-%d_%H-%M-%S.png"),
    -- dmenu scripts
    ((myModMask, xK_x), spawn "~/repos/dotfiles/scripts/dmenu-logout.sh"),
    ((myModMask, xK_v), spawn "~/repos/dotfiles/scripts/dmenu-mullvad.sh"),
    ((myModMask, xK_m), spawn "~/repos/dotfiles/scripts/dmenu-help.sh"),
    ((myModMask, xK_w), namedScratchpadAction (myScratchpads terminal browser) "browser"),
    ((myModMask, xK_p), namedScratchpadAction (myScratchpads terminal browser) "htop")
  ]
    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N and follow it
    [ ((m .|. myModMask, k), windows $ f i)
      | (i, k) <- zip myWorkspaces [xK_1 .. xK_9],
        (f, m) <- [(W.greedyView, 0), (\i -> \w -> W.greedyView i (W.shift i w), shiftMask)]
    ]

myConfig terminal browser =
  def
    { modMask = myModMask,
      terminal = terminal,
      workspaces = myWorkspaces,
      focusedBorderColor = myMagenta,
      layoutHook = myLayout,
      manageHook = myManageHook terminal browser,
      startupHook = myStartupHook terminal,
      logHook = fadeWindowsLogHook myFadeHook
    }
    `additionalKeys` myKeys terminal browser

main :: IO ()
main = do
  -- Read terminal and browser from environment variables with fallbacks
  myTerminal <- fromMaybe "xterm" <$> lookupEnv "TERMINAL"
  myBrowser <- fromMaybe "firefox" <$> lookupEnv "BROWSER"
  xmonad
    . ewmhFullscreen
    . ewmh
    $ myConfig myTerminal myBrowser
