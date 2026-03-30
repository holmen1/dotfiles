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
  [ ((modMask, xK_a), spawn myAppLauncher),
    ((modMask, xK_e), spawn $ terminal ++ " -e lf"),
    ((modMask, xK_Return), spawn terminal),
    ((modMask .|. shiftMask, xK_Return), windows W.swapMaster),
    ((modMask, xK_q), kill),
    -- screenshots
    ((modMask, xK_s), spawn "scrot ~/Downloads/screenshot_%Y-%m-%d_%H-%M-%S.png"),
    ((modMask .|. shiftMask, xK_s), unGrab >> spawn "scrot -s ~/Downloads/screenshot_%Y-%m-%d_%H-%M-%S.png"),
    -- dmenu scripts
    ((modMask, xK_x), spawn "~/repos/dotfiles/scripts/dmenu-logout.sh"),
    ((modMask, xK_v), spawn "~/repos/dotfiles/scripts/dmenu-mullvad.sh"),
    ((modMask, xK_m), spawn "~/repos/dotfiles/scripts/dmenu-help.sh"),
    ((modMask, xK_w), namedScratchpadAction (myScratchpads terminal browser) "browser"),
    ((modMask, xK_p), namedScratchpadAction (myScratchpads terminal browser) "htop")
  ]

myConfig terminal browser =
  def
    { terminal = terminal,
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
