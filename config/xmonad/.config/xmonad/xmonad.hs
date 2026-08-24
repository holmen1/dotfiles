import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.FadeWindows
import XMonad.Operations (unGrab)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.SpawnOnce

myModMask :: KeyMask
myModMask = mod1Mask

myAppLauncher :: String
myAppLauncher = "dmenu_run -fn 'Liberation Mono-16' -nb '#222222' -nf '#bbbbbb' -sb '#A300A3' -sf '#eeeeee'"

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1 .. 4 :: Int]

myLayout :: Choose Tall (Choose (Mirror Tall) Full) a
myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 11 / 20
    delta = 3 / 100

noBorderWidth :: Dimension
noBorderWidth = 0

myStartupHook :: String -> X ()
myStartupHook terminal = do
  spawnOnce terminal -- Start terminal on first launch only

myFadeHook :: FadeHook
myFadeHook =
  composeAll
    [ opaque,
      isUnfocused --> transparency 0.6
    ]

myKeys :: [Char] -> String -> [((KeyMask, KeySym), X ())]
myKeys terminal browser =
  [ ((myModMask, xK_a), spawn myAppLauncher),
    ((myModMask, xK_e), spawn $ terminal ++ " lf"),
    ((myModMask, xK_p), spawn $ terminal ++ " htop"),
    ((myModMask, xK_Return), spawn terminal),
    ((myModMask, xK_w), spawn browser),
    ((myModMask .|. shiftMask, xK_Return), windows W.swapMaster),
    ((myModMask, xK_Tab), nextWS),
    ((myModMask .|. shiftMask, xK_Tab), prevWS),
    ((myModMask, xK_q), kill),
    -- screenshots
    ((myModMask, xK_s), spawn "scrot ~/Downloads/screenshot_%Y-%m-%d_%H-%M-%S.png"),
    ((myModMask .|. shiftMask, xK_s), unGrab >> spawn "scrot -s ~/Downloads/screenshot_%Y-%m-%d_%H-%M-%S.png"),
    -- scripts
    ((myModMask, xK_x), spawn "xkb-toggle"),
    -- dmenu scripts
    ((myModMask, xK_m), spawn "dmenu-menu")
  ]
    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N and follow it
    [ ((m .|. myModMask, k), windows $ f i)
      | (i, k) <- zip myWorkspaces [xK_1 .. xK_9],
        (f, m) <- [(W.greedyView, 0), (\i w -> W.greedyView i (W.shift i w), shiftMask)]
    ]

myConfig :: String -> String -> XConfig (Choose Tall (Choose (Mirror Tall) Full))
myConfig terminal browser =
  def
    { terminal = terminal,
      workspaces = myWorkspaces,
      borderWidth = noBorderWidth,
      layoutHook = myLayout,
      startupHook = myStartupHook terminal,
      logHook = fadeWindowsLogHook myFadeHook
    }
    `additionalKeys` myKeys terminal browser

main :: IO ()
main = do
  -- Read terminal and browser from environment variables with fallbacks
  myTerminal <- fromMaybe "xterm" <$> lookupEnv "TERMINAL"
  myBrowser <- fromMaybe "firefox" <$> lookupEnv "BROWSER"
  xmonad $ myConfig myTerminal myBrowser
