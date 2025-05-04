import XMonad
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.FadeWindows
import XMonad.Actions.CycleWS
import XMonad.Hooks.EwmhDesktops (ewmhFullscreen, ewmh)
-- import XMonad.Util.Ungrab (unGrab) --  0.17.2 in debian broken
import System.Environment (lookupEnv, getExecutablePath)
import System.FilePath (takeDirectory, (</>))

myModMask       = mod4Mask -- Rebind Mod to the Super key
myFileManager   = "thunar"
myTerminal      = "st"
myBrowser       = "brave"
myAppLauncher   = "dmenu_run"
myMagenta       = "#FF00FF"
myCyan          = "#00FFFF"

main :: IO ()
main = do 
    specificKeys <- machineSpecificKeys
    xmonad
      . ewmhFullscreen
      . ewmh
      . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) toggleStrutsKey
      $ myConfig specificKeys
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)

myConfig additionalKeys' = def
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
    , ((myModMask, xK_e                   ), spawn myFileManager)
    , ((myModMask .|. shiftMask, xK_Return), spawn myTerminal)
    , ((myModMask, xK_c                   ), kill)
    , ((myModMask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    , ((myModMask, xK_Tab), nextWS)  -- Cycle to the next workspace
    -- , ((myModMask .|. shiftMask, xK_s), unGrab *> spawn "scrot -s ~/Downloads/screenshot_%Y-%m-%d_%H-%M-%S.png")
    ] ++ additionalKeys'

-- Define machine-specific keybindings based on HOSTNAME
machineSpecificKeys :: IO [((KeyMask, KeySym), X ())]
machineSpecificKeys = do
    exePath <- getExecutablePath
    let logFilePath = takeDirectory exePath </> "xmonad-debug.log"
    hostname <- lookupEnv "HOSTNAME"
    -- debug log ~/.cache/xmonad/xmonad-debug.log
    appendFile logFilePath $ case hostname of
        Nothing -> "Error: HOSTNAME not found\n"
        Just h  -> "Hostname: " ++ h ++ "\n"
    return $ case hostname of
        Just "xps" -> [ ((0, xK_F6), spawn "brightnessctl set 10%-")
                      , ((0, xK_F7), spawn "brightnessctl set +10%")
                      ]
        Just "hp"  -> [ ((0, xK_F9), spawn "brightnessctl set 10%-")
                      , ((0, xK_F10), spawn "brightnessctl set +10%")
                      ]
        Just "x"  ->  [ ((0, xK_F5), spawn "brightnessctl set 10%-")
                      , ((0, xK_F6), spawn "brightnessctl set +10%")
                      ] -- ThinkPad 11e
        _ -> []

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
    { ppSep   = xmobarColor myMagenta "" $ ppSep def
     , ppTitleSanitize   = xmobarStrip
     , ppCurrent         = xmobarColor myCyan ""
     , ppOrder = \[ws, l, wins] -> [l, ws, wins]
    }
