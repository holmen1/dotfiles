import XMonad
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.FadeWindows
import XMonad.Actions.CycleWS
import XMonad.Hooks.EwmhDesktops (ewmhFullscreen, ewmh)
import System.Environment (lookupEnv, getExecutablePath)
import System.FilePath (takeDirectory, (</>))
import qualified XMonad.StackSet as W
import XMonad.Prompt
import XMonad.Prompt.Workspace
--import XMonad.Util.NamedWindows (getName)
--import XMonad.Util.WorkspaceCompare (getSortByIndex)

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
    , ((myModMask, xK_w                   ), spawn myBrowser)
    , ((myModMask, xK_f                   ), spawn "firefox")
    , ((myModMask, xK_e                   ), spawn myFileManager)
    , ((myModMask .|. shiftMask, xK_Return), spawn myTerminal)
    , ((myModMask, xK_c                   ), kill)
    , ((myModMask .|. shiftMask, xK_z     ), spawn "xscreensaver-command -lock")
    , ((myModMask, xK_Tab                 ), nextWS)  -- Cycle to the next workspace
    , ((myModMask, xK_s                   ), spawn "scrot ~/Downloads/screenshot_%Y-%m-%d_%H-%M-%S.png")
    -- , ((myModMask .|. shiftMask, xK_s), unGrab *> spawn "scrot -s ~/Downloads/screenshot_%Y-%m-%d_%H-%M-%S.png")
    , ((myModMask, xK_BackSpace), showWorkspaceStatus)
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

showWorkspaceStatus :: X ()
showWorkspaceStatus = do
    ws <- gets windowset
    
    let current = W.currentTag ws
        -- Hidden workspaces WITH windows
        hiddenOccupied = [W.tag w | w <- W.hidden ws, 
                           maybe False (const True) (W.stack w)]
        
        -- Empty workspaces
        emptyWs = [W.tag w | w <- W.workspaces ws, 
                   maybe True (const False) (W.stack w),
                   W.tag w /= current]
        
        -- Build status message (only show non-empty sections)
        status = "Current: " ++ current ++ 
                 (if not (null hiddenOccupied)
                     then "\nHidden: " ++ unwords hiddenOccupied
                     else "")
                 
    -- Use timeout (-t) of 1500ms and prevent title truncation with proper flags
        -- Use timeout (-t) of 1500ms and prevent title truncation with proper flags
    spawn $ "notify-send -t 800 -u normal -h string:x-canonical-private-synchronous:ws 'WS' '" ++ status ++ "'"

myXmobarPP :: PP
myXmobarPP = def
    { ppSep   = xmobarColor myMagenta "" $ ppSep def
     , ppTitleSanitize   = xmobarStrip
     , ppCurrent         = xmobarColor myCyan ""
     , ppOrder = \[ws, l, wins] -> [l, ws, wins]
    }
