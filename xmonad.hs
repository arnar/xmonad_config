import XMonad
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import XMonad.Actions.SwapWorkspaces
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Grid
import XMonad.Util.Run
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.WindowBringer
--import XMonad.Actions.Commands
import XMonad.Layout.NoBorders
import Data.List
import System.IO

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

myLayout = tiled ||| Mirror tiled ||| noBorders Full ||| Grid
  where
    tiled  = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

myManageHook = composeAll . concat $ 
    [ [ resource =? "Do" --> doIgnore ]
    , [(className =? "Shiretoko" <&&> resource =? "Download") --> doFloat ]
    ]

myKeys = concat [
          [ ("M-x f",       spawn "firefox" ) 
          , ("M-<Left>",    prevWS )
          , ("M-<Right>",   nextWS )
          , ("M-S-<Left>",  shiftToPrev )
          , ("M-S-<Right>", shiftToNext )
          , ("M-S-g",       gotoMenu )
          , ("M-S-b",       bringMenu )
          --, ("M-C-k",       defaultCommands >>= runCommand )
          ],
          [("M-C-" ++ i, windows $ swapWithCurrent i) | i <- myWorkspaces]
         ]


-- LogHook prettyprinter for dzen

pixmaps = "/home/arnar/.xmonad/"
light_gray = "#c7c8c6"
dark_gray  = "#434541"
font = "xft:Sans:size=9:weight=regular:hinting=true:hintstyle=hintslight:antialias=true:rgba=rgb:lcdfilter=lcdligh"

myPP =  defaultPP { ppCurrent  = dzenColor "black" "#999999" . pad
                  , ppVisible  = dzenColor "black" light_gray . pad
                  , ppHidden   = dzenColor "#e6e6e6" dark_gray . pad
                  , ppHiddenNoWindows = const ""
                  , ppUrgent   = dzenColor "red" "yellow"
                  , ppWsSep    = ""
                  , ppSep      = ""
                  , ppLayout   = dzenColor light_gray dark_gray .
                                 (\ x -> case x of
                                           "Tall"            -> " ^i(" ++ pixmaps ++ "layout-tall.xbm)  "
                                           "Mirror Tall"     -> " ^i(" ++ pixmaps ++ "layout-mtall.xbm)  "
                                           "Full"            -> " ^i(" ++ pixmaps ++ "layout-full.xbm)  "
                                           "Grid"            -> " ^i(" ++ pixmaps ++ "layout-grid.xbm)  "
                                           _                 -> pad x
                                 )
                  , ppTitle    = dzenEscape . wrap "[ " " ]"
                  }

main = do
  dzen <- spawnPipe ("dzen2 -x '225' -y '4' -h '15' -w '685' -ta 'l' "
                     ++ "-fg '" ++ light_gray ++ "' -bg '" ++ dark_gray ++ "' "
                     ++ "-fn '" ++ font ++ "'")

  xmonad $ gnomeConfig { 
          modMask = mod4Mask
        , workspaces = myWorkspaces
        , layoutHook = desktopLayoutModifiers myLayout
        , manageHook = manageHook gnomeConfig <+> myManageHook
        , logHook = ewmhDesktopsLogHook >> dynamicLogWithPP (myPP { ppOutput = hPutStrLn dzen })
        }
        `additionalKeysP` myKeys
