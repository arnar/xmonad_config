import XMonad
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import XMonad.Actions.SwapWorkspaces
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Reflect
import XMonad.Util.Run
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.WindowBringer
import XMonad.Util.Loggers
--import XMonad.Actions.Commands
import XMonad.Layout.NoBorders
import Data.List
import Data.Maybe
import Data.Ratio ((%))
import System
import System.IO
import Control.Monad
import qualified XMonad.StackSet as S
import XMonad.Util.NamedWindows (getName)


myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

myLayout = tiled ||| Mirror tiled ||| noBorders Full ||| Grid ||| im ||| gimp
  where
    tiled  = Tall nmaster delta ratio
    nmaster = 1
    ratio = 3/5
    delta = 3/100
    im = withIM (1%7) (Title "Buddy List") Grid
    gimp = withIM (0.11) (Role "gimp-toolbox") $
           reflectHoriz $
           withIM (0.15) (Role "gimp-dock") Full

myManageHook = composeAll . concat $ 
    [ [ resource =? "Do" --> doIgnore ]
    , [ resource =? "spotify.exe" --> doFloat ]
    , [(className =? "Firefox" <&&> resource =? "Download") --> doFloat ]
    , [(className =? "Firefox" <&&> resource =? "Extension") --> doFloat ]
    ]

myKeys = concat [
          [ ("M-x f",       spawn "firefox" ) 
          , ("M-<Left>",    prevWS )
          , ("M-<Right>",   nextWS )
          , ("M-S-<Left>",  shiftToPrev )
          , ("M-S-<Right>", shiftToNext )
          , ("M-S-g",       gotoMenu )
          , ("M-S-b",       bringMenu )
          --, ("M-c",         spawn "/bin/bash -l -c '~/bin/caps_toggle'" )
          --, ("M-C-k",       defaultCommands >>= runCommand )
          ],
          [("M-C-" ++ i, windows $ swapWithCurrent i) | i <- myWorkspaces]
         ]


-- LogHook prettyprinter for dzen

pixmaps = "/data/home/arnar/.xmonad/"
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
                                           "Tall"                 -> " ^i(" ++ pixmaps ++ "layout-tall.xbm) "
                                           "Mirror Tall"          -> " ^i(" ++ pixmaps ++ "layout-mtall.xbm) "
                                           "Full"                 -> " ^i(" ++ pixmaps ++ "layout-full.xbm) "
                                           "Grid"                 -> " ^i(" ++ pixmaps ++ "layout-grid.xbm) "
                                           "IM Grid"              -> " ^i(" ++ pixmaps ++ "layout-im.xbm) "
                                           "IM ReflectX IM Full"  -> " ^i(" ++ pixmaps ++ "layout-gimp.xbm) "
                                           _                      -> pad x
                                 )
                  , ppTitle    = dzenEscape . wrap "[ " " ]"
                  , ppExtras   = [(liftM . liftM $ pad) (logCmd "/data/home/arnar/bin/caps_check")]
                  , ppOrder    = \[a,b,c,d] -> [a,b,d,c]
                  }

extraLogHook :: X ()
extraLogHook = do
  ws <- gets windowset
  case S.peek ws of
    Nothing -> return ()
    Just w  -> do cls <- withDisplay $ \d -> fmap (fromMaybe "") $ getStringProperty d w "WM_CLASS"
                  if (takeWhile (/= '\NUL') cls) == "emacs-snapshot-gtk"
                     then (io $ system "~/bin/caps_control") >> return ()
                     else (io $ system  "~/bin/caps_escape") >> return ()

main = do
  dzen <- spawnPipe ("dzen2 -x '230' -y '4' -h '15' -w '1200' -ta 'l' "
                     ++ "-fg '" ++ light_gray ++ "' -bg '" ++ dark_gray ++ "' "
                     ++ "-fn '" ++ font ++ "'")

  xmonad $ gnomeConfig { 
          modMask = mod4Mask
        , workspaces = myWorkspaces
        , layoutHook = desktopLayoutModifiers myLayout
        , manageHook = manageHook gnomeConfig <+> myManageHook
        , logHook = extraLogHook >> ewmhDesktopsLogHook >> dynamicLogWithPP (myPP { ppOutput = hPutStrLn dzen })
        }
        `additionalKeysP` myKeys
