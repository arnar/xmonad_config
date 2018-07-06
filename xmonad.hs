import XMonad
import XMonad.Config.Desktop
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import XMonad.Actions.SwapWorkspaces
import XMonad.Hooks.DynamicLog
import XMonad.Layout.GridVariants
import XMonad.Layout.IM
import XMonad.Layout.Reflect
import XMonad.Layout.TwoPane
import qualified XMonad.Layout.Groups as G
import XMonad.Layout.Groups.Helpers
import XMonad.Layout.SimpleFloat
import XMonad.Util.Run
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Actions.WindowBringer
import XMonad.Util.Loggers
--import XMonad.Actions.Commands
import XMonad.Layout.NoBorders
import XMonad.Util.Scratchpad
import Data.List
import Data.Maybe
import Data.Ratio ((%))
import System.IO
import Control.Monad
import qualified XMonad.StackSet as S
import XMonad.Hooks.Place
import XMonad.Actions.CopyWindow
import XMonad.Hooks.SetWMName

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

myLayout = smartBorders tiled
       ||| noBorders Full
       ||| smartBorders (TwoPane (3/100) (3/5))      -- Good for TeXing
       ||| smartBorders (G.group (Grid (16/9)) Full) -- Good for editor fullscreen + bunch of terminals
       ||| smartBorders (Grid (16/9))
       ||| im
       ||| gimp
       ||| simpleFloat
  where
    tiled  = Tall nmaster delta ratio
    nmaster = 1
    ratio = 3/5
    delta = 3/100
    im = withIM (1%7) (Or (Title "Buddy List") (Title "arnarbi - Skype™")) (Grid (1/2))
    gimp = withIM (0.11) (Role "gimp-toolbox") $
           reflectHoriz $
           withIM (0.15) (Role "gimp-dock") Full

myManageHook = (composeAll
    [ title =? "Do" --> doIgnore
    , resource =? "Do" --> doFloat
    , resource =? "unity-2d-panel" --> doIgnore
    , resource =? "spotify.exe" --> doFloat
    , resource =? "hamster-applet" --> doFloat
    , (className =? "Firefox" <&&> resource =? "Download") --> doFloat
    , (className =? "Firefox" <&&> resource =? "Extension") --> doFloat
    , className =? "Shutter" --> doFloat
    --, (stringProperty "WM_WINDOW_ROLE" =? "pop-up") --> doFloat   -- e.g. the hangouts extension
    , isHangouts --> placeHook hangoutsPlacement <+> doFloat
    ])  <+> manageScratchPad
  where
    isHangouts = appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd"

hangoutsPlacement = withGaps (20,5,30,0) $ smart (1.0, 1.0)

manageScratchPad = scratchpadManageHook (S.RationalRect l t w h)
  where
    h = 0.1
    w = 1
    t = 1 - h
    l = 1 - w

myKeys = concat [
          [ ("M-x f",       spawn "firefox" ) 
          , ("M-u",         prevWS )
          , ("M-d",         nextWS )
          , ("M-S-u",       shiftToPrev )
          , ("M-S-d",       shiftToNext )
          , ("M-S-g",       gotoMenu )
          , ("M-S-b",       bringMenu )
          , ("M-p",         spawn "dmenu_run -l 5 -nb dimgray -nf whitesmoke -sb darkgray -sf white" )
          --, ("M-u",         scratchpadSpawnActionTerminal "gnome-terminal --disable-factory --name scratchpad" )
          , ("M-S-C-j",     moveToGroupDown True)
          , ("M-S-C-k",     moveToGroupUp True)
          , ("M-S-C-h",     moveToNewGroupDown)
          , ("M-S-C-l",     moveToNewGroupUp)
          , ("M-x c",       placeFocused hangoutsPlacement)  -- place floated chat window
	  --, ("M-a",         sequence_ $ [windows $ copy i | i <- XMonad.workspaces conf])
	  --, ("M-S-a",       windows $ kill8)
	  , ("M1-<KP_Add>", spawn "amixer -c 0 set Master 1+")
	  , ("M1-<KP_Subtract>", spawn "amixer -c 0 set Master 1-")
	  , ("<XF86PowerOff>", spawn "xsecurelock")
          ],
          [("M-C-" ++ i, windows $ swapWithCurrent i) | i <- myWorkspaces],
	  [ (prefix ++ "M-" ++ ws, action ws)
            | ws <- myWorkspaces
	    , (prefix, action) <- [ ("", windows . S.view)
	    			  , ("S-", windows . S.shift)]
	  ]
         ]

kill8 ss | Just w <- S.peek ss = (S.insertUp w) $ S.delete w ss
         | otherwise = ss

-- LogHook prettyprinter for dzen

pixmaps = "/usr/local/google/home/arnarb/.xmonad/"
light_gray = "#cccccc" -- #c7c8c6"
dark_gray  = "#434541"  -- very very dark, was "#434541" before, #303030 to match vim notext
--font = "xft:Sans:size=9:weight=regular:hinting=true:hintstyle=hintslight:antialias=true:rgba=rgb:lcdfilter=lcdligh"
font = "Ubuntu Mono-10"
--font = "Droid Sans Mono-10"

myPP pipe =  defaultPP { ppOutput = hPutStrLn pipe
                       , ppCurrent  = dzenColor "white" "#990000" . pad
                       , ppVisible  = dzenColor "#CCCCCC" "#666666" . pad
                       , ppHidden   = dzenColor "#e6e6e6" dark_gray . pad
                       , ppHiddenNoWindows = dzenColor "#666666" dark_gray . pad
                       , ppUrgent   = dzenColor "red" "yellow"
                       , ppWsSep    = ""
                       , ppSep      = ""
                       , ppLayout   = dzenColor light_gray dark_gray . layoutPixmap
                       , ppTitle    = dzenEscape . (":: " ++)
                       {-, ppOrder    = \x -> case x of -}
                       {-                       [a,b,c,d,e] -> [a,b,d,e,c]-}
                       {-                       _ -> ["bzzt"]-}
                       }
        where
          layoutPixmap lname = case (lookup lname pixmapMap) of
                                Just x -> " ^i(" ++ pixmaps ++ "layout-" ++ x ++ ".xbm) "
                                Nothing -> pad lname
          pixmapMap = [ ( "Tall"                  , "tall" )
                      , ( "Mirror Tall"           , "mtall" )
                      , ( "Full"                  , "full" )
                      , ( "TwoPane"               , "twopane" )
                      , ( "Grid"                  , "grid" )
                      , ( "IM Grid"               , "im" )
                      , ( "IM ReflectX IM Full"   , "gimp" )
                      , ( "Grid by Full"          , "gridbyfull" )
                      ]

myLogHook :: X ()
myLogHook = fadeInactiveLogHook 0xdddddddd

myStartupHook = do
  startupHook desktopConfig
  setWMName "LG3D"

myConfig = desktopConfig 
           { modMask = mod4Mask
           , workspaces = myWorkspaces
           , layoutHook = desktopLayoutModifiers myLayout
           , manageHook = manageHook desktopConfig <+> myManageHook
           , startupHook = myStartupHook
           , normalBorderColor = "#505050"
           , focusedBorderColor = "#660000"
           , terminal = "urxvt"
           } `additionalKeysP` myKeys

main = do
  dzen <- spawnPipe ("~/tools/dzen/dzen2 -dock -xs 1 -h 20 -w 1600 -ta l "
                     ++ "-fg '" ++ light_gray ++ "' -bg '" ++ dark_gray ++ "' "
                     ++ "-fn '" ++ font ++ "'")
  --dzen <- openFile "/usr/local/google/home/arnarb/.xmonad/logpipe" ReadWriteMode
  --hSetBuffering dzen LineBuffering
  xmonad $ myConfig {
               logHook = myLogHook >> dynamicLogWithPP (myPP dzen)
             }
