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
import XMonad.Util.Run
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
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

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

myLayout = smartBorders tiled
       ||| noBorders Full
       ||| smartBorders (TwoPane (3/100) (3/5))      -- Good for TeXing
       ||| smartBorders (G.group (Grid (16/9)) Full) -- Good for editor fullscreen + bunch of terminals
       ||| smartBorders (Grid (16/9))
       ||| im
       ||| gimp
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
    --, (stringProperty "WM_WINDOW_ROLE" =? "pop-up") --> doFloat   -- e.g. the hangouts extension
    , isHangouts --> placeHook hangoutsPlacement <+> doFloat
    ])  <+> manageScratchPad
  where
    isHangouts = appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd"

hangoutsPlacement = withGaps (20,0,0,0) $ smart (1.0, 1.0)

manageScratchPad = scratchpadManageHook (S.RationalRect l t w h)
  where
    h = 0.1
    w = 1
    t = 1 - h
    l = 1 - w

myKeys = concat [
          [ ("M-x f",       spawn "firefox" ) 
          , ("M-<Left>",    prevWS )
          , ("M-<Right>",   nextWS )
          , ("M-S-<Left>",  shiftToPrev )
          , ("M-S-<Right>", shiftToNext )
          , ("M-S-g",       gotoMenu )
          , ("M-S-b",       bringMenu )
          , ("M-p",         spawn "dmenu_run -l 5 -nb dimgray -nf whitesmoke -sb darkgray -sf white" )
          , ("M-u",         scratchpadSpawnActionTerminal "gnome-terminal --disable-factory --name scratchpad" )
          , ("M-S-C-j",     moveToGroupDown True)
          , ("M-S-C-k",     moveToGroupUp True)
          , ("M-S-C-h",     moveToNewGroupDown)
          , ("M-S-C-l",     moveToNewGroupUp)
          , ("M-x c",       placeFocused hangoutsPlacement)  -- place floated chat window
          ],
          [("M-C-" ++ i, windows $ swapWithCurrent i) | i <- myWorkspaces]
         ]


-- LogHook prettyprinter for dzen

pixmaps = "/usr/local/google/home/arnarb/.xmonad/"
light_gray = "#cccccc" -- #c7c8c6"
dark_gray  = "#434541"  -- very very dark, was "#434541" before, #303030 to match vim notext
--font = "xft:Sans:size=9:weight=regular:hinting=true:hintstyle=hintslight:antialias=true:rgba=rgb:lcdfilter=lcdligh"
font = "Ubuntu Mono-10"
--font = "Droid Sans Mono-10"

myPP pipe =  defaultPP { ppOutput = hPutStrLn pipe
                       , ppCurrent  = dzenColor "black" "#999999" . pad
                       , ppVisible  = dzenColor "black" light_gray . pad
                       , ppHidden   = dzenColor "#e6e6e6" dark_gray . pad
                       , ppHiddenNoWindows = dzenColor "#666666" dark_gray . pad
                       , ppUrgent   = dzenColor "red" "yellow"
                       , ppWsSep    = ""
                       , ppSep      = ""
                       , ppLayout   = dzenColor light_gray dark_gray . layoutPixmap
                       , ppTitle    = dzenEscape . (":: " ++)
                       --, ppExtras   = map (liftM . liftM $ pad) [capsControl, logCmd "~/bin/tstatus"]
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

capsControl :: X (Maybe String)
capsControl = return $ Just "E"

capsControl' = do
  ws <- gets windowset
  case S.peek ws of
    Nothing -> return $ Just ""
    Just w  -> do cls <- withDisplay $ \d -> fmap (fromMaybe "") $ getStringProperty d w "WM_CLASS"
                  if "emacs" `isPrefixOf` cls
                     then spawn "/data/home/arnar/bin/caps_control" >> return (Just "C")
                     else spawn "/data/home/arnar/bin/caps_escape"  >> return (Just "E")

myLogHook :: X ()
myLogHook = fadeInactiveLogHook 0xdddddddd

myConfig = desktopConfig 
           { modMask = mod4Mask
           , workspaces = myWorkspaces
           , layoutHook = desktopLayoutModifiers myLayout
           , manageHook = manageHook desktopConfig <+> myManageHook
           , startupHook = startupHook desktopConfig >> setWMName "LG3D"
           , normalBorderColor = "#505050"
           , focusedBorderColor = "#660000"
           , terminal = "urxvt"
           } `additionalKeysP` myKeys

main = do
  dzen <- spawnPipe ("~/tools/dzen/dzen2 -dock -h 20 -w 1500 -ta l "
                     ++ "-fg '" ++ light_gray ++ "' -bg '" ++ dark_gray ++ "' "
                     ++ "-fn '" ++ font ++ "'")
  --dzen <- openFile "/usr/local/google/home/arnarb/.xmonad/logpipe" ReadWriteMode
  --hSetBuffering dzen LineBuffering
  xmonad $ myConfig {
               logHook = myLogHook >> dynamicLogWithPP (myPP dzen)
             }
