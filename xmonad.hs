{-# LANGUAGE OverloadedStrings #-}

import XMonad
import XMonad.Config.Desktop
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import XMonad.Actions.SwapWorkspaces
import XMonad.Hooks.DynamicLog
import XMonad.Layout.GridVariants
import XMonad.Layout.TwoPane
import XMonad.Layout.Spacing
import qualified XMonad.Layout.Gaps as Gaps
import XMonad.Util.Run
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.NoBorders
import XMonad.Util.Scratchpad
import qualified XMonad.StackSet as S
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers (doRectFloat)

import qualified Data.ByteString as B
import Data.String (fromString)

myWorkspaces = map show [1..9]

myLayoutHook = spacingRaw True (Border 1 0 1 0) True (Border 0 1 0 1) True
               $ desktopLayoutModifiers myLayout
  where myLayout = narrowable (noBorders tiled)
          ||| narrowable (noBorders Full)
          ||| noBorders (TwoPane (3/100) (3/5))  -- One win always visible, rest stacked
          ||| noBorders (Grid (16/9))
        tiled  = Tall nmaster delta ratio
        nmaster = 1
        ratio = 3/5
        delta = 3/100
        narrowable = Gaps.gaps' [((Gaps.L,512),False),((Gaps.R,512),False)]

myManageHook = manageScratchPad <+> (composeAll
    [ resource =? "URxvtFuzzy" --> placeHook fzfCmdPlacement <+> hasBorder False <+> doFloat
    , (stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog") --> doRectFloat(S.RationalRect 0.25 0.25 0.5 0.5)
    , resource =? "gnubby_ssh_prompt" --> doFloat
    , resource =? "_Org_Capture_" --> placeHook capturePlacement <+> hasBorder False <+> doFloat
    ])
    where
      fzfCmdPlacement = withGaps (20,0,20,0) $ fixed (0.5,0.6)
      capturePlacement = withGaps (20,0,20,0) $ fixed (0.5,0.4)
      manageScratchPad = scratchpadManageHook (S.RationalRect l t w h)
        where
          h = 0.5
          w = 1
          t = 0
          l = 1 - w

-- Carbon X1 function key row:
--  XF86AudioMute
--  XF86AudioLowerVolume
--  XF86AudioRaiseVolume
--  XF86AudioMicMute
--  XF86MonBrightnessDown
--  XF86MonBrightnessUp
--  XF86Display
--  XF86WLAN
--  XF86Tools
--  XF86Bluetooth
--  XF86Favorites
myKeys = concat [
          [ ("M-p",                     spawn "~/bin/fuzzy_win ~/bin/fuzzy_cmd" )
          , ("M-`",                     scratchpadSpawnActionTerminal "urxvtc" )
          , ("M-y",                     swapNextScreen)
          , ("M-c",                     spawn "emacsclient -ne '(make-capture-frame)'")
          , ("<XF86PowerOff>",          screenlock)
          , ("<XF86Favorites>",         screenlock)  -- Star key on thinkpad
          , ("<XF86MonBrightnessDown>", spawn "~/tools/brightlight/brightlight -p -d 5")
          , ("<XF86MonBrightnessUp>",   spawn "~/tools/brightlight/brightlight -p -i 5")
          , ("<XF86AudioMute>",         spawn "amixer set Master toggle")
          , ("<XF86AudioLowerVolume>",  spawn "amixer set Master 2%-")
          , ("<XF86AudioRaiseVolume>",  spawn "amixer set Master 2%+")
          , ("<XF86AudioMicMute>",      spawn "amixer set Capture toggle")
          , ("<XF86Display>",           spawn "~/.dotfiles/resetscreens.sh")
          , ("C-<Down>",                spawn "~/.dotfiles/resetscreens.sh")
          , ("<Print>",                 spawn "flameshot gui")
          , ("M-n",                     toggleGaps)
          , ("M-S-h",                   incGaps)
          , ("M-S-l",                   decGaps)
          , ("M-x",                     spawn "xterm -e sh")  -- in case urxvt has issues
          ],
          [ (prefix ++ ws, action ws) | ws <- myWorkspaces
                                      , (prefix, action) <- [ ("M-", windows . S.view)
                                                            , ("S-M-", windows . S.shift)
                                                            , ("C-M-", windows . swapWithCurrent)
                                                            ]
          ]
         ]
        where
          toggleGaps = sendMessage Gaps.ToggleGaps
          incGaps = sequence_ $ map (sendMessage . Gaps.IncGap 64) [Gaps.R, Gaps.L]
          decGaps = sequence_ $ map (sendMessage . Gaps.DecGap 64) [Gaps.R, Gaps.L]
          screenlock = spawn "\
              \XSECURELOCK_BLANK_TIMEOUT=1 \
              \XSECURELOCK_BLANK_DPMS_STATE=suspend \
              \/usr/share/goobuntu-desktop-files/xsecurelock.sh"


polylinePP = def { ppOutput = B.appendFile "/tmp/.xmonad-workspace-log" . fromString . (++ "\n")
                 , ppCurrent = overline highlight . background highlight . pad , ppVisible = overline highlight . pad
                 , ppHidden = pad . omit "NSP"
                 , ppHiddenNoWindows = foreground (nord 3) . pad . omit "NSP"
                 , ppUrgent = overline (nord 11) . pad
                 , ppWsSep = ""
                 --, ppSep = ":: \xF79F \xf66b \xfcb5 \xe795 \xe7a2 \xf120 \xf268 \xf7ae \xf7c0 \xfc56 \xfa6f \xf57f \xf017 \xf64f "
                 , ppSep = ":: "
                 , ppLayout = id
                 , ppTitle = const ""
                 }
             where
               omit nameToOmit x = if x == nameToOmit then "" else x
               underline color s = concat [ "%{u", color, "}%{+u}", s, "%{u-}" ]
               overline color s = concat [ "%{o", color, "}%{+o}", s, "%{o-}" ]
               foreground color s = concat [ "%{F", color, "}", s, "%{F-}" ]
               background color s = concat [ "%{B", color, "}", s, "%{B-}" ]
               highlight = "#990000"
               nord n = [ "#2e3440", "#3b4252", "#434c5e", "#4c566a", "#d8dee9", "#e5e9f0", "#eceff4", "#8fbcbb",
                          "#88c0d0", "#81a1c1", "#5e81ac", "#bf616a", "#d08770", "#ebcb8b", "#a3be8c", "#b48ead" ] !! n


myLogHook = do
  dynamicLogWithPP polylinePP
  -- For now using compton's 'inactive-fade', but if that stops working we can go back to this:
  --fadeInactiveLogHook 0xeeeeeeee

myStartupHook = do
  startupHook desktopConfig
  setWMName "LG3D"

myConfig = desktopConfig 
           { modMask = mod4Mask
           , workspaces = myWorkspaces
           , layoutHook = myLayoutHook
           , manageHook = manageHook desktopConfig <+> myManageHook
           , handleEventHook = docksEventHook <+> handleEventHook desktopConfig
           , startupHook = myStartupHook
           , terminal = "urxvtc"
           , logHook = myLogHook
           } `additionalKeysP` myKeys

main = do
  safeSpawn "mkfifo" ["/tmp/.xmonad-workspace-log"]
  xmonad $ withUrgencyHook NoUrgencyHook
         $ docks
         $ ewmh
         $ myConfig
