{-# LANGUAGE OverloadedStrings #-}

import XMonad
import XMonad.Config.Desktop
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import XMonad.Actions.SwapWorkspaces
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Layout.GridVariants
import XMonad.Layout.IndependentScreens
import XMonad.Layout.TwoPane
import XMonad.Layout.Spacing
import qualified XMonad.Layout.Gaps as Gaps
import XMonad.Util.Run
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.NoBorders
import XMonad.Util.NamedScratchpad
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

scratchpads = [
        NS "quaketerm" 
           "alacritty --class quaketerm" 
           (resource =? "quaketerm") 
           (customFloating $ S.RationalRect 0 0 1 0.5),
        NS "launcher" 
           "~/bin/fuzzy_win ~/bin/fuzzy_cmd" 
           (resource =? "URxvtFuzzy") 
           (placeHook fzfCmdPlacement <+> hasBorder False <+> doFloat),
       NS "org-capture"
          "emacsclient -ne '(make-capture-frame)'"
          (title =? "*Org Capture*")
          (placeHook capturePlacement <+> hasBorder False <+> doFloat)
    ]
    where
      fzfCmdPlacement = withGaps (20,0,20,0) $ fixed (0.5,0.6)
      capturePlacement = withGaps (20,0,20,0) $ fixed (0.5,0.4)

myManageHook = composeAll [
    namedScratchpadManageHook scratchpads,
    stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog" --> doRectFloat(S.RationalRect 0.25 0.25 0.5 0.5)
  ]

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
          [ ("M-p",                     scratch "launcher")
          , ("M-`",                     scratch "quaketerm")
          , ("M-y",                     swapNextScreen)
          , ("M-c",                     scratch "org-capture")
          , ("<XF86PowerOff>",          screenlock)
          , ("<XF86Favorites>",         screenlock)  -- Star key on thinkpad
          , ("M-<Esc>",                 screenlock)
          , ("<XF86MonBrightnessDown>", spawn "polybar-msg action '#backlight.dec'")
          , ("<XF86MonBrightnessUp>",   spawn "polybar-msg action '#backlight.inc'")
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
          , ("M-x",                     spawn "xterm -e sh")  -- "safe mode"
          , ("M-m",                     windows . S.view $ "monitor")
          ],
          [ (prefix ++ ws, action ws) | ws <- myWorkspaces
                                      , (prefix, action) <- [ ("M-", windows . S.view)
                                                            , ("S-M-", windows . S.shift)
                                                            , ("C-M-", windows . swapWithCurrent)
                                                            ]
          ]
         ]
        where
          scratch = namedScratchpadAction scratchpads
          toggleGaps = sendMessage Gaps.ToggleGaps
          incGaps = sequence_ $ map (sendMessage . Gaps.IncGap 64) [Gaps.R, Gaps.L]
          decGaps = sequence_ $ map (sendMessage . Gaps.DecGap 64) [Gaps.R, Gaps.L]
          screenlock = spawn "\
              \XSECURELOCK_BLANK_TIMEOUT=1 \
              \XSECURELOCK_BLANK_DPMS_STATE=suspend \
              \/usr/share/goobuntu-desktop-files/xsecurelock.sh"


polylinePP = def { ppOutput = B.appendFile "/tmp/.xmonad-workspace-log" . fromString . (++ "\n")
                 , ppCurrent = overline highlight . background highlight . pad
                 , ppVisible = overline highlight . pad . omit "monitor"
                 , ppHidden = pad . omit "NSP" . omit "monitor"
                 , ppHiddenNoWindows = foreground (nord 3) . pad . omit "NSP"
                 , ppUrgent = overline (nord 11) . pad
                 , ppWsSep = ""
                 , ppSep = ":: "
                 , ppLayout = layoutIcon
                 --, ppTitle = const "$$ \xF79F \xf66b \xfcb5 \xe795 \xe7a2 \xf120 \xf268 \xf7ae \xf7c0 \xfc56 \xfa6f \xf57f \xf017 \xf64f"
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
               layoutIcon "Spacing Tall" = "\xf878 "
               layoutIcon "Spacing Full" = "\xf096 "
               layoutIcon "Spacing TwoPane" = "\xf0db "
               layoutIcon "Spacing Grid" = "\xfc56 "
               layoutIcon layout = layout


myLogHook = do
  dynamicLogWithPP polylinePP
  fadeInactiveLogHook 0xee000000  -- Alternative to compton's 'inactive-fade'

myStartupHook = do
  startupHook desktopConfig
  setWMName "LG3D"

myConfig = desktopConfig 
           { modMask = mod4Mask
           , workspaces = "monitor" : myWorkspaces
           , layoutHook = myLayoutHook
           , manageHook = manageHook desktopConfig <+> myManageHook
           , handleEventHook = docksEventHook <+> handleEventHook desktopConfig
           , startupHook = myStartupHook
           , terminal = "alacritty"
           , logHook = myLogHook
           } `additionalKeysP` myKeys

main = do
  safeSpawn "mkfifo" ["/tmp/.xmonad-workspace-log"]
  xmonad $ withUrgencyHook NoUrgencyHook
         $ docks
         $ ewmh
         $ myConfig
