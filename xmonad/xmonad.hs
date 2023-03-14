import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)

import XMonad.Util.Loggers
import XMonad.Util.Run

import XMonad.Layout.Gaps (gaps, Direction2D(..))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.SimpleFloat

import XMonad.Actions.WindowGo (runOrRaise, ifWindows, raiseMaybe)

import qualified XMonad.Util.Dzen as Dzen
import XMonad.Util.Dzen ((>=>))

import XMonad.Prompt
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.XMonad (xmonadPrompt)


import qualified XMonad.StackSet as W

import Graphics.X11.ExtraTypes.XF86
import qualified Data.Map as M
import Data.Monoid
import Text.Printf

toggleOnlyTerm set = ifWindows (className =? "ONLY_TERM") (flipWindow . head) (spawn "kitty --class=ONLY_TERM")
  where flipWindow :: Window -> X ()
        flipWindow w = case W.findTag w set of
          Nothing -> return ()
          Just tag -> windows $ W.shiftWin (flipWS tag set) w
        current = W.currentTag set
        flipWS ws set = if ws == current then (last myWorkspaces) else current

displayBig :: String -> X ()
displayBig text = Dzen.dzenConfig (Dzen.timeout 3 >=> Dzen.font myBigFont >=> Dzen.vCenter 0 0) text

shellCommand command args = runProcessWithInput command args "" >> (return ())
shellCommandFirstLine command args = do
  result <- runProcessWithInput command args ""
  return $ head $ lines result

displayVolume = shellCommandFirstLine "pamixer" ["--get-volume-human"] >>= (\vol -> displayBig $ "VOL: " ++ vol)
volumeUp = shellCommand "pamixer" ["--increase", "5"] >> displayVolume
volumeDown = shellCommand "pamixer" ["--decrease", "5"] >> displayVolume
volumeToggle = shellCommand "pamixer" ["--toggle-mute"] >> displayVolume

displayPlayerStatus = do
  status <- shellCommandFirstLine "playerctl" ["status"]
  artist <- shellCommandFirstLine "playerctl" ["metadata", "artist"]
  t <- shellCommandFirstLine "playerctl" ["metadata", "title"]
  displayBig (printf "%s: %s - %s" status artist t)
playToggle = shellCommand "playerctl" ["play-pause"] >> displayPlayerStatus
playNext = shellCommand "playerctl" ["next"] >> displayPlayerStatus
playPrev = shellCommand "playerctl" ["prev"] >> displayPlayerStatus

displayBrightness = shellCommandFirstLine "xbacklight" [] >>= (\bl -> displayBig $ "BACKLIGHT: " ++ (take 2 bl) ++ "%")
increaseBrightness = shellCommand "xbacklight" ["+5"] >> displayBrightness
decreaseBrightness = shellCommand "xbacklight" ["-5"] >> displayBrightness

runOrRaiseEmacs :: X ()
runOrRaiseEmacs = (raiseMaybe . spawn) "emacsclient -c -a 'emacs'" (className =? "Emacs")

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2 . xmobarBorder "Bottom" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logClassnames formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . blue  . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . white . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 10

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#444444" ""

myFont = "xft:Roboto:bold:size=14:antialias=true:hinting=true"
myBigFont = "xft:Liberation Mono:bold:size=20:antialias=true:hinting=true"
myModMask = mod4Mask
myTerminal = "kitty"
myFocusFollowsMouse = True
myClickJustFocuses = False
myBorderWidth = 3
myNormalBorderColor = "black"
myFocusedBorderColor = "#00bcd4"
myWorkspaces = map show [1..10]
myLayoutHook = gaps [(D, 2)] $
               smartBorders $
               tiled ||| Full
  where tiled = Tall 1 (5/100) (2/3)
myWorkspaceKeys = [xK_quotedbl, xK_guillemotleft, xK_guillemotright,
                   xK_parenleft, xK_parenright, xK_at,
                   xK_plus, xK_minus, xK_slash, xK_asterisk]



myKeys conf@(XConfig {modMask = m}) =
    M.fromList $
         [ -- Launch emacs, or just focus it
           ((m, xK_e),               runOrRaiseEmacs)
         -- Launch firefox, or just focus it
         , ((m, xK_f),               runOrRaise "firefox" (className =? "firefox"))
         -- Invoke/hides the only terminal
         , ((m, xK_Return),     withWindowSet $ toggleOnlyTerm)
         -- Launch terminal
         , ((m .|. s, xK_Return),    spawn (XMonad.terminal conf))
         -- Launch command prompt
         , ((m, xK_comma),           shellPrompt def { font = myFont  })
         -- Launch XMonad prompt
         , ((m .|. s , xK_comma),    xmonadPrompt amberXPConfig)
         -- Volume management
         , ((0, xF86XK_AudioRaiseVolume), volumeUp)
         , ((0, xF86XK_AudioLowerVolume), volumeDown)
         , ((0, xF86XK_AudioMute),   volumeToggle)
         , ((0, xF86XK_MonBrightnessDown), decreaseBrightness)
         , ((0, xF86XK_MonBrightnessUp), increaseBrightness)
         -- Music
         , ((0, xF86XK_AudioPlay), playToggle)
         , ((m, xK_a), playToggle)
         , ((m, xK_u), playPrev)
         , ((m, xK_i), playNext)
         -- Close a window
         , ((m .|. s, xK_q ), kill)
         -- Restart xmonad
         , ((m, xK_q), spawn "xmonad --recompile; xmonad --restart")
         -- Dynamic layout conf
         , ((m, xK_n),               windows W.focusDown)
         , ((m .|. s, xK_n),         windows W.swapDown)
         , ((m, xK_p),               windows W.focusUp)
         , ((m .|. s, xK_p),         windows W.swapUp)
         , ((m, xK_m),               windows W.focusMaster)
         , ((m .|. s, xK_m),         windows W.swapMaster)
         , ((m, xK_c),               sendMessage Shrink)
         , ((m, xK_t),               sendMessage Shrink)
         , ((m, xK_r),               sendMessage Expand)
         , ((m, xK_s),               sendMessage Expand)
         , ((m, xK_g),               withFocused $ windows . W.sink)
         , ((m, xK_v),               sendMessage (IncMasterN 1))
         , ((m, xK_d),               sendMessage (IncMasterN (-1)))
         , ((m, xK_space),           sendMessage NextLayout)
         , ((m .|. s, xK_space),     setLayout $ XMonad.layoutHook conf)
         ]
         ++
         -- Move between workspaces (bepo layout :])
         [((m, key), windows $ W.greedyView workspace)
              | (workspace, key) <- zip (XMonad.workspaces conf)
                                        myWorkspaceKeys]
         ++
         -- Move windows between workspaces
         [((m .|. s, key), windows $ W.shift workspace)
              | (workspace, key) <- zip (XMonad.workspaces conf)
                                        myWorkspaceKeys]
           where s = shiftMask

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
  [  className =? "mpv"             --> doFloat
   , className =? "xmessage"        --> doCenterFloat
  ]

myStartupHook :: X ()
myStartupHook = do
  spawn "xsetroot -solid black"
  spawn "xset b off"
  spawn "setxkbmap -option ctrl:nocaps fr bepo"
  spawn "/usr/bin/emacs --daemon"
  spawn "redshift -l 45:-73"
  spawn "XSECURELOCK_PASSWORD_PROMPT=time_hex XSECURELOCK_SHOW_HOSTNAME=0 XSECURELOCK_SHOW_USERNAME=0 XSECURELOCK_SHOW_KEYBOARD_LAYOUT=0 xss-lock -n /usr/lib/xsecurelock/dimmer -l -- xsecurelock"

myConfig = def
  { modMask = myModMask
  , terminal = myTerminal
  , focusFollowsMouse = myFocusFollowsMouse
  , clickJustFocuses = myClickJustFocuses
  , borderWidth = myBorderWidth
  , normalBorderColor = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , workspaces = myWorkspaces
  , layoutHook = myLayoutHook
  , manageHook = myManageHook
  , startupHook = myStartupHook
  , keys = myKeys
  }

main :: IO ()
main = xmonad
     . ewmh
     . withEasySB (statusBarGeneric "polybar -config /home/quentin/.dotfiles/polybar/config.ini" mempty) defToggleStrutsKey
     $ myConfig
