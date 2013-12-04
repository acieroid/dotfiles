{-# LANGUAGE ScopedTypeVariables #-}
import Control.Exception (catch)
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import qualified Network.MPD as MPD
import System.Environment (getEnv)
import XMonad
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.Dzen
import XMonad.Util.Run

-- Like getEnv, but exception-less, with a default value instead
getEnv' :: String -> String -> IO String
getEnv' var def = getEnv var `catch` \(_ :: IOError) -> return def

-- Like head, but don't crash on failure
safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

-- Overwrite some parameter in default graphical configurations

myXPConfig = defaultXPConfig

myDzenConfig = (onCurr (vCenter 25))

-- Display a centered message
message str =
    dzenConfig myDzenConfig str

-- Run a command with the given arguments, and display the result using 'message'
messageCmd cmd args =
    runProcessWithInput cmd args "" >>= message

-- MPD stuff

data MPDCommand = MPDStatus | MPDNext | MPDPrev | MPDToggle

mpd :: MPDCommand -> X ()
mpd MPDStatus =
    (io $ MPD.withMPD MPD.currentSong >>=
           (\res -> return $ case res of
                      Left err -> show err
                      Right song ->
                          (extract MPD.Artist song) ++ " - " ++
                             (extract MPD.Title song))) >>=
    message
  where extract :: MPD.Metadata -> Maybe MPD.Song -> String
        extract tag song = maybe "" MPD.toString
                           (song >>= MPD.sgGetTag tag >>= safeHead)

mpd MPDNext =
    -- TODO: display error instead of ignoring it?
    io $ MPD.withMPD MPD.next >> return ()

mpd MPDPrev =
    io $ MPD.withMPD MPD.previous >> return ()

mpd MPDToggle =
    error "todo"

-- Basing changes of XMonad defaults

myModMask = mod1Mask -- TODO: mod4Mask
myTerminal = "urxvt"
myFocusFollowsMouse = False
myWorkspaces = map show [1..10]

-- Keybindings, defined from scratch (some bits are copied from
-- XMonad's defaultKeys)

myWorkspaceKeys = [xK_quotedbl, xK_guillemotleft, xK_guillemotright,
                   xK_parenleft, xK_parenright, xK_at,
                   xK_plus, xK_minus, xK_slash, xK_asterisk]

myKeys conf@(XConfig {modMask = m}) =
    M.fromList $
         [ -- Launch emacs, or just focus it
           ((m, xK_e),         runOrRaise "emacs"   (className =? "Emacs"))
           -- Launch firefox, or just focus it
         , ((m, xK_f),         runOrRaise "firefox" (className =? "browser"))
           -- Launch terminal
         , ((m, xK_Return),    spawn (XMonad.terminal conf))
           -- Launch command prompt
         , ((m, xK_comma),     shellPrompt myXPConfig)
           -- Launch XMonad prompt
         , ((m .|. shiftMask , xK_comma), xmonadPrompt myXPConfig)
           -- Various information (time, mpd, ...)
         , ((0, xF86XK_Calculator), messageCmd "/usr/bin/date" [])
         , ((m, xK_a), mpd MPDStatus)
           -- No need for:
           --  - a way to kill windows: I either cleanly close the
           --    program (eg. C-x C-c in emacs), and should I not, I
           --    prefer to call the 'kill' command from the command
           --    prompt, thus avoiding killing something by mistake

           -- Still need for:
           --  - static layout (like stumpwm)
           --  - move into a specific direction (up, down, left, right)
           --  - testing the behaviour when multiple screens are used
           --  - deny map and raise from the browser
           --  - have a simple way to output message (eg. time, battery etc.)
           --  - screenshot command
           --  - something similar to stumpwm's fullscreen command
           --  - center XMonad.Prompt
         ]
         ++
         -- Move between workspaces (bepo layout :])
         [((m, key), windows $ W.view workspace)
              | (workspace, key) <- zip (XMonad.workspaces conf)
                                        myWorkspaceKeys]
         ++
         -- Move windows between workspaces
         [((m .|. shiftMask, key), windows $ W.shift workspace)
              | (workspace, key) <- zip (XMonad.workspaces conf)
                                        myWorkspaceKeys]

main = xmonad defaultConfig
        { modMask = myModMask
        , terminal = myTerminal
        , focusFollowsMouse = myFocusFollowsMouse
        , workspaces = myWorkspaces
        , keys = myKeys
        }
