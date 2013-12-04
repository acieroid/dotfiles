{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
import Control.Exception (catch)
import Control.Monad
import qualified Data.Map as M
import Data.Monoid
import Data.Word
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

-- Simple fullscreen layout
data Fullscreen a = Fullscreen
                  deriving (Show, Read)

instance LayoutClass Fullscreen a where
    description _ = "Fullscreen"

    -- The currently focused window takes the entire allowed rectangle
    pureLayout _ r s = [(W.focus s, r)]

    emptyLayout _ _ = return ([], Nothing)

    pureMessage _ _ = Nothing

-- Static layout

-- Split the current frame
data Split = VerticalSplit | HorizontalSplit deriving Typeable

instance Message Split

instance Ord Rectangle where
    compare r1 r2 = mconcat [ compare (rect_x r1) (rect_x r2)
                            , compare (rect_y r1) (rect_y r2)
                            , compare (rect_width r1) (rect_width r2)
                            , compare (rect_height r1) (rect_height r2)
                            ]

data StaticLayout a = StaticLayout { splits :: M.Map Rectangle (Maybe Window)
                                   , currentSplit :: Rectangle
                                   }
                      deriving (Show, Read)

instance LayoutClass StaticLayout Window where
    description _ = "Static"

    pureLayout l r s = [(W.focus s, currentSplit l)]

    emptyLayout _ r = return ([], Just (StaticLayout
                                        { splits = M.singleton r Nothing
                                        , currentSplit = r
                                        }))

    pureMessage l m = msum [fmap split $ fromMessage m]
        where split s =
                  l { splits = M.insert r1 win $
                               M.insert r2 Nothing $
                               M.delete (currentSplit l) (splits l)
                    , currentSplit = r1
                    }
                  where r = currentSplit l
                        win = case M.lookup r (splits l) of
                                Nothing -> Nothing
                                Just w -> w
                        (r1, r2) = f s r
                        f HorizontalSplit = vsplit
                        f VerticalSplit = hsplit
              vsplit r = (r { rect_height = h },
                          r { rect_height = h
                            , rect_y = fromIntegral (h + 1) })
                  where h = (rect_height r) `div` 2
              hsplit r = (r { rect_width = w },
                          r { rect_width = w
                            , rect_x = fromIntegral (w + 1) })
                  where w = (rect_width r) `div` 2

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

-- Run a command with the given arguments, and display the result
-- using 'message'
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
    io $ MPD.withMPD MPD.status >>=
       (\res -> case res of
                  Left err -> return ()
                  Right status -> case MPD.stState status of
                                    MPD.Playing -> MPD.withMPD (MPD.pause True)
                                                   >> return ()
                                    _ -> MPD.withMPD (MPD.play Nothing)
                                         >> return ())

-- Basing changes of XMonad defaults

myModMask = mod1Mask -- TODO: mod4Mask
myTerminal = "urxvt"
myFocusFollowsMouse = False
myWorkspaces = map show [1..10]
myLayout = Fullscreen

-- No xF86XK_Battery in Haskell's X11 library.
-- https://github.com/haskell-pkg-janitors/X11/issues/21
xF86XK_Battery :: KeySym
xF86XK_Battery = 0x1008FF93

-- Keybindings, defined from scratch (some bits are copied from
-- XMonad's defaultKeys)

myWorkspaceKeys = [xK_quotedbl, xK_guillemotleft, xK_guillemotright,
                   xK_parenleft, xK_parenright, xK_at,
                   xK_plus, xK_minus, xK_slash, xK_asterisk]

myKeys conf@(XConfig {modMask = m}) =
    M.fromList $
         [ -- Launch emacs, or just focus it
           ((m, xK_e),              runOrRaise "emacs"   (className =? "Emacs"))
           -- Launch firefox, or just focus it
         , ((m, xK_f),              runOrRaise "firefox" (className =? "browser"))
           -- Launch terminal
         , ((m, xK_Return),         spawn (XMonad.terminal conf))
           -- Launch command prompt
         , ((m, xK_comma),          shellPrompt myXPConfig)
           -- Launch XMonad prompt
         , ((m .|. s , xK_comma),   xmonadPrompt myXPConfig)
           -- Various control & information (time, mpd, ...)
         , ((0, xF86XK_Calculator), messageCmd "/usr/bin/date" [])
         , ((0, xF86XK_Launch1),    messageCmd "/usr/bin/date" [])
         , ((0, xF86XK_Battery),    messageCmd "/usr/bin/acpi" ["-b"])
         , ((0, xF86XK_Mail),       mpd MPDStatus)
         , ((0, xF86XK_AudioNext),  mpd MPDNext)
         , ((0, xF86XK_AudioPrev),  mpd MPDPrev)
         , ((0, xF86XK_AudioPlay),  mpd MPDToggle)
           -- Move focus between windows
         , ((m, xK_Tab),            windows W.focusDown)
         , ((m .|. s, xK_Tab),      windows W.swapDown)
           -- Manage layout
         , ((m, xK_s),              sendMessage VerticalSplit)
         , ((m .|. s, xK_s),        sendMessage HorizontalSplit)
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
           --  - screenshot command
           --  - something similar to stumpwm's fullscreen command
         ]
         ++
         -- Move between workspaces (bepo layout :])
         [((m, key), windows $ W.view workspace)
              | (workspace, key) <- zip (XMonad.workspaces conf)
                                        myWorkspaceKeys]
         ++
         -- Move windows between workspaces
         [((m .|. s, key), windows $ W.shift workspace)
              | (workspace, key) <- zip (XMonad.workspaces conf)
                                        myWorkspaceKeys]
           where s = shiftMask

main = xmonad defaultConfig
        { modMask = myModMask
        , terminal = myTerminal
        , focusFollowsMouse = myFocusFollowsMouse
        , workspaces = myWorkspaces
        , layoutHook = myLayout
        , keys = myKeys
        }
