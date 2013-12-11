{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
import Control.Exception (catch)
import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.List as L
import qualified Data.Set as S
import Data.Word
import Debug.Trace
import Graphics.X11.ExtraTypes.XF86
import qualified Network.MPD as MPD
import System.Environment (getEnv)
import XMonad
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.Volume (toggleMute, raiseVolume, lowerVolume)
import XMonad.Layout.Gaps (gaps, Direction2D(..))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Prompt
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.XMonad (xmonadPrompt)
import qualified XMonad.StackSet as W
import XMonad.Util.Dzen
import XMonad.Util.Run

-- For debugging purposes
t a = traceShow a a

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

-- Unsplit the current frame
data Unsplit = Unsplit deriving Typeable

instance Message Unsplit

-- Move focus between frames
data Focus = FocusLeft | FocusDown | FocusUp | FocusRight deriving Typeable

instance Message Focus

-- Move windows between frames
data Move = MoveLeft | MoveDown | MoveUp | MoveRight deriving Typeable

instance Message Move

-- Swap focused windows, staying on the same frame
data FocusWin = FocusNext | FocusPrev deriving Typeable

instance Message FocusWin

-- Add or remove a window from the layout
data WinEvent = NewWindow Window | RemovedWindow Window deriving Typeable

instance Message WinEvent

-- A static rectangle represents the space a window takes on the
-- screen, but it is not linked to the screen resolution
data StaticRectangle =
    StaticRectangle { rx :: Rational
                    , ry :: Rational
                    , rw :: Rational
                    , rh :: Rational }
    deriving (Show, Read, Eq)

instance Ord StaticRectangle where
    compare r1 r2 = mconcat [ compare (rx r1) (rx r2)
                            , compare (ry r1) (ry r2)
                            , compare (rw r1) (rw r2)
                            , compare (rh r1) (rh r2)
                            ]

-- Full screen static rectangle
fsRect :: StaticRectangle
fsRect = StaticRectangle { rx = 0, ry = 0, rw = 1, rh = 1 }

-- Split a rectangle in a given direction
rectSplit :: StaticRectangle -> Split -> (StaticRectangle, StaticRectangle)
rectSplit r VerticalSplit =
    (r { rh = h },
     r { rh = h
       , ry = h })
    where h = (rh r) / 2
rectSplit r HorizontalSplit =
    (r { rw = w },
     r { rw = w
       , rx = w })
    where w = (rw r) / 2

-- Scales a X rectangle according to a static rectangle
-- TODO: will not be correct if Rectangle does not start at 0, 0 (see
-- if it can happen when using multiple screens
rectScale :: Rectangle -> StaticRectangle -> Rectangle
rectScale xr r  =
    Rectangle { rect_x = rx r ** rect_width xr
              , rect_y = ry r ** rect_height xr
              , rect_width = rw r ** rect_width xr
              , rect_height = rh r ** rect_height xr }
    where x ** y = round (x * (fromIntegral y))

rectContains :: StaticRectangle -> Rational -> Rational -> Bool
rectContains r x y = rx r <= x && ry r <= y &&
                     (rx r + rw r) > x && (ry r + rh r) > y

data StaticLayout a =
    StaticLayout { -- Contains the frames that are not focused (each
                   -- one is associated with one possible window)
                   frames :: M.Map StaticRectangle (Maybe a)
                   -- The currently focused frame (represented by the
                   -- StaticRectangle), associated with a stack of
                   -- windows, one of theim being focused, the other
                   -- ones being hidden
                 , current :: (Maybe (W.Stack a), StaticRectangle)
                 }
    deriving (Show, Read)

emptyStaticLayout :: StaticLayout a
emptyStaticLayout =
    StaticLayout { frames = M.empty
                 , current = (Nothing, fsRect)
                 }

-- Finds the rectangle that contains a position in the layout (don't
-- look into the current frame)
findRect :: StaticLayout a -> Rational -> Rational -> Maybe StaticRectangle
findRect (StaticLayout { frames = f }) x y =
    if M.size remaining == 0
    then Nothing
    else Just (fst (M.elemAt 0 remaining))
    where remaining = M.filterWithKey (\r _ -> rectContains r x y) f

split :: (Show a) => StaticLayout a -> Split -> StaticLayout a
split l s =
    -- TODO: can put a hidden window in the new frame (r2)
    l { frames = M.insert r2 Nothing $
                 frames l
      , current = (stack, r1) }
    where (stack, r) = current l
          (r1, r2) = rectSplit r s

addWindow :: StaticLayout a -> a -> StaticLayout a
addWindow l w = l { current = (Just (W.Stack { W.focus = w
                                             , W.up = newUp
                                             , W.down = newDown
                                             }),
                               (snd (current l)))
                  }
    where newUp = maybe [] (\s -> (W.focus s):(W.up s)) stack
          newDown = maybe [] W.down stack
          stack = fst (current l)

removeWindow :: StaticLayout a -> a -> StaticLayout a
removeWindow l w = error "TODO"

instance LayoutClass StaticLayout Window where
    description _ = "Static"

    pureLayout l r s = t $ mapMaybe (\(mw, r) -> fmap (\w -> (w, r)) mw) $
                       cur:rest
        where cur = (fmap W.focus stack, rectScale r frame)
              rest = map (\(rect, w) -> (w, rectScale r rect)) $
                     M.toList $ frames l
              (stack, frame) = current l

    emptyLayout l r = return ([], Just l)

    handleMessage l m = do
      return $ msum [ fmap (split l) $ fromMessage m
                    , fmap xmessage $ fromMessage m
                    ]
        where xmessage (NewWindow w) = addWindow l w
              xmessage (RemovedWindow w) = removeWindow l w
              printMessage (NewWindow w) = putStrLn "new"
              printMessage (RemovedWindow w) = putStrLn "removed"

-- Overrides XMonad's default event hook to add some behaviour (mainly
-- sending messages on some actions). Most of the code is unshamefully
-- taken from XMonad original source code
myHandle :: Event -> X All
myHandle (MapRequestEvent {ev_window = w}) = withDisplay $ \dpy -> do
    wa <- io $ getWindowAttributes dpy w -- ignore override windows
    -- need to ignore mapping requests by managed windows not on the
    -- current workspace
    managed <- isClient w
    when (not (wa_override_redirect wa) && not managed) $ do
      sendMessage (NewWindow w)
      manage w
    return (All False) -- Don't run XMonad's handle

myHandle (DestroyWindowEvent {ev_window = w}) = do
  whenX (isClient w) $ do
    unmanage w
    sendMessage (RemovedWindow w)
    modify (\s -> s { mapped       = S.delete w (mapped s)
                    , waitingUnmap = M.delete w (waitingUnmap s)})
  return (All False) -- Don't run XMonad's handle

myHandle (UnmapEvent {ev_window = w, ev_send_event = synthetic}) = do
  whenX (isClient w) $ do
    e <- gets (fromMaybe 0 . M.lookup w . waitingUnmap)
    if (synthetic || e == 0)
        then unmanage w >> sendMessage (RemovedWindow w)
        else modify (\s -> s { waitingUnmap = M.update mpred w (waitingUnmap s) })
  return (All False) -- Don't run XMonad's handle
 where mpred 1 = Nothing
       mpred n = Just $ pred n

myHandle _ = return (All True)

-- Like getEnv, but exception-less, with a default value instead
getEnv' :: String -> String -> IO String
getEnv' var def = getEnv var `catch` \(_ :: IOError) -> return def

-- Like head, but don't crash on failure
safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

-- Overwrite some parameter in default graphical configurations

myXPConfig = defaultXPConfig

myDzenConfig = (onCurr (vCenter 25)) >=> (timeout 1)

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

-- Some commands to display info or launch stuff
displayDate = messageCmd "/usr/bin/date" []
displayBattery = messageCmd "/usr/bin/acpi" ["-b"]
lockScreen = safeSpawn "/usr/bin/xlock"
             ["-mode", "blank",
              "-use3d", "-font", "-*-erusfont-*-*-*-*-*-*-*-*-*-*-*-*",
              "+description", "-info", " ",
              "-fg", "grey55", "-echokeys", "-echokey", "*",
              "+usefirst", "-icongeometry", "0x0"]
displayVolume = message . (++ "%") . ("Volume: " ++) . show . ceiling
volumeDown = lowerVolume 4 >>= displayVolume
volumeUp = raiseVolume 4 >>= displayVolume
volumeToggle = toggleMute >>= message . ("Volume: " ++) . show

-- Basing changes of XMonad defaults

myModMask = mod4Mask
myTerminal = "urxvt"
myFocusFollowsMouse = False
myClickJustFocuses = False
myBorderWidth = 3
myNormalBorderColor = "black"
myFocusedBorderColor = "white"
myWorkspaces = map show [1..10]
myLayoutHook = gaps [(D, 2)] $
               smartBorders $
               tiled ||| Mirror tiled ||| Full
    where tiled = Tall 1 (5/100) (2/3)

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
           ((m, xK_e),               runOrRaise "emacs"   (className =? "Emacs"))
         -- Launch firefox, or just focus it
         , ((m, xK_f),               runOrRaise "firefox" (className =? "Firefox"))
         -- Launch terminal
         , ((m, xK_Return),          spawn (XMonad.terminal conf))
         -- Launch command prompt
         , ((m, xK_comma),           shellPrompt myXPConfig)
         -- Launch XMonad prompt
         , ((m .|. s , xK_comma),    xmonadPrompt myXPConfig)
         -- Various control & information (time, mpd, ...)
         , ((0, xF86XK_Calculator),  displayDate)
         , ((0, xF86XK_Launch1),     displayDate)
         , ((0, xF86XK_Battery),     displayBattery)
         , ((0, xF86XK_ScreenSaver), lockScreen)
         , ((0, xF86XK_Mail),        mpd MPDStatus)
         , ((0, xF86XK_AudioNext),   mpd MPDNext)
         , ((0, xF86XK_AudioPrev),   mpd MPDPrev)
         , ((0, xF86XK_AudioPlay),   mpd MPDToggle)
         , ((0, xF86XK_AudioRaiseVolume), volumeUp)
         , ((0, xF86XK_AudioLowerVolume), volumeDown)
         , ((0, xF86XK_AudioMute),   volumeToggle)
         -- Move focus between windows
         , ((m, xK_Tab),             windows W.focusDown)
         , ((m .|. s, xK_Tab),       windows W.swapDown)
         -- Static layout related stuff
         -- Move focus between frames
         {-
         , ((m, xK_c),               sendMessage FocusLeft)
         , ((m, xK_t),               sendMessage FocusDown)
         , ((m, xK_s),               sendMessage FocusUp)
         , ((m, xK_r),               sendMessage FocusRight)
         -- Change displayed window inside a frame
         , ((m, xK_n),               sendMessage FocusNext)
         , ((m, xK_p),               sendMessage FocusPrev)
         -- Move frames
         , ((m .|. s, xK_c),         sendMessage MoveLeft)
         , ((m .|. s, xK_t),         sendMessage MoveDown)
         , ((m .|. s, xK_s),         sendMessage MoveUp)
         , ((m .|. s, xK_r),         sendMessage MoveRight)
         -- Manage frames
         , ((m, xK_v),               sendMessage VerticalSplit)
         , ((m, xK_d),               sendMessage HorizontalSplit)
         , ((m, xK_l),               sendMessage Unsplit)
         -}
         -- Dynamic layout conf
         , ((m, xK_n),               windows W.focusDown)
         , ((m .|. s, xK_n),         windows W.swapDown)
         , ((m, xK_p),               windows W.focusUp)
         , ((m .|. s, xK_p),         windows W.swapUp)
         , ((m, xK_m),               windows W.focusMaster)
         , ((m .|. s, xK_m),         windows W.swapMaster)
         , ((m, xK_c),               sendMessage Shrink)
         , ((m, xK_r),               sendMessage Expand)
         , ((m, xK_g),               withFocused $ windows . W.sink)
         , ((m, xK_v),               sendMessage (IncMasterN 1))
         , ((m, xK_d),               sendMessage (IncMasterN (-1)))
         , ((m, xK_space),           sendMessage NextLayout)
         , ((m .|. s, xK_space),     setLayout $ XMonad.layoutHook conf)
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
        , clickJustFocuses = myClickJustFocuses
        , borderWidth = myBorderWidth
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , workspaces = myWorkspaces
        , layoutHook = myLayoutHook
        -- , handleEventHook = myHandle
        , keys = myKeys
        }
