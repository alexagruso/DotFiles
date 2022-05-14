--
--        _                                              
--   __ _| | _____  __   __ _  __ _ _ __ _   _ ___  ___  
--  / _` | |/ _ \ \/ /  / _` |/ _` | '__| | | / __|/ _ \ 
-- | (_| | |  __/>  <  | (_| | (_| | |  | |_| \__ \ (_) |
--  \__,_|_|\___/_/\_\  \__,_|\__, |_|   \__,_|___/\___/ 
--                            |___/ 
-- 

import qualified Codec.Binary.UTF8.String as UTF8

                   
import qualified DBus as D
import qualified DBus.Client as D

import Data.Monoid
import qualified Data.Map as M

import System.Exit

import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Layout.Spacing

import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import qualified XMonad.StackSet as W

currentIcon :: String -> String
currentIcon _ = "\xf111"

hiddenIcon :: String -> String
hiddenIcon _ = "\xf192"

hiddenNoWindowsIcon :: String -> String
hiddenNoWindowsIcon _ = "\xf10c"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth :: Dimension
myBorderWidth = 3

shift :: KeyMask
shift = shiftMask

control :: KeyMask
control = controlMask

super :: KeyMask
super = mod4Mask

alt :: KeyMask
alt = mod1Mask

myNormalBorderColor :: String
myNormalBorderColor = "#ff0000"

myFocusedBorderColor :: String
myFocusedBorderColor = "#1111ff"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- move focus
    [ ((super,                     xK_j     ), windows W.focusDown)
    , ((super,                     xK_k     ), windows W.focusUp)
    , ((super,                     xK_Return), windows W.focusMaster)

    -- move windows
    , ((super .|. shift,           xK_Return), windows W.swapMaster)
    , ((super .|. shift,           xK_j     ), windows W.swapDown  )
    , ((super .|. shift,           xK_k     ), windows W.swapUp    )

    -- open applications
    , ((super .|. alt,             xK_Return), spawn $ XMonad.terminal conf)
    , ((super .|. alt,             xK_p     ), spawn "pulsemixer")
    , ((super .|. alt,             xK_b     ), spawn "brave-bin")
    , ((super .|. alt,             xK_n     ), spawn "pcmanfm")
    , ((super .|. alt,             xK_m     ), spawn "musescore")
    , ((super .|. alt,             xK_space ), kill)

    -- wm actions
    , ((control .|. super,         xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((control .|. super,         xK_space ), sendMessage NextLayout)
    , ((control .|. super,         xK_h     ), sendMessage Shrink)
    , ((control .|. super,         xK_l     ), sendMessage Expand)
    , ((control .|. super,         xK_f     ), withFocused $ windows . W.sink)
    , ((control .|. super,         xK_comma ), sendMessage (IncMasterN (-1)))
    , ((control .|. super,         xK_period), sendMessage (IncMasterN 1))
    , ((control .|. super,         xK_b     ), sendMessage ToggleStruts)
    , ((control .|. super,         xK_j     ), spawn "pulsemixer --change-volume -5")
    , ((control .|. super,         xK_k     ), spawn "pulsemixer --change-volume +5")
    , ((control .|. super,         xK_m     ), spawn "pulsemixer --toggle-mute")

    -- system actions
    , ((control .|. super .|. alt, xK_p     ), io (exitWith ExitSuccess))
    , ((control .|. super .|. alt, xK_n     ), spawn "sudo reboot")
    , ((control .|. super .|. alt, xK_m     ), spawn "sudo halt")
    , ((control .|. super .|. alt, xK_Return), spawn "pkill polybar ; xmonad --recompile ; xmonad --restart")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_s, xK_d, xK_f, xK_x, xK_c, xK_v]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shift)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

myManageHook = composeAll
    [ appName =? "pcmanfm" --> doCenterFloat
    ]

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

myLogHook :: D.Client -> PP
myLogHook dbus = def
    { ppOutput          = dbusOutput dbus
    , ppOrder           = \(workspace:layout:_) -> [workspace, layout]
    , ppCurrent         = wrap "" " " . currentIcon
    , ppHidden          = wrap "" " " . hiddenIcon
    , ppHiddenNoWindows = wrap "" " " . hiddenNoWindowsIcon
    }

myStartupHook :: X ()
myStartupHook = do
    setWMName "XMonad"
    spawn     "nitrogen --restore"
    spawn     "picom --experimental-backends"
    spawn     "polybar"

main :: IO ()
main = do
    dbus <- D.connectSession

    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

    xmonad $ docks $ defaultConfig
        { terminal           = "kitty"
        , focusFollowsMouse  = myFocusFollowsMouse
        , borderWidth        = myBorderWidth
        , modMask            = super
        , workspaces         = ["1","2","3","4","5","6"]
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor

        , keys               = myKeys
        , mouseBindings      = myMouseBindings

        , layoutHook         = spacingWithEdge 7 $ myLayout
        , manageHook         = myManageHook
        , handleEventHook    = mempty
        , logHook            = dynamicLogWithPP (myLogHook dbus)
        , startupHook        = myStartupHook
        } 
