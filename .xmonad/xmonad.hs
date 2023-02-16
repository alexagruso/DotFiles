import XMonad
import Data.Monoid
import System.Exit

import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing

import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- give mod#mask variables clearer names
altMask = mod1Mask
superMask = mod4Mask

-- space around each window, gap between windows is double this amount
windowGap = 6

-- keys are organized by function, e.g. moving windows, layout, applications, etc.
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- super: change window focus
    [ ((modm, xK_j), windows W.focusDown)
    , ((modm, xK_k), windows W.focusUp)
    , ((modm, xK_m), windows W.focusMaster)
    ]
    ++
    -- super + alt: open applications or close windows
    [ ((modm .|. altMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm .|. altMask, xK_space),  kill)
    , ((modm .|. altMask, xK_b),      spawn $ "brave-bin")
    , ((modm .|. altMask, xK_v),      spawn $ "vscode")
    , ((modm .|. altMask, xK_o),      spawn $ "openmpt")
    , ((modm .|. altMask, xK_t),      spawn $ "qbittorrent")
    , ((modm .|. altMask, xK_g),      spawn $ "godot")
    , ((modm .|. altMask, xK_f),      spawn $ "zoom")
    , ((modm .|. altMask, xK_m),      spawn $ "Musescore")
    , ((modm .|. altMask, xK_s),      spawn $ "scrot -s -z")
    ]
    ++
    -- super + control: change layouts 
    [ ((modm .|. controlMask, xK_space ),  sendMessage NextLayout)
    , ((modm .|. controlMask, xK_Return ), setLayout $ XMonad.layoutHook conf)
    , ((modm .|. controlMask, xK_c),       withFocused $ windows . W.sink)
    , ((modm .|. controlMask, xK_comma ),  sendMessage (IncMasterN 1))
    , ((modm .|. controlMask, xK_period),  sendMessage (IncMasterN (-1)))
    , ((modm .|. controlMask, xK_h),       sendMessage Shrink)
    , ((modm .|. controlMask, xK_l),       sendMessage Expand)
    , ((modm .|. controlMask, xK_j),       sendMessage ToggleStruts)
    ]
    ++
    -- super + shift:  moving windows
    [ ((modm .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j),      windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k),      windows W.swapUp    )
    ]
    ++
    -- super + alt + shift: system actions
    [ ((modm .|. controlMask .|. altMask, xK_n),      spawn $ "sudo reboot")
    , ((modm .|. controlMask .|. altMask, xK_m),      spawn $ "sudo halt")
    , ((modm .|. controlMask .|. altMask, xK_space),  io (exitWith ExitSuccess))
    , ((modm .|. controlMask .|. altMask, xK_Return),
        spawn $ "xmonad --recompile ; xmonad --restart")
    ]
    ++
    -- workspace bindings
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_s, xK_d, xK_f, xK_g, xK_v]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]


myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

myManageHook = composeAll
    [ (isFullscreen --> doFullFloat)
    ]

myLayout = avoidStruts $ (master ||| full)
  where
    -- default layout with master window
    master = spacingWithEdge windowGap $ Tall 1 (5/100) (1/2)
    -- fullscreen, no borders and cover panel
    full   = noBorders $ Full

myStartupHook = do
    spawnOnce $ "nitrogen --restore" -- wallpaper
    spawn     $ "picom"              -- compositor
    spawn     $ "polybar -r"         -- panel

main = do
    xmonad $ docks $ ewmh $ def
        { terminal           = "alacritty"
        , focusFollowsMouse  = True
        , borderWidth        = 2
        , modMask            = superMask
        , workspaces         = ["1", "2", "3", "4", "5"]
        , normalBorderColor  = "#004040"
        , focusedBorderColor = "#f00040"
        , manageHook         = myManageHook

        , keys               = myKeys
        , mouseBindings      = myMouseBindings

        , layoutHook         = myLayout
        , startupHook        = myStartupHook
        }
