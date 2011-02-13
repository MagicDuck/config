-- Imports {{{
import XMonad
-- Prompt
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.AppendFile (appendFilePrompt)
-- Hooks
import XMonad.Operations

import System.IO
import System.Exit

import XMonad.Util.Run

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.IM
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.Gaps

import XMonad.Actions.CycleWS

import qualified XMonad.StackSet as W
import qualified Data.Map as M

--}}}

-- Config {{{
-- Define Terminal
myTerminal      = "urxvt"
-- Define modMask
modMask' :: KeyMask
modMask' = mod4Mask
-- Define workspaces
myWorkspaces    = ["1:www","2:dev","3:vbox","4:main","5:void","6:chat"]
-- Dzen config
myStatusBarLeft = "dzen2 -x '0' -y '0' -h '16' -w '960' -ta 'l' -fg '#cccccc' -bg '#111111' -fn 'ubuntu:size=9'"
myStatusBarRight = "conky -c /home/bmartin/.xmonad/conky_bottom_dzen | dzen2 -x '960' -y '0' -w '960' -h '16' -ta 'r' -bg '#111111' -fg '#cccccc' -fn 'ubuntu:size=9'"
myBitmapsDir = "/home/bmartin/.xmonad/dzen"
--}}}
-- Main {{{
main = do
    dzenTopBar <- spawnPipe myStatusBarLeft
    dzenBtmBar <- spawnPipe myStatusBarRight
    spawn "sh /home/bmartin/.xmonad/autostart.sh"
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
      { terminal            = myTerminal
      , workspaces          = myWorkspaces
      , keys                = keys'
      , modMask             = modMask'
      , startupHook         = ewmhDesktopsStartup >> setWMName "LG3D"
      , layoutHook          = layoutHook'
      , manageHook          = manageHook'
      , logHook             = myLogHook dzenTopBar >> fadeInactiveLogHook 0xdddddddd  >> setWMName "LG3D"
      , normalBorderColor   = "#444444"
      , focusedBorderColor  = "#bf7b26"
}
--}}}


-- Hooks {{{
-- ManageHook {{{
manageHook' :: ManageHook 
manageHook' = (composeAll . concat $
    [ [resource     =? r            --> doIgnore            |   r   <- myIgnores] -- ignore desktop
    , [resource     =? r            --> doShift  "2:dev"    |   r   <- myDevs   ] -- move devs to devs
    , [className    =? c            --> doShift  "1:www"    |   c   <- myWebs   ] -- move webs to webs
    , [className    =? c            --> doShift  "6:chat"   |   c   <- myChats  ] -- move chats to chats
    , [className    =? c            --> doCenterFloat       |   c   <- myFloats ] -- float my floats
    , [name         =? n            --> doCenterFloat       |   n   <- myNames  ] -- float my names
    , [isFullscreen                 --> myDoFullFloat                           ]
    ]) 

    where

        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"

        -- classnames
        myFloats  = ["MPlayer","Zenity","VirtualBox","Xmessage","Save As...","XFontSel","Downloads","Nm-connection-editor","Pidgin"]
        myWebs    = ["Google-chrome","Chromium"]
        myDevs    = ["Redcar","gvim","tmuxdev"]
        myChats   = ["Pidgin"]

        -- resources
        myIgnores = ["desktop","desktop_window","notify-osd","stalonetray","trayer","dzen2"]

        -- names
        myNames   = ["bashrun","Google Chrome Options","Chromium Options"]

-- a trick for fullscreen but stil allow focusing of other WSs
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat
-- }}}
layoutHook' = customLayout

-- Bar
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor "#cccccc" "#111111" . pad
      , ppVisible           =   dzenColor "#bf7b26" "#111111" . pad
      , ppHidden            =   dzenColor "#bf7b26" "#111111" . pad
      , ppHiddenNoWindows   =   dzenColor "#444444" "#111111" . pad
      , ppUrgent            =   dzenColor "#ff0000" "#111111" . pad . dzenStrip
      , ppWsSep             =   " "
      , ppSep               =   "  |  "
      , ppLayout            =   dzenColor "#bf7b26" "#111111" .
                                (\x -> case x of
                                    "ResizableTall"             ->      "^i(" ++ myBitmapsDir ++ "/tall.xbm)"
                                    "Mirror ResizableTall"      ->      "^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                                    "Full"                      ->      "^i(" ++ myBitmapsDir ++ "/full.xbm)"
                                    "Simple Float"              ->      "~"
                                    _                           ->      x
                                )
      , ppTitle             =   (" " ++) . dzenColor "#cccccc" "#111111" . dzenEscape
      , ppOutput            =   hPutStrLn h
    }
-- Layout
customLayout =  avoidStruts $ smartBorders tiled ||| smartBorders (Mirror tiled)  ||| noBorders Full ||| smartBorders simpleFloat
  where
    --tiled = ResizableTall 1 (2/100) (1/2) []
    tiled   = ResizableTall nmaster delta ratio []
    nmaster = 1   
    delta   = 2/100
    ratio   = 1/2
--}}}
-- Theme {{{
-- Color names are easier to remember:
colorOrange          = "#ff7701"
colorDarkGray        = "#171717"
colorPink            = "#e3008d"
colorGreen           = "#00aa4a"
colorBlue            = "#008dd5"
colorYellow          = "#fee100"
colorWhite           = "#cfbfad"
 
colorNormalBorder    = "#1c2636"
colorFocusedBorder   = "#ebac54"
barFont  = "Meslo LG M:size=10"
barXFont = "ubuntu:size=10"
xftFont = "xft: ubuntu-10"
--}}}

-- Prompt Config {{{
mXPConfig :: XPConfig
mXPConfig =
    defaultXPConfig { font                  = barFont
                    , bgColor               = "#111111"
                    , fgColor               = "#cccccc"
                    , bgHLight              = "#bf7b26"
                    , fgHLight              = "#111111"
                    , promptBorderWidth     = 0
                    , height                = 16
                    , historyFilter         = deleteConsecutive
                    }
 
-- Run or Raise Menu
largeXPConfig :: XPConfig
largeXPConfig = mXPConfig
                { font = xftFont
                , height = 16
                }
-- }}}
-- Key mapping {{{
keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((mod1Mask,                   xK_x        ), runOrRaisePrompt largeXPConfig)
    , ((mod1Mask,                   xK_F2       ), spawn "gmrun")
    , ((0,                          xK_Print    ), spawn "screenshot scr")

    -- Programs
    , ((mod1Mask,                   xK_t        ), spawn $ XMonad.terminal conf) -- spawn terminal
    , ((mod1Mask,                   xK_d        ), spawn "urxvt -name tmuxdev -e tmux new -s dev vim")
    , ((mod1Mask .|. shiftMask,     xK_d        ), spawn "urxvt -name tmuxdev")
    , ((mod1Mask,                   xK_c        ), spawn "urxvt -name tmuxchat -e tmux new -s chat weechat-curses")
    , ((mod1Mask .|. shiftMask,     xK_c        ), spawn "urxvt -name tmuxchat")
    , ((mod1Mask,                   xK_w        ), spawn "chromium")
    , ((mod1Mask .|. shiftMask,     xK_w        ), spawn "firefox")
    , ((mod1Mask,                   xK_r        ), spawn "redcar")
    , ((mod1Mask,                   xK_e        ), spawn "thunderbird")
    , ((mod1Mask,                   xK_v        ), spawn "VirtualBox")
    , ((mod1Mask,                   xK_g        ), spawn "gvim")
    , ((mod1Mask,                   xK_f        ), spawn "thunar")
    , ((mod1Mask,                   xK_p        ), spawn "pidgin")
    , ((mod1Mask,                   xK_b        ), spawn "dbus-launch brasero")
    --, ((mod1Mask,                   xK_q        ), spawn "shutdown-dialog.py ")

    -- Media Keys
    , ((0,                          0x1008ff12  ), spawn "/home/bmartin/.bin/changevol -t") -- XF86AudioMute
    , ((0,                          0x1008ff11  ), spawn "/home/bmartin/.bin/changevol -d 5") -- XF86AudioLowerVolume
    , ((0,                          0x1008ff13  ), spawn "/home/bmartin/.bin/changevol -i 5") -- XF86AudioRaiseVolume

    -- layouts
    , ((modMask,                    xK_space    ), sendMessage NextLayout) -- Rotate through the available layout algorithms)
    , ((modMask .|. shiftMask,      xK_space    ), setLayout $ XMonad.layoutHook conf) -- reset layout on current desktop to default
    , ((modMask,                    xK_b        ), sendMessage ToggleStruts) -- Toggle the status bar gap. Use this binding with avoidStruts from Hooks.ManageDocks. See also the statusBar function from Hooks.DynamicLog. 
    , ((modMask,                    xK_n        ), refresh) -- Resize viewed windows to the correct size)
    , ((modMask,                    xK_Tab      ), windows W.focusDown) -- Move focus to the next window
    , ((modMask,                    xK_j        ), windows W.focusDown) -- Move focus to the next window
    , ((modMask,                    xK_k        ), windows W.focusUp  ) -- Move focus to the previous window
    , ((modMask,                    xK_m        ), windows W.focusMaster  ) -- Move focus to the master window
    , ((modMask,                    xK_Return   ), windows W.swapMaster) -- Swap the focused window and the master window
    , ((modMask .|. shiftMask,      xK_j        ), windows W.swapDown  ) -- Swap the focused window with the next window
    , ((modMask .|. shiftMask,      xK_k        ), windows W.swapUp    ) -- Swap the focused window with the previous window
    , ((modMask,                    xK_h        ), sendMessage Shrink) -- Shrink the master area
    , ((modMask,                    xK_l        ), sendMessage Expand) -- Expand the master area
    , ((modMask,                    xK_t        ), withFocused $ windows . W.sink) -- Push window back into tiling
    , ((modMask,                    xK_comma    ), sendMessage (IncMasterN 1)) -- Increment the number of windows in the master area
    , ((modMask,                    xK_period   ), sendMessage (IncMasterN (-1))) -- Deincrement the number of windows in the master area
    , ((modMask .|. shiftMask,      xK_c        ), kill) -- kill selected window

    -- workspaces
    , ((mod1Mask .|. controlMask,   xK_Right    ), nextWS)
    , ((mod1Mask .|. shiftMask,     xK_Right    ), shiftToNext)
    , ((mod1Mask .|. controlMask,   xK_Left     ), prevWS)
    , ((mod1Mask .|. shiftMask,     xK_Left     ), shiftToPrev)
    
    -- quit, or restart
    , ((modMask .|. shiftMask,      xK_q        ), io (exitWith ExitSuccess))
    , ((modMask              ,      xK_q        ), restart "xmonad" True)
    , ((modMask .|. shiftMask,      xK_r        ), spawn "killall conky dzen2 && xmonad --recompile && xmonad --restart")
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

--}}}

