-- dzen2 plus conky config with urgency for xmonad-0.9*
-- uses icons from dzen.geekmode.org
import XMonad
import XMonad.Core
 
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Man
 
import XMonad.Layout
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
 
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
 
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Graphics.X11.Xlib
import XMonad.Util.Run
import XMonad.Actions.CycleWS
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.ToggleLayouts
import Data.Ratio ((%))
 
main = do
  myStatusBarPipe <- spawnPipe myStatusBar
  spawn "nitrogen --restore"
--  spawn myCPUBar
--  spawn myBatteryBar
  spawn myTimeBar
--  spawn myXxkbBar
  spawn myXsetRate
  --xmonad $ myUrgencyHook $ defaultConfig {
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
    terminal = myTerminal,
    borderWidth = 3,
    normalBorderColor = myInactiveBorderColor,
    focusedBorderColor = myActiveBorderColor,
    workspaces = myWorkspaces,
    modMask = myModMask,
    keys = myKeys,
    manageHook = myManageHook <+> manageDocks <+> manageHook defaultConfig,
    layoutHook = myLayoutHook,
    startupHook = myStartupHook,
    logHook = do dynamicLogWithPP $ myDzenPP myStatusBarPipe
                 myFadingHook
    }
 
-- Fonts
myFont = "xft:Terminus:size=12"
mySmallFont = "xft:Terminus:size=10"
 
-- Paths
myBitmapsPath = "/home/catalin/.xmonad/dzen/bitmaps/"
 
-- Colors
myFgColor = "#444444"
myBgColor = "#e7e7e7"
 
myHighlightedFgColor = "#fefefe"
myHighlightedBgColor = "#d97600"
 
myActiveBorderColor = "#d97600"
myInactiveBorderColor = "#dae3e0"
 
myCurrentWsFgColor = "white"
--myCurrentWsBgColor = "gray40"
myCurrentWsBgColor = "#d97600"
myVisibleWsFgColor = "#444444"
myVisibleWsBgColor = "#e7e7e7"
myHiddenWsFgColor = "#4cbb17"
myHiddenEmptyWsFgColor = "#444444"
myUrgentWsBgColor = "brown"
myTitleFgColor = "#d97600"
 
myUrgencyHintFgColor = "white"
myUrgencyHintBgColor = "brown"
 
-- Bars
myDzenBarGeneralOptions = "-h 24 -fn '" ++ myFont ++ "' -fg '" ++ myFgColor ++
                          "' -bg '" ++ myBgColor ++ "'"
 
myStatusBar = "dzen2 -w 1500 -ta l " ++ myDzenBarGeneralOptions
myCPUBar = "conky -c ~/.xmonad/conky_cpu | sh | dzen2 -x 1300 -w 90 -ta l " ++
           myDzenBarGeneralOptions
myBatteryBar = "conky -c ~/.xmonad/conky_battery | sh | dzen2 -x 1046 -w 63 -ta l " ++
               myDzenBarGeneralOptions
myTimeBar = "conky -c ~/.xmonad/conky_time | dzen2 -x 1500 -w 180 -ta c " ++
            myDzenBarGeneralOptions
myXxkbBar = "xxkb" -- configuration in ~/.xxkbrc
myXsetRate = "xset r rate 250 30"
 
-- Prefered terminal
myTerminal = "terminal"
 
-- Rebind Mod to Windows key
myModMask = mod1Mask
 
-- Prompt config
myXPConfig = defaultXPConfig {
  position = Bottom,
  promptBorderWidth = 0,
  font = myFont,
  height = 15,
  bgColor = myBgColor,
  fgColor = myFgColor,
  fgHLight = myHighlightedFgColor,
  bgHLight = myHighlightedBgColor
  }
 
-- Union default and new key bindings
myKeys x  = M.union (M.fromList (newKeys x)) (keys defaultConfig x)
 
-- Add new and/or redefine key bindings
newKeys conf@(XConfig {XMonad.modMask = modm}) = [
  -- Use shellPrompt instead of default dmenu
  ((modm, xK_r), shellPrompt myXPConfig),
  -- Do not leave useless conky, dzen and xxkb after restart
  ((modm, xK_q), spawn "killall conky dzen2 xxkb xcompmgr; xmonad --recompile; xmonad --restart"),
  -- poweroff
  ((modm .|. shiftMask, xK_q), spawn "sudo poweroff"),
  -- ResizableTall key bindings
  ((modm, xK_a), sendMessage MirrorShrink),
  ((modm, xK_z), sendMessage MirrorExpand),
  -- Manual page prompt
  ((modm, xK_o), manPrompt myXPConfig),
  ((modm, xK_u), focusUrgent),
  -- Make a screeshot
  ((0,           xK_Print), spawn "scrot -e 'mv $f ~/tmp/'"),
  ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s -e 'mv $f ~/tmp/'")
  -- workspaces
  , ((modm, xK_u), prevWS)
  , ((modm, xK_i), nextWS)
  , ((modm .|. shiftMask, xK_u), shiftToPrev)
  , ((modm .|. shiftMask, xK_i), shiftToNext)

  , ((modm, xK_Return), spawn $ XMonad.terminal conf) -- spawn terminal
  , ((modm, xK_c), kill) -- close focused window
  , ((mod1Mask, xK_Tab   ), windows W.focusDown)
  ]
 
-- Workspaces names
supWsNum wsName wsNum =" " ++ wsName ++  "^p(;_TOP)^fn(" ++ mySmallFont  ++ ")" ++ wsNum ++ "  ^fn()^p()"
gtdTag    = supWsNum "1" "gtd"
webTag    = supWsNum "2" "web"
ideTag    = supWsNum "3" "ide"
myWorkspaces = [
  gtdTag,
  webTag,
  ideTag,
  supWsNum "4" "read",
  supWsNum "5" "",
  supWsNum "6" ""
  ]
--  where
--    supWsNum wsName wsNum =" " ++ wsName ++  "^p(;_TOP)^fn(" ++ mySmallFont  ++ ")" ++ wsNum ++ "  ^fn()^p()"
 
-- Dzen config
--myDzenPP h = defaultPP {
myDzenPP h = dzenPP {
  ppOutput = hPutStrLn h,
  ppSep = "",
  ppWsSep = "",
  ppCurrent = wrapFgBg myCurrentWsFgColor myCurrentWsBgColor,
  ppVisible = wrapFgBg myVisibleWsFgColor myVisibleWsBgColor,
  ppHidden = wrapFg myHiddenWsFgColor,
  ppHiddenNoWindows = wrapFg myHiddenEmptyWsFgColor,
  ppUrgent = wrapBg myUrgentWsBgColor,
  ppTitle = (\x -> " " ++ wrapFg myTitleFgColor x),
  ppLayout  = dzenColor myFgColor"" .
                (\x -> case x of
                    "ResizableTall" -> wrapBitmap "rob/tall.xbm"
                    "Mirror ResizableTall" -> wrapBitmap "rob/mtall.xbm"
                    "Full" -> wrapBitmap "rob/full.xbm"
                )
  }
  where
    wrapFgBg fgColor bgColor content= wrap ("^fg(" ++ fgColor ++ ")^bg(" ++ bgColor ++ ")") "^fg()^bg()" content
    wrapFg color content = wrap ("^fg(" ++ color ++ ")") "^fg()" content
    wrapBg color content = wrap ("^bg(" ++ color ++ ")") "^bg()" content
    wrapBitmap bitmap = "^p(5)^i(" ++ myBitmapsPath ++ bitmap ++ ")^p(5)"
 
-- Fading hook
myFadingHook = fadeInactiveLogHook fadeAmount
  where fadeAmount = 0.85
 
-- Define a combination of layouts
--myLayoutHook = smartBorders $ (tiled ||| Mirror tiled ||| Full)
myLayoutHook = avoidStruts . smartBorders
        $ onWorkspace gtdTag   (Full
                                ||| tiled
                                ||| Mirror tiled
                                )
        $ onWorkspace webTag     (tiled
                                ||| Mirror tiled
                                ||| Full
                                )
        $ onWorkspace ideTag     (tiledfat
                                ||| Mirror tiledfat
                                ||| tiledfat
                                )
        $ tiled
            ||| Mirror tiled
            ||| Full
  where
    tiled = ResizableTall nmaster delta ratio []
    tiledfat = ResizableTall nmaster delta ratiofat []
    nmaster = 1
    delta = 3/100
    ratio = 1%2
    ratiofat = 3%5
 
-- Urgency hint configuration
--myUrgencyHook = withUrgencyHook dzenUrgencyHook
--    {
--      args = [
--         "-x", "0", "-y", "785", "-h", "15", "-w", "1280",
--         "-ta", "r", "-expand", "l",
--         "-fg", "" ++ myUrgencyHintFgColor ++ "",
--         "-bg", "" ++ myUrgencyHintBgColor ++ "",
--         "-fn", "" ++ myFont ++ ""
--         ]
--      --duration = (7 `seconds`)
--    }
 
-- Window rules
--myManageHook = composeAll . concat $ [
--  [isDialog --> doFloat],
--  [className =? c --> doFloat | c <- myCFloats],
--  [title =? t --> doFloat | t <- myTFloats],
--  [resource =? r --> doFloat | r <- myRFloats],
--  [resource =? i --> doIgnore | i <- myIgnores]
--  ]
--  where
--    myCFloats = ["Xmessage"]
--    myTFloats = ["Save As...", "Save File", "Options"]
--    myRFloats = []
--    myIgnores = ["XXkb"]

myStartupHook = do spawn "/home/catalin/bin/myPrismApps"
                   spawn "firefox"

myManageHook :: ManageHook 
myManageHook = (composeAll . concat $
    [ [resource     =? r            --> doIgnore            |   r   <- myIgnores] -- ignore desktop
    , [className    =? c            --> doShift  webTag     |   c   <- myWebs   ] -- move webs to webs
    , [className    =? c            --> doShift  gtdTag     |   c   <- myGtd    ] -- move chats to chats
    , [className    =? c            --> doCenterFloat       |   c   <- myFloats ] -- float my floats
    , [name         =? n            --> doCenterFloat       |   n   <- myNames  ] -- float my names
    , [isFullscreen                 --> myDoFullFloat                           ]
    ]) 

    where

        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"

        -- classnames
        myFloats  = ["gimp", "inkscape", "MPlayer","Zenity","VirtualBox","Xmessage","Save As...","XFontSel","Downloads","Nm-connection-editor","Pidgin"]
        myWebs    = ["Google-chrome","Chromium","Vimperator","Namoroka","Swiftfox"]
        myGtd     = ["Prism"]

        -- resources
        myIgnores = ["desktop","desktop_window","notify-osd","stalonetray","trayer","dzen2"]

        -- names
        myNames   = ["bashrun","Google Chrome Options","Chromium Options"]

-- a trick for fullscreen but stil allow focusing of other WSs
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat
