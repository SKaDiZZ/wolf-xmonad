--------------------------------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs                                                                    --
-- validate syntax: xmonad --recompile                                                    --
--------------------------------------------------------------------------------------------
 
-- Misc
{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, TypeSynonymInstances, MultiParamTypeClasses #-}
 
-- Imported libraries
import XMonad
import XMonad.Core
import XMonad.Layout
import XMonad.Layout.IM
import XMonad.Layout.Gaps
import XMonad.Layout.Named
import XMonad.Layout.Tabbed
import XMonad.Layout.OneBig
import XMonad.Layout.Master
import XMonad.Layout.Reflect
import XMonad.Layout.MosaicAlt
import XMonad.Layout.NoBorders (noBorders,smartBorders,withBorder)
import XMonad.Layout.ResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Minimize
import XMonad.Layout.Spacing
import XMonad.StackSet (RationalRect (..), currentTag)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts,avoidStrutsOn,manageDocks)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Util.Cursor
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad (scratchpadManageHook, scratchpadSpawnActionCustom)
import XMonad.Util.NamedScratchpad
import XMonad.Actions.CycleWS (nextWS, prevWS, toggleWS, toggleOrView)
import XMonad.Actions.GridSelect
import XMonad.Actions.FloatKeys
import Data.Monoid
import Data.List
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO (Handle, hPutStrLn)
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified XMonad.Actions.FlexibleResize as Flex
 
-- Main
main :: IO ()
main = do
    workspaceBar            <- spawnPipe myWorkspaceBar
    bottomStatusBar         <- spawnPipe myBottomStatusBar
    topStatusBar            <- spawnPipe myTopStatusBar
    xmonad $ myUrgencyHook $ defaultConfig
        { terminal           = "urxvt"
        , modMask            = mod4Mask
        , focusFollowsMouse  = True
        , borderWidth        = 2
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , manageHook         = manageDocks <+> myManageHook
        , logHook            = (myLogHook workspaceBar) <+> ewmhDesktopsLogHook >> setWMName "LG3D" --ewmh needed so that chromium gain focus
        , handleEventHook    = fullscreenEventHook                                                  --needed for chromium full screen
        , keys               = myKeys
        , mouseBindings      = myMouseBindings
        , startupHook        = setDefaultCursor xC_left_ptr >> setWMName "LG3D"
        }
 
 
--------------------------------------------------------------------------------------------
-- APPEARANCE CONFIG                                                                      --
--------------------------------------------------------------------------------------------
 
-- Colors and fonts
myFont               = "Monaco:size=12"
dzenFont             = "AvantGarde LT Medium:size=7"
colorBlack           = "#1a1a1a" --Background (Dzen_BG)
colorBlackAlt        = "#404040" --Black Xdefaults
colorGray            = "#444444" --Gray       (Dzen_FG2)
colorGrayAlt         = "#161616" --Gray dark
colorWhite           = "#808080" --Foreground (Shell_FG)
colorWhiteAlt        = "#9d9d9d" --White dark (Dzen_FG)
colorMagenta         = "#8e82a2"
colorBlue            = "#87afd7"
colorYellow          = "#ffaf5f"
colorRed             = "#d75f5f"
colorGreen           = "#87af5f"
myArrow              = "^fg(" ++ colorWhiteAlt ++ ")>^fg(" ++ colorBlue ++ ")>^fg(" ++ colorGray ++ ")>"
myNormalBorderColor  = "#222222"
myFocusedBorderColor = "#d75f5f"
 
-- Tab theme
myTabTheme :: Theme
myTabTheme = defaultTheme
    { fontName            = myFont
    , inactiveBorderColor = colorBlackAlt
    , inactiveColor       = colorBlack
    , inactiveTextColor   = colorGray
    , activeBorderColor   = colorGray
    , activeColor         = colorBlackAlt
    , activeTextColor     = colorWhiteAlt
    , urgentBorderColor   = colorGray
    , urgentTextColor     = colorGreen
    , decoHeight          = 14
    }
 
-- Prompt theme
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
    { font                = myFont
    , bgColor             = colorBlack
    , fgColor             = colorWhite
    , bgHLight            = colorBlue
    , fgHLight            = colorWhite
    , borderColor         = colorGrayAlt
    , promptBorderWidth   = 1
    , height              = 16
    , position            = Top
    , historySize         = 100
    , historyFilter       = deleteConsecutive
    , autoComplete        = Nothing
    }
 
-- GridSelect color scheme
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
    (0x00,0x00,0x00) -- lowest inactive bg
    (0x60,0xA0,0xC0) -- highest inactive bg
    (0x34,0x75,0xAA) -- active bg
    (0xBB,0xBB,0xBB) -- inactive fg
    (0x00,0x00,0x00) -- active fg
 
-- GridSelect theme
myGSConfig :: t -> GSConfig Window
myGSConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight  = 50
    , gs_cellwidth   = 200
    , gs_cellpadding = 10
    , gs_font        = myFont
    }
 
-- Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["[1:TERM]", "[2:WEBS]", "[3:CODE]", "[4:GRFX]", "[5:CHAT]", "[6:GAME]", "[7:VIDS]", "[8:OTHR]"]
 
 
--------------------------------------------------------------------------------------------
-- LAYOUT CONFIG                                                                          --
--------------------------------------------------------------------------------------------
 
-- Layouts
myTile = named "T" $ spacing 7 $ smartBorders $ ResizableTall 1 0.03 0.5 []
myMirr = named "MT" $ spacing 7 $ smartBorders $ Mirror myTile
myMosA = named "M"  $ spacing 7 $ smartBorders $ MosaicAlt M.empty
myObig = named "O"  $ spacing 7 $ smartBorders $ OneBig 0.75 0.65
myTabs = named "TS" $ spacing 7 $ smartBorders $ tabbed shrinkText myTabTheme
myFull = named "TS" $ spacing 7 $ smartBorders $ tabbedAlways shrinkText myTabTheme
myTabM = named "TM" $ spacing 7 $ smartBorders $ mastered 0.01 0.4 $ tabbed shrinkText myTabTheme
myGimp = named "G"  $ spacing 7 $ withIM (0.15) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.20) (Role "gimp-dock") myMosA
myChat = named "C"  $ spacing 7 $ withIM (0.20) (Title "Buddy List") $ Mirror $ ResizableTall 1 0.03 0.5 []
 
-- Transformers (W+f)
data TABBED = TABBED deriving (Read, Show, Eq, Typeable)
instance Transformer TABBED Window where
    transform TABBED x k = k myFull (\_ -> x)
 
-- Layout hook
myLayoutHook = id
    $ gaps [(U,16), (D,16), (L,0), (R,0)]
    $ avoidStruts
    $ minimize
    $ mkToggle (single TABBED)
    $ mkToggle (single MIRROR)
    $ mkToggle (single REFLECTX)
    $ mkToggle (single REFLECTY)
    $ onWorkspace (myWorkspaces !! 1) webLayouts  --Workspace 1 layouts
    $ onWorkspace (myWorkspaces !! 2) codeLayouts --Workspace 2 layouts
    $ onWorkspace (myWorkspaces !! 3) gimpLayouts --Workspace 3 layouts
    $ onWorkspace (myWorkspaces !! 4) chatLayouts --Workspace 4 layouts
    $ allLayouts
    where
        allLayouts  = myTile ||| myObig ||| myMirr ||| myMosA ||| myTabM
        webLayouts  = myTabs ||| myTabM
        codeLayouts = myTabM ||| myTile
        gimpLayouts = myGimp
        chatLayouts = myChat
 
 
--------------------------------------------------------------------------------------------
-- MANAGE HOOK CONFIG                                                                     --
--------------------------------------------------------------------------------------------
 
-- Scratchpad (W+º)
manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect (0) (1/50) (1) (3/4))
scratchPad = scratchpadSpawnActionCustom "urxvtc -name scratchpad"
 
-- Manage hook
myManageHook :: ManageHook
myManageHook = (composeAll . concat $
    [ [resource     =? r     --> doIgnore                             | r <- myIgnores] --ignore desktop
    , [className    =? c     --> doShift (myWorkspaces !! 1)          | c <- myWebS   ] --move myWebS windows to workspace 1 by classname
    , [className    =? c     --> doShift (myWorkspaces !! 4)          | c <- myChatS  ] --move myChatS windows to workspace 4 by classname
    , [className    =? c     --> doShift (myWorkspaces !! 3)          | c <- myGfxS   ] --move myGfxS windows to workspace 4 by classname
    , [className    =? c     --> doShiftAndGo (myWorkspaces !! 5)     | c <- myGameS  ] --move myGameS windows to workspace 5 by classname and shift
    , [className    =? c     --> doShiftAndGo (myWorkspaces !! 7)     | c <- myOtherS ] --move myOtherS windows to workspace 5 by classname and shift
    , [className    =? c     --> doCenterFloat                        | c <- myFloatCC] --float center geometry by classname
    , [name         =? n     --> doCenterFloat                        | n <- myFloatCN] --float center geometry by name
    , [name         =? n     --> doSideFloat NW                       | n <- myFloatSN] --float side NW geometry by name
    , [className    =? c     --> doF W.focusDown                      | c <- myFocusDC] --dont focus on launching by classname
    , [isFullscreen          --> doFullFloat]
    ]) <+> manageScratchPad
    where
        doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws
        role            = stringProperty "WM_WINDOW_ROLE"
        name            = stringProperty "WM_NAME"
        myIgnores       = ["desktop","desktop_window"]
        myWebS          = ["Chromium","Firefox","Google-chrome","google-chrome"]
        myGfxS          = ["gimp-2.6", "Gimp-2.6", "Gimp", "gimp", "GIMP"]
        myChatS         = ["Pidgin", "Xchat"]
        myGameS         = ["hon-x86", "Heroes of Newerth"]
        myOtherS        = ["Amule", "Transmission-gtk"]
        myFloatCC       = ["MPlayer", "File-roller", "zsnes", "Gcalctool", "Exo-helper-1", "Gksu", "PSX", "Galculator", "Nvidia-settings", "XFontSel", "XCalc", "XClock", "Desmume", "Ossxmix", "Xvidcap", "Main", "Wicd-client.py", "com-mathworks-util-PostVMInit"]
        myFloatCN       = ["ePSXe - Enhanced PSX emulator", "Seleccione Archivo", "Config Video", "Testing plugin", "Config Sound", "Config Cdrom", "Config Bios", "Config Netplay", "Config Memcards", "About ePSXe", "Config Controller", "Config Gamepads", "Select one or more files to open", "Add media", "Choose a file", "Open Image", "File Operation Progress", "Firefox Preferences", "Preferences", "Search Engines", "Set up sync", "Passwords and Exceptions", "Autofill Options", "Rename File", "Copying files", "Moving files", "File Properties", "Replace", ""]
        myFloatSN       = ["Event Tester"]
        myFocusDC       = ["Event Tester", "Notify-osd"]
 
 
--------------------------------------------------------------------------------------------
-- STATUS BARS CONFIG                                                                     --
--------------------------------------------------------------------------------------------
 
-- UrgencyHook
myUrgencyHook = withUrgencyHook dzenUrgencyHook
    { args = ["-fn", dzenFont, "-bg", colorBlack, "-fg", colorGreen] }
 
-- StatusBars
myWorkspaceBar, myBottomStatusBar, myTopStatusBar :: String
myWorkspaceBar    = "dzen2 -x '0' -y '0' -h '17' -w '1920' -ta 'l' -fg '" ++ colorWhiteAlt ++ "' -bg '" ++ colorBlack ++ "' -fn '" ++ dzenFont ++ "' -p -e ''"
myBottomStatusBar = "conky"
myTopStatusBar    = "conky -c /home/samir/.conkydzentop | dzen2 -x '1200' -y '0' -h '17' -w '1200' -fg '" ++colorWhiteAlt ++ "' -bg '" ++ colorBlack ++ "' -fn '" ++ dzenFont ++ "' -ta r -e ''"
 
-- myWorkspaceBar config
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    { ppOutput          = hPutStrLn h
    , ppSort            = fmap (namedScratchpadFilterOutWorkspace.) (ppSort defaultPP) -- hide "NSP" from workspace list
    , ppOrder           = orderText
    , ppExtras          = []
    , ppSep             = "^fg(" ++ colorGray ++ ")|"
    , ppWsSep           = ""
    , ppCurrent         = dzenColor colorYellow     colorBlack . pad
    , ppUrgent          = dzenColor colorGreen    colorBlack . pad . wrapClickWorkSpace . (\a -> (a,a))
    , ppVisible         = dzenColor colorGray     colorBlack . pad . wrapClickWorkSpace . (\a -> (a,a))
    , ppHidden          = dzenColor colorWhiteAlt colorBlack . pad . wrapClickWorkSpace . (\a -> (a,a))
    , ppHiddenNoWindows = dzenColor colorGray     colorBlack . pad . wrapClickWorkSpace . (\a -> (a,a))
    , ppLayout          = dzenColor colorGreen     colorBlack . pad . wrapClickLayout . layoutText
    , ppTitle           = dzenColor colorWhiteAlt colorBlack . pad . wrapClickTitle . titleText 
. dzenEscape
    }
    where
        --display config
        orderText (ws:l:t:_) = [ws,l,t]
        titleText [] = "Desktop " ++ myArrow
        titleText x = (shorten 82 x) ++ " " ++ myArrow
        layoutText "Minimize T"  = "ReTall"
        layoutText "Minimize O"  = "OneBig"
        layoutText "Minimize TS" = "Tabbed"
        layoutText "Minimize TM" = "Master"
        layoutText "Minimize M"  = "Mosaic"
        layoutText "Minimize MT" = "Mirror"
        layoutText "Minimize G"  = "Mosaic"
        layoutText "Minimize C"  = "Mirror"
        layoutText "Minimize ReflectX T"  = "^fg(" ++ colorGreen ++ ")ReTall X^fg()"
        layoutText "Minimize ReflectX O"  = "^fg(" ++ colorGreen ++ ")OneBig X^fg()"
        layoutText "Minimize ReflectX TS" = "^fg(" ++ colorGreen ++ ")Tabbed X^fg()"
        layoutText "Minimize ReflectX TM" = "^fg(" ++ colorGreen ++ ")Master X^fg()"
        layoutText "Minimize ReflectX M"  = "^fg(" ++ colorGreen ++ ")Mosaic X^fg()"
        layoutText "Minimize ReflectX MT" = "^fg(" ++ colorGreen ++ ")Mirror X^fg()"
        layoutText "Minimize ReflectX G"  = "^fg(" ++ colorGreen ++ ")Mosaic X^fg()"
        layoutText "Minimize ReflectX C"  = "^fg(" ++ colorGreen ++ ")Mirror X^fg()"
        layoutText "Minimize ReflectY T"  = "^fg(" ++ colorGreen ++ ")ReTall Y^fg()"
        layoutText "Minimize ReflectY O"  = "^fg(" ++ colorGreen ++ ")OneBig Y^fg()"
        layoutText "Minimize ReflectY TS" = "^fg(" ++ colorGreen ++ ")Tabbed Y^fg()"
        layoutText "Minimize ReflectY TM" = "^fg(" ++ colorGreen ++ ")Master Y^fg()"
        layoutText "Minimize ReflectY M"  = "^fg(" ++ colorGreen ++ ")Mosaic Y^fg()"
        layoutText "Minimize ReflectY MT" = "^fg(" ++ colorGreen ++ ")Mirror Y^fg()"
        layoutText "Minimize ReflectY G"  = "^fg(" ++ colorGreen ++ ")Mosaic Y^fg()"
        layoutText "Minimize ReflectY C"  = "^fg(" ++ colorGreen ++ ")Mirror Y^fg()"
        layoutText "Minimize ReflectX ReflectY T"  = "^fg(" ++ colorGreen ++ ")ReTall XY^fg()"
        layoutText "Minimize ReflectX ReflectY O"  = "^fg(" ++ colorGreen ++ ")OneBig XY^fg()"
        layoutText "Minimize ReflectX ReflectY TS" = "^fg(" ++ colorGreen ++ ")Tabbed XY^fg()"
        layoutText "Minimize ReflectX ReflectY TM" = "^fg(" ++ colorGreen ++ ")Master XY^fg()"
        layoutText "Minimize ReflectX ReflectY M"  = "^fg(" ++ colorGreen ++ ")Mosaic XY^fg()"
        layoutText "Minimize ReflectX ReflectY MT" = "^fg(" ++ colorGreen ++ ")Mirror XY^fg()"
        layoutText "Minimize ReflectX ReflectY G"  = "^fg(" ++ colorGreen ++ ")Mosaic XY^fg()"
        layoutText "Minimize ReflectX ReflectY C"  = "^fg(" ++ colorGreen ++ ")Mirror XY^fg()"
        layoutText x = "^fg(" ++ colorGreen ++ ")" ++ x ++ "^fg()"
        --clickable config
        wrapClickLayout content = "^ca(1,xdotool key super+space)" ++ content ++ "^ca()"                                                           --clickable layout
        wrapClickTitle content = "^ca(1,xdotool key super+j)" ++ content ++ "^ca()"                                                                --clickable title
        wrapClickWorkSpace (idx,str) = "^ca(1," ++ xdo "w;" ++ xdo index ++ ")" ++ "^ca(3," ++ xdo "e;" ++ xdo index ++ ")" ++ str ++ "^ca()^ca()" --clickable workspaces
            where
                wsIdxToString Nothing = "1"
                wsIdxToString (Just n) = show (n+1)
                index = wsIdxToString (elemIndex idx myWorkspaces)
                xdo key = "xdotool key super+" ++ key
 
 
--------------------------------------------------------------------------------------------
-- BINDINGS CONFIG                                                                        --
--------------------------------------------------------------------------------------------
 
-- Key bindings
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)                       --Launch a terminal
    , ((mod1Mask, xK_F2), shellPrompt myXPConfig)                                              --Launch Xmonad shell prompt
    , ((modMask, xK_F2), xmonadPrompt myXPConfig)                                              --Launch Xmonad prompt
    , ((modMask, xK_g), goToSelected $ myGSConfig myColorizer)                                 --Launch GridSelect
    , ((modMask, xK_masculine), scratchPad)                                                    --Scratchpad
    , ((mod1Mask .|. shiftMask, xK_w), spawn "google-chrome")                                  --Launch chromium
    , ((mod1Mask .|. shiftMask, xK_e), spawn "subl")                                           --Launch chromium
    , ((mod1Mask .|. shiftMask, xK_f), spawn "thunar")                                         --Launch thunar
    , ((mod1Mask .|. shiftMask, xK_t), spawn "urxvt")                                          --Launch urxvt terminal
    , ((modMask, xK_c), kill)                                                                  --Close focused window
    , ((mod1Mask, xK_p), spawn "dmenu_run")						       --Launch dmenu
    , ((mod1Mask, xK_r), spawn "shutdown -r now")					       --Reboot
    , ((mod1Mask, xK_s), spawn "shutdown -h now")					       --Shutdown
    , ((modMask, xK_space), sendMessage NextLayout)                                            --Rotate through the available layout algorithms
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)                 --Reset the layouts on the current workspace to default
    , ((modMask, xK_n), refresh)                                                               --Resize viewed windows to the correct size
    , ((modMask, xK_Tab), windows W.focusDown)                                                 --Move focus to the next window
    , ((modMask, xK_j), windows W.focusDown)
    , ((mod1Mask, xK_Tab), windows W.focusDown)
    , ((modMask, xK_k), windows W.focusUp)                                                     --Move focus to the previous window
    , ((modMask, xK_a), windows W.focusMaster)                                                 --Move focus to the master window
    , ((modMask .|. shiftMask, xK_a), windows W.swapMaster)                                    --Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_j), windows W.swapDown  )                                    --Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k), windows W.swapUp    )                                    --Swap the focused window with the previous window
    , ((modMask, xK_h), sendMessage Shrink)                                                    --Shrink the master area
    , ((modMask .|. shiftMask, xK_Left), sendMessage Shrink)
    , ((modMask, xK_l), sendMessage Expand)                                                    --Expand the master area
    , ((modMask .|. shiftMask, xK_Right), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h), sendMessage MirrorShrink)                                --MirrorShrink the master area
    , ((modMask .|. shiftMask, xK_Down), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l), sendMessage MirrorExpand)                                --MirrorExpand the master area
    , ((modMask .|. shiftMask, xK_Up), sendMessage MirrorExpand)
    , ((modMask .|. controlMask, xK_Left), withFocused (keysResizeWindow (-30,0) (0,0)))       --Shrink floated window horizontally by 50 pixels
    , ((modMask .|. controlMask, xK_Right), withFocused (keysResizeWindow (30,0) (0,0)))       --Expand floated window horizontally by 50 pixels
    , ((modMask .|. controlMask, xK_Up), withFocused (keysResizeWindow (0,-30) (0,0)))         --Shrink floated window verticaly by 50 pixels
    , ((modMask .|. controlMask, xK_Down), withFocused (keysResizeWindow (0,30) (0,0)))        --Expand floated window verticaly by 50 pixels
    , ((modMask, xK_t), withFocused $ windows . W.sink)                                        --Push window back into tiling
    , ((modMask .|. shiftMask, xK_t), rectFloatFocused)                                        --Push window into float
    , ((modMask, xK_f), sendMessage $ XMonad.Layout.MultiToggle.Toggle TABBED)                 --Push layout into tabbed
    , ((modMask .|. shiftMask, xK_z), sendMessage $ Toggle MIRROR)                             --Push layout into mirror
    , ((modMask .|. shiftMask, xK_x), sendMessage $ XMonad.Layout.MultiToggle.Toggle REFLECTX) --Reflect layout by X
    , ((modMask .|. shiftMask, xK_y), sendMessage $ XMonad.Layout.MultiToggle.Toggle REFLECTY) --Reflect layout by Y
    , ((modMask, xK_m), withFocused minimizeWindow)                                            --Minimize window
    , ((modMask .|. shiftMask, xK_m), sendMessage RestoreNextMinimizedWin)                     --Restore window
    , ((modMask .|. shiftMask, xK_f), fullFloatFocused)                                        --Push window into full screen
    , ((modMask, xK_comma), sendMessage (IncMasterN 1))                                        --Increment the number of windows in the master area
    , ((modMask, xK_period), sendMessage (IncMasterN (-1)))                                    --Deincrement the number of windows in the master area
    , ((modMask , xK_d), spawn "killall dzen2")                                                --Kill dzen2 and trayer
    , ((modMask , xK_s), spawn "xscreensaver-command -lock")                                   --Lock screen
    , ((modMask .|. shiftMask, xK_q), io (exitWith ExitSuccess))                               --Quit xmonad
    , ((modMask, xK_q), restart "xmonad" True)                                                 --Restart xmonad
    , ((modMask, xK_comma), toggleWS)                                                          --Toggle to the workspace displayed previously
    , ((mod1Mask, xK_masculine), toggleOrView (myWorkspaces !! 0))                             --if ws != 0 then move to workspace 0, else move to latest ws I was
    , ((mod1Mask .|. controlMask, xK_Left),  prevWS)                                           --Move to previous Workspace
    , ((modMask, xK_Left), prevWS)
    , ((modMask, xK_Right), nextWS)                                                            --Move to next Workspace
    , ((mod1Mask .|. controlMask, xK_Right), nextWS)
    , ((0, xF86XK_AudioMute), spawn "sh /home/nnoell/bin/voldzen.sh t -d")                     --Mute/unmute volume
    , ((0, xF86XK_AudioRaiseVolume), spawn "sh /home/nnoell/bin/voldzen.sh + -d")              --Raise volume
    , ((0, xF86XK_AudioLowerVolume), spawn "sh /home/nnoell/bin/voldzen.sh - -d")              --Lower volume
    , ((0, xF86XK_AudioNext), spawn "ncmpcpp next")                                            --next song
    , ((0, xF86XK_AudioPrev), spawn "ncmpcpp prev")                                            --prev song
    , ((0, xF86XK_AudioPlay), spawn "ncmpcpp toggle")                                          --toggle song
    , ((0, xF86XK_AudioStop), spawn "ncmpcpp stop")                                            --stop song
    , ((0, xF86XK_MonBrightnessUp), spawn "sh /home/nnoell/bin/bridzen.sh")                    --Raise brightness
    , ((0, xF86XK_MonBrightnessDown), spawn "sh /home/nnoell/bin/bridzen.sh")                  --Lower brightness
    , ((0, xF86XK_ScreenSaver), spawn "xscreensaver-command -lock")                            --Lock screen
    , ((0, xK_Print), spawn "scrot '%Y-%m-%d_$wx$h.png'")                                      --Take a screenshot
    ]
    ++
    [((m .|. modMask, k), windows $ f i)                                                       --Switch to n workspaces and send client to n workspaces
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))                --Switch to n screens and send client to n screens
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    where
        fullFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f
        rectFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery (doRectFloat $ RationalRect 0.05 0.05 0.9 0.9) f
 
-- Mouse bindings
myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)) -- set the window to floating mode and move by dragging
    , ((modMask, button2), (\w -> focus w >> windows W.shiftMaster))                      -- raise the window to the top of the stack
    , ((modMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))                   -- set the window to floating mode and resize by dragging
    , ((modMask, button4), (\_ -> prevWS))                                                -- switch to previous workspace
    , ((modMask, button5), (\_ -> nextWS))                                                -- switch to next workspace
    ]
