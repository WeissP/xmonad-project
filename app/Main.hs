{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}


module Main where

import           Data.Maybe
import           MyLogger
import           MyNamedScratchpad
import           MyPromptPass
import           MyWindowOperations
import           MyXmobar
import           System.IO                      ( hPutStrLn )
import           Text.Regex
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.MouseResize
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.DynamicProperty
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.StatusBar
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.NoBorders
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.Spacing
import           XMonad.Layout.TwoPane
import           XMonad.Layout.WindowArranger
import           XMonad.Layout.WindowNavigation
import           XMonad.Prompt                  ( XPConfig(..)
                                                , XPPosition(..)
                                                , font
                                                , height
                                                , position
                                                )
import qualified XMonad.StackSet               as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Loggers
import           XMonad.Util.Paste
import           XMonad.Util.Run                ( runInTerm
                                                , runProcessWithInput
                                                , safeSpawn
                                                , spawnPipe
                                                )
import           XMonad.Util.Ungrab
myTerminal = "alacritty"

myBorderWidth :: Dimension
myBorderWidth = 3 -- Sets border width for windows

myNormColor :: String
myNormColor = "#282c34" -- Border color of normal windows

myFocusColor :: String
myFocusColor = "#46d9ff" -- Border color of focused windows

myModMask :: KeyMask
myModMask = mod4Mask

myWorkspaces :: [WorkspaceId]
myWorkspaces =
    zipWith (\i n -> show i ++ n)
            [1 .. 8 :: Int]
            (map (":" ++) ["主", "副", "娱", "邮", "音"] ++ repeat "")

        ++ [scratchpadWorkspaceTag]

mylogLayout :: Logger
mylogLayout = withWindowSet $ return . Just . ld
    where ld = description . W.layout . W.workspace . W.current

-- Gaps around and between windows
-- Changes only seem to apply if I log out then in again
-- Dimensions are given as (Border top bottom right left)
mySpacing :: l a -> ModifiedLayout Spacing l a
mySpacing = spacingRaw True             -- Only for >1 window
                       -- The bottom edge seems to look narrower than it is
                       (Border 0 0 0 0) -- Size of screen edge gaps
                       True             -- Enable screen edge gaps
                       (Border 5 5 5 5) -- Size of window gaps
                       True             -- Enable window gaps

myXPConfig :: XPConfig
myXPConfig = def { position     = Top
                 , font         = "xft:DejaVu Sans:size=9"
                 , height       = 40
                 , autoComplete = Just 800
                 }

myLayout =
    avoidStruts
        $   mySpacing
        $   smartBorders
        $   mouseResize
        $   windowArrange myTall
        ||| Mirror myTall
  where
    -- addTopBar = noFrillsDeco shrinkText topBarTheme
    twoPane = TwoPane delta ratio
    myTall  = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 1 / 2
    delta   = 3 / 100

myKeys :: [([Char], X ())]
myKeys =
    [ ( "<XF86Launch5>"
      , spawn
          "rofi -run-list-command \". /home/weiss/weiss/zsh_aliases.sh\" -run-command \"/bin/zsh -i -c '{cmd}'\" -show run"
      )
        , ("<XF86Launch8>", nextScreen)
        , ("<F6>", spawnHereNamedScratchpadAction myScratchPads "tmux")
        , ("<F11>"        , withFocused toggleFloat)
        , ( "M-3"
          , spawn
              "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"
          )
        , ("<XF86Launch6>", mySwapMaster)
        , ("M-<Escape>"   , kill)
        , ("M-1"          , myFocusUp)
        , ("M-2"          , myFocusDown)
        -- , ("C-<Tab>"      , unGrab *> spawn "xdotool key Control_L+Tab")
        -- , ("C-<Tab>"      , myFocusDown)
        -- , ("M-4"          , moveFloat $ namedScratchpadAction myScratchPads "tmux")
        ]
        ++ [ ("M-4 " ++ key, fun)
           | (key, fun) <-
               [ ("v", spawnHereNamedScratchpadAction myScratchPads "pavu")
               , ("t", windows (`skipFloating` W.focusDown))
               ]
           ]
        ++ [ (keyPrefix ++ " " ++ k, fun i)
           | (k, i) <- zip ["m", ",", ".", "j", "k", "l", "u", "i", "o"]
                           myWorkspaces
           , (keyPrefix, fun) <-
               [ ("<XF86Launch7>"         , windows . W.greedyView)
               , ("<XF86Launch7> <Space>" , shiftThenSwitchOrFocus)
               , ("<XF86Launch7> <Escape>", windows . W.shift)
               ]
           ]
        ++ [ ("<XF86Launch7> " ++ key, fun)
           | (key, fun) <-
               [ ("n", withFocused $ windows . W.sink)
               , ("t", sendMessage NextLayout)
               , ( "r"
                 , spawn
                     "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"
                 )
               , ("v"      , spawn "sh /home/weiss/.screenlayout/vertical.sh")
               , ("h"      , spawn "sh /home/weiss/.screenlayout/horizontal.sh")
               , ("s"      , spawn "flameshot gui")
               , ("p"      , mkPassPrompt "select pass" sendToClj myXPConfig)
               -- , ("h"      , spawn "rofi-pass")
               , ("<Left>" , sendMessage $ Move L)
               , ("<Right>", sendMessage $ Move R)
               , ("<Up>"   , sendMessage $ Move U)
               , ("<Down>" , sendMessage $ Move D)
               ]
           ]

myScratchPads :: [NamedScratchpad]
myScratchPads =
    [ NS "tmux"
         (myTerminal ++ " -e /home/weiss/weiss/tmux-init.sh")
         (title =? "tmux-Scratchpad")
         moveFloat
    , NS "pavu" "pavucontrol" (className =? "Pavucontrol") moveFloat
    ]
  where
    moveFloat :: Window -> X ()
    moveFloat a = do
        m <- logMaster
        l <- logLayout
        case (m, trimLayoutModifiers l) of
            (True, Just "Mirror Tall") -> windows $ W.float
                a
                (W.RationalRect (1 / 50) (26 / 50) (45 / 50) (20 / 50))
            (False, Just "Mirror Tall") -> windows $ W.float
                a
                (W.RationalRect (1 / 50) (5 / 50) (45 / 50) (20 / 50))
            (True, _) -> windows $ W.float
                a
                (W.RationalRect (26 / 50) (6 / 50) (23 / 50) (20 / 50))
            (False, _) -> windows $ W.float
                a
                (W.RationalRect (1 / 50) (6 / 50) (23 / 50) (20 / 50))


myManageHook :: ManageHook
myManageHook = composeAll
    (concat
        [ [isDialog --> doFloat]
        , [className =? "Chromium" --> doShift (getWorkspace 2)]
        , [className =? "Google-chrome" --> doShift (getWorkspace 3)]
        , [className =? "Thunderbird" --> doShift (getWorkspace 4)]
        , [className =? "Spotify" --> doShift (getWorkspace 5)]
        , [ className =? x --> doIgnore | x <- myIgnoreClass ]
        , [ className =? x --> doHideIgnore | x <- myHideIgnoreClass ]
        , [ className =? x --> doCenterFloat | x <- myCenterFloatClass ]
        , [ title =? x --> doCenterFloat | x <- myCenterFloatTitle ]
        , [ title *=? x --> doCenterFloat | x <- myCenterFloatTitleReg ]
        , [ className =? x --> doFullFloat | x <- myFullFloatClass ]
        ]
    )
  where
    (*=?) :: Functor f => f String -> String -> f Bool
    q *=? x =
        let matchReg = \a b -> isJust $ matchRegex (mkRegex a) b
        in  fmap (matchReg x) q
    myIgnoreClass         = ["trayer"]
    myHideIgnoreClass     = ["Blueman-applet"]
    myCenterFloatClass    = ["Blueman-manager", "zoom", "Pavucontrol"]
    myCenterFloatTitle    = ["tmux-Scratchpad"]
    myCenterFloatTitleReg = []
    myFullFloatClass      = ["MPlayer"]
    netName               = stringProperty "_NET_WM_NAME"


myConfig =
    def { modMask            = myModMask
        , terminal           = myTerminal
        -- , startupHook        = myStartupHook
        , manageHook         = myManageHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , layoutHook         = myLayout
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        -- , logHook            = myLogHook
        , handleEventHook    = handleEventHook def <+> fullscreenEventHook
        }
        -- `removeKeysP` ["M-4"]
        `additionalKeysP` myKeys

main :: IO ()
main = do
    xmonad
        $ ewmhFullscreen
        $ ewmh
        $ withEasySB
              (statusBarProp
                  "xmobar -x 0 /home/weiss/.config/xmobar/xmobarrc0.hs"
                  (pure myXmobarPP)
              )
              defToggleStrutsKey
        $ docks myConfig

getWorkspace :: Int -> String
getWorkspace i = myWorkspaces !! (i - 1)

myFocusUp, myFocusDown, mySwapMaster :: X ()
myFocusUp = myFocusUpWithNSP myScratchPads
myFocusDown = myFocusDownWithNSP myScratchPads
mySwapMaster = mySwapMasterWithNsp myScratchPads
-- ttt :: NS
