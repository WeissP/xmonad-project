{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}


module Main where

import           Data.List
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Semigroup
import           MyLogger
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
import           XMonad.Layout.Accordion
import           XMonad.Layout.BorderResize
import           XMonad.Layout.DragPane
import           XMonad.Layout.LayoutBuilder
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.NoBorders
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import           XMonad.Layout.TwoPane
import           XMonad.Layout.WindowArranger
import           XMonad.Layout.WindowNavigation
import           XMonad.Prompt                  ( XPConfig(..)
                                                , XPPosition(..)
                                                , font
                                                , height
                                                , position
                                                )
-- import           XMonad.Prompt.Pass
import qualified XMonad.StackSet               as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Loggers
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Paste
import           XMonad.Util.Run                ( runInTerm
                                                , runProcessWithInput
                                                , safeSpawn
                                                , spawnPipe
                                                )
import           XMonad.Util.Themes
import           XMonad.Util.Ungrab

myTerminal = "alacritty"

myBorderWidth :: Dimension
myBorderWidth = 2 -- Sets border width for windows

myNormColor :: String
myNormColor = "#282c34" -- Border color of normal windows

myFocusColor :: String
myFocusColor = "#46d9ff" -- Border color of focused windows

myModMask :: KeyMask
myModMask = mod4Mask

myWorkspaces :: [WorkspaceId]
myWorkspaces =
    zipWith (\i n -> show i ++ n) [1 .. 9 :: Int]
        $  map (":" ++) ["主", "副", "娱", "邮", "NSP"]
        ++ repeat ""

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
                 , autoComplete = Just 0
                 }

myLayout =
    avoidStruts
        $   mySpacing
        $   smartBorders
        $   mouseResize
        $   windowArrange
        twoPane ||| myTall ||| Mirror myTall
  where
    -- addTopBar = noFrillsDeco shrinkText topBarTheme
    twoPane = TwoPane delta ratio
    myTall  = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 1 / 2
    delta   = 3 / 100

myKeys :: [([Char], X ())]
myKeys =
    [ ( "<XF86Launch8>"
      , spawn
          "rofi -run-list-command \". /home/weiss/weiss/zsh_aliases.sh\" -run-command \"/bin/zsh -i -c '{cmd}'\" -show run"
      )
        , ("<XF86Launch5>", nextScreen)
        , ("<F6>", namedScratchpadAction myScratchPads "tmux")
        , ("<F11>"        , withFocused toggleFloat)
        , ( "M-3"
          , spawn
              "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"
          )
        , ("<XF86Launch6>", mySwapMaster)
        , ("M-<Escape>"   , kill)
        , ("M-1"          , myFocusUp)
        , ("M-2"          , myFocusDown)
        -- , ("C-v"          , unGrab *> spawn "xdotool ")
        -- , ("M-4"          , moveFloat $ namedScratchpadAction myScratchPads "tmux")
        , ( "M-4"
          , spawn
              "/home/weiss/.local/bin/xmobar -x 1 /home/weiss/.xmonad/xmobar/xmobarrc0.hs"
          )
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
               [ ("n"      , withFocused $ windows . W.sink)
               , ("t"      , sendMessage NextLayout)
               , ("p"      , spawn "rofi-pass")
               , ("h"      , spawn "rofi-pass")
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
         (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
    ]

myManageHook :: ManageHook
myManageHook = namedScratchpadManageHook myScratchPads <+> composeAll
    (concat
        [ [isDialog --> doFloat]
        , [className =? "Thunderbird" --> doShift (getWorkspace 4)]
        , [className =? "Google-chrome" --> doShift (getWorkspace 3)]
        , [className =? "Spotify" --> doShift (getWorkspace 2)]
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
    myCenterFloatClass    = ["Blueman-manager", "zoom"]
    myCenterFloatTitle    = ["tmux-Scratchpad"]
    myCenterFloatTitleReg = []
    myFullFloatClass      = ["MPlayer"]
    netName               = stringProperty "_NET_WM_NAME"




myHandleEventHook :: Event -> X All
myHandleEventHook = dynamicPropertyChange
    "WM_NAME"
    (title =? "tmux-Scratchpad" --> floating)
  where
    floating = do
        -- ms <- withFocused isMaster
        customFloating $ W.RationalRect (26 / 50) (1 / 8) (9 / 20) (1 / 2)

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
        , handleEventHook    = myHandleEventHook
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
                  "xmobar -x 1 /home/weiss/.config/xmobar/xmobarrc0.hs"
                  (pure myXmobarPP)
                  -- (pure def)
              )
              defToggleStrutsKey
        -- $ withSB (xmobar1 <> xmobar2 <> xmobar3)
        $ docks
        $ myConfig

getWorkspace :: Int -> String
getWorkspace i = myWorkspaces !! (i - 1)

-- ttt :: NS
