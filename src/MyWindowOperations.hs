{-# LANGUAGE LambdaCase #-}

module MyWindowOperations where
import qualified Data.Map                      as M
import           MyLogger
import           XMonad.Util.Loggers
import           XMonad
import qualified XMonad.StackSet               as W

isMaster :: W.StackSet i l a s sd -> Bool
isMaster ss = case W.stack . W.workspace . W.current $ ss of
    Just (W.Stack _ [] _) -> True
    _                     -> False

myFocusDown :: X ()
myFocusDown = do
    l <- logLayout
    case trimLayoutModifiers l of
        Just "TwoPane"     -> windows focusDownTwoPane
        Just "Mirror Tall" -> windows W.focusUp
        _                  -> windows W.focusDown
  where
    focusDownTwoPane :: W.StackSet i l a s sd -> W.StackSet i l a s sd
    focusDownTwoPane = W.modify' $ \stack -> case stack of
        -- W.Stack r2 (l : r1 : up) [] -> W.Stack r1 [l] r2: [up] 
        -- W.Stack r2 (l : r1 : up) (r3:down) -> W.Stack r3 [l] (r1 : down)
        W.Stack r1 (l : up) (r2 : down) -> W.Stack r2 [l] (r1 : up ++ down)
        W.Stack l [] (r1 : r2 : down) -> W.Stack r1 [l] (r2 : down)
        _ -> W.focusDown' stack

myFocusUp :: X ()
myFocusUp = do
    l <- logLayout
    case trimLayoutModifiers l of
        Just "TwoPane"     -> windows focusUpTwoPane
        Just "Mirror Tall" -> windows W.focusDown
        _                  -> windows W.focusUp
  where
    focusUpTwoPane :: W.StackSet i l a s sd -> W.StackSet i l a s sd
    focusUpTwoPane = W.modify' $ \stack -> case stack of
        -- W.Stack r2 (l : r1 : up) down -> W.Stack l [] (r2 : r1 : down)
        W.Stack r1 (l : up) (r2 : down) -> W.Stack l [] (r1 : r2 : down)
        W.Stack l [] (r1 : r2 : down) -> W.Stack r2 [l] (r1 : down)
        _ -> W.focusUp' stack

mySwapMaster :: X ()
mySwapMaster = do
    l <- logLayout
    case trimLayoutModifiers l of
        Just "TwoPane" -> windows swapMasterTwoPane
        _              -> windows $ W.modify' swapBetweenMasterAndSlave
  where
    swapBetweenMasterAndSlave :: W.Stack a -> W.Stack a
    swapBetweenMasterAndSlave stack = case stack of
        W.Stack f [] [] -> stack
        W.Stack f [] ds -> W.Stack (last ds) [] (f : init ds)
        W.Stack t ls rs -> W.Stack t [] (xs ++ x : rs)
            where (x : xs) = reverse ls
    swapMasterTwoPane :: W.StackSet i l a s sd -> W.StackSet i l a s sd
    swapMasterTwoPane = W.modify' $ \stack -> case stack of
        W.Stack r2 (l : r1 : up) down -> W.Stack r1 [r2] (l : down)
        W.Stack r1 (l : up) (r2 : down) -> W.Stack r2 [r1] (l : down)
        W.Stack l [] (r1 : r2 : down) -> W.Stack l [] (r2 : r1 : down)
        _ -> swapBetweenMasterAndSlave stack




-- | if the workspace is visible in some screen, then focus to this screen, else switch current screen to that workspace
switchOrFocus :: WorkspaceId -> X ()
switchOrFocus ws = switchOrFocusHelp ws 0
  where
    switchOrFocusHelp ws sc = screenWorkspace sc >>= \case
        Nothing -> windows $ W.greedyView ws
        Just x  -> if x == ws
            then windows $ W.view x
            else switchOrFocusHelp ws (sc + 1)

-- from https://www.reddit.com/r/xmonad/comments/hm2tg0/how_to_toggle_floating_state_on_a_window/
toggleFloat :: Window -> X ()
toggleFloat w = windows
    (\s -> if M.member w (W.floating s)
        then W.sink w s
        else W.float w (W.RationalRect 0 0 1 1) s
    )

shiftThenSwitchOrFocus i = do
    windows $ W.shift i
    switchOrFocus i

moveFloat :: X () -> X ()
moveFloat f = do
    m <- logMaster
    l <- logLayout
    f
    case (m, f) of
        (True, _) -> withFocused (`tileWindow` Rectangle 50 50 200 200)
        _         -> withFocused (`tileWindow` Rectangle 50 50 500 500)

