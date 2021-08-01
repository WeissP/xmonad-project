module MyNamedScratchpad where

import           XMonad
import           XMonad.Actions.DynamicWorkspaces
                                                ( addHiddenWorkspace )
import           XMonad.Actions.SpawnOn         ( spawnHere )
import           XMonad.Hooks.DynamicLog        ( PP
                                                , ppSort
                                                )
import           XMonad.Hooks.ManageHelpers     ( doRectFloat )
import           XMonad.Hooks.RefocusLast       ( withRecentsIn )
import           XMonad.Prelude                 ( filterM
                                                , find
                                                , unless
                                                , when
                                                )

import qualified Data.List.NonEmpty            as NE

import qualified XMonad.StackSet               as W

-- | Single named scratchpad configuration
data NamedScratchpad = NS
    { name  :: String      -- ^ Scratchpad name
    , cmd   :: String      -- ^ Command used to run application
    , query :: Query Bool  -- ^ Query to find already running application
    , hook  :: ManageHook  -- ^ Manage hook called for application window, use it to define the placement. See @nonFloating@, @defaultFloating@ and @customFloating@
    }

-- | Manage hook that makes the window non-floating
nonFloating :: ManageHook
nonFloating = idHook

-- | Manage hook that makes the window floating with the default placement
defaultFloating :: ManageHook
defaultFloating = doFloat

-- | Manage hook that makes the window floating with custom placement
customFloating :: W.RationalRect -> ManageHook
customFloating = doRectFloat

-- | Named scratchpads configuration
type NamedScratchpads = [NamedScratchpad]

-- | Finds named scratchpad configuration by name
findByName :: NamedScratchpads -> String -> Maybe NamedScratchpad
findByName c s = find ((s ==) . name) c

-- | Runs application which should appear in specified scratchpad
runApplication :: NamedScratchpad -> X ()
runApplication = spawn . cmd

-- | Runs application which should appear in a specified scratchpad on the workspace it was launched on
runApplicationHere :: NamedScratchpad -> X ()
runApplicationHere = spawnHere . cmd

-- | Action to pop up specified named scratchpad
namedScratchpadAction
    :: NamedScratchpads -- ^ Named scratchpads configuration
    -> String           -- ^ Scratchpad name
    -> X ()
namedScratchpadAction = customRunNamedScratchpadAction runApplication

-- | Action to pop up specified named scratchpad, initially starting it on the current workspace.
spawnHereNamedScratchpadAction
    :: NamedScratchpads           -- ^ Named scratchpads configuration
    -> String                     -- ^ Scratchpad name
    -> X ()
spawnHereNamedScratchpadAction =
    customRunNamedScratchpadAction runApplicationHere

-- | Action to pop up specified named scratchpad, given a custom way to initially start the application.
customRunNamedScratchpadAction
    :: (NamedScratchpad -> X ())  -- ^ Function initially running the application, given the configured @scratchpad@ cmd
    -> NamedScratchpads           -- ^ Named scratchpads configuration
    -> String                     -- ^ Scratchpad name
    -> X ()
customRunNamedScratchpadAction =
    someNamedScratchpadAction (\f ws -> f $ NE.head ws)

allNamedScratchpadAction :: NamedScratchpads -> String -> X ()
allNamedScratchpadAction = someNamedScratchpadAction mapM_ runApplication

-- | A @logHook@ to hide scratchpads when they lose focus.  This can be
-- useful for e.g. dropdown terminals.  Note that this also requires you
-- to use the 'XMonad.Hooks.RefocusLast.refocusLastLogHook'.
--
-- ==== __Example__
--
-- > import XMonad.Hooks.RefocusLast (refocusLastLogHook)
-- > import XMonad.Util.NamedScratchpad
-- >
-- > main = xmonad $ def
-- >   { logHook = refocusLastLogHook
-- >            >> nsHideOnFocusLoss myScratchpads
-- >               -- enable hiding for all of @myScratchpads@
-- >   }
nsHideOnFocusLoss :: NamedScratchpads -> X ()
nsHideOnFocusLoss scratches = withWindowSet $ \winSet -> do
    let cur = W.currentTag winSet
    withRecentsIn cur () $ \lastFocus _ ->
        when (lastFocus `elem` W.index winSet && cur /= scratchpadWorkspaceTag)
            $ whenX (isNS lastFocus)
            $ shiftToNSP (W.workspaces winSet) ($ lastFocus)
  where
    isNS :: Window -> X Bool
    isNS w = or <$> traverse ((`runQuery` w) . query) scratches

-- | execute some action on a named scratchpad
someNamedScratchpadAction
    :: ((Window -> X ()) -> NE.NonEmpty Window -> X ())
    -> (NamedScratchpad -> X ())
    -> NamedScratchpads
    -> String
    -> X ()
someNamedScratchpadAction f runApp scratchpadConfig scratchpadName =
    case findByName scratchpadConfig scratchpadName of
        Just conf -> withWindowSet $ \winSet -> do
            let focusedWspWindows = maybe
                    []
                    W.integrate
                    (W.stack . W.workspace . W.current $ winSet)
                allWindows = W.allWindows winSet
            matchingOnCurrent <- filterM (runQuery (query conf))
                                         focusedWspWindows
            matchingOnAll <- filterM (runQuery (query conf)) allWindows

            case NE.nonEmpty matchingOnCurrent of
                -- no matching window on the current workspace -> scratchpad not running or in background
                Nothing -> case NE.nonEmpty matchingOnAll of
                    Nothing -> runApp conf
                    Just wins ->
                        f (windows . W.shiftWin (W.currentTag winSet)) wins

                -- matching window running on current workspace -> window should be shifted to scratchpad workspace
                Just wins -> shiftToNSP (W.workspaces winSet) (`f` wins)
        Nothing -> return ()

-- | Tag of the scratchpad workspace
scratchpadWorkspaceTag :: String
scratchpadWorkspaceTag = "NSP"

-- | Manage hook to use with named scratchpads
namedScratchpadManageHook
    :: NamedScratchpads -- ^ Named scratchpads configuration
    -> ManageHook
namedScratchpadManageHook = composeAll . fmap (\c -> query c --> hook c)

-- | Shift some windows to the scratchpad workspace according to the
-- given function.  The workspace is created if necessary.
shiftToNSP :: [WindowSpace] -> ((Window -> X ()) -> X ()) -> X ()
shiftToNSP ws f = do
    unless (any ((scratchpadWorkspaceTag ==) . W.tag) ws)
        $ addHiddenWorkspace scratchpadWorkspaceTag
    f (windows . W.shiftWin scratchpadWorkspaceTag)

-- | Transforms a workspace list containing the NSP workspace into one that
-- doesn't contain it. Intended for use with logHooks.
namedScratchpadFilterOutWorkspace :: [WindowSpace] -> [WindowSpace]
namedScratchpadFilterOutWorkspace =
    filter (\(W.Workspace tag _ _) -> tag /= scratchpadWorkspaceTag)
{-# DEPRECATED namedScratchpadFilterOutWorkspace "Use XMonad.Util.WorkspaceCompare.filterOutWs [scratchpadWorkspaceTag] instead" #-}

-- | Transforms a pretty-printer into one not displaying the NSP workspace.
--
-- A simple use could be:
--
-- > logHook = dynamicLogWithPP . namedScratchpadFilterOutWorkspace $ def
--
-- Here is another example, when using "XMonad.Layout.IndependentScreens".
-- If you have handles @hLeft@ and @hRight@ for bars on the left and right screens, respectively, and @pp@ is a pretty-printer function that takes a handle, you could write
--
-- > logHook = let log screen handle = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP . marshallPP screen . pp $ handle
-- >           in log 0 hLeft >> log 1 hRight
namedScratchpadFilterOutWorkspacePP :: PP -> PP
namedScratchpadFilterOutWorkspacePP pp =
    pp { ppSort = fmap (. namedScratchpadFilterOutWorkspace) (ppSort pp) }
{-# DEPRECATED namedScratchpadFilterOutWorkspacePP "Use XMonad.Hooks.DynamicLog.filterOutWsPP [scratchpadWorkspaceTag] instead" #-}

-- vim:ts=4:shiftwidth=4:softtabstop=4:expandtab:foldlevel=20:
