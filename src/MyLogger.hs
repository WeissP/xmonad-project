module MyLogger where

import           Data.List
import           Data.Maybe
import           Text.Regex
import           XMonad
import qualified XMonad.StackSet               as W
import           XMonad.Util.Loggers
import           XMonad.Util.NamedWindows

-- receive one sperate and three funs to format count, focused window and unfocused window
myLogTitles
    :: String
    -> String
    -> (Int -> String)
    -> (String -> String)
    -> ([String] -> String)
    -> Logger
myLogTitles sep1 sep2 formatCount formatFoc formatUnfoc = do
    winset <- gets windowset
    let focWin     = W.peek winset
        wins       = W.index winset
        winsNonFoc = filter (\w -> Just w /= focWin) wins
        count      = length wins
    winNamesNonFoc <- case winsNonFoc of
        [] -> pure ""
        xs -> (sep2 ++) . formatUnfoc <$> traverse (fmap show . getName) xs
    focWinName <- case focWin of
        Just justFoc -> (sep1 ++) . formatFoc . show <$> getName justFoc
        Nothing      -> pure ""
    pure . Just $ formatCount count ++ focWinName ++ winNamesNonFoc


logWindowCount :: X (Maybe String)
logWindowCount = withWindowSet ct  where
    ct ss =
        return
            $ Just
            $ show
            $ length
            $ W.integrate'
            $ W.stack
            $ W.workspace
            $ W.current ss


logMaster :: X Bool
logMaster = withWindowSet isMaster  where
    isMaster ss = return $ case W.stack . W.workspace . W.current $ ss of
        Just (W.Stack _ [] _) -> True
        _                     -> False

trimPrefixWithList :: [String] -> Maybe String -> Maybe String
trimPrefixWithList _  Nothing  = Nothing
trimPrefixWithList xs (Just s) = case mapMaybe (`stripPrefix` s) xs of
    []    -> Just s
    n : _ -> trimPrefixWithList xs (Just n)

trimLayoutModifiers :: Maybe String -> Maybe String
trimLayoutModifiers = trimPrefixWithList ["Spacing", " "]

