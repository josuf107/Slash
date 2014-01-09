module Slash where

import Slash.Handler

import Data.Word
import Graphics.Vty
import System.Posix.IO (stdInput)
import System.Posix.Terminal

data Slash a = Slash
    { slashContent :: String
    , point :: (Word, Word)
    , handler :: Handler (Slash a)
    , vty :: Vty
    , userData :: a
    }

data TextUnit = Word

slash :: a -> Handler (Slash a) -> IO ()
slash u h = do
    oattrs <- getTerminalAttributes stdInput
    v <- mkVty
    input $ Slash "" (0, 0) h v u
    setTerminalAttributes stdInput oattrs Immediately

input :: Slash a -> IO ()
input s = do
    (update . vty $ s) $ pic_for_image (string def_attr (slashContent s))
    ev <- next_event (vty s)
    case ev of
        EvKey KEsc _ -> putStrLn "Bye"
        k -> input . handler s k $ s

changeUserData :: (a -> a) -> Slash a -> Slash a
changeUserData f s = s { userData = f . userData $ s }

visibleContent :: Slash a -> String
visibleContent = slashContent

changeText :: (String -> String) -> Slash a -> Slash a
changeText f s = s { slashContent = f . slashContent $ s }

putKey :: Char -> Slash a -> Slash a
putKey c = changeText (++[c])

putString :: String -> Slash a -> Slash a
putString s = changeText (++s)

delete :: Int -> Slash a -> Slash a
delete n = changeText (reverse . drop n . reverse)

deleteBy :: TextUnit -> Int -> Slash a -> Slash a
deleteBy Word n = changeText (unwords . reverse . drop n . reverse . words)
