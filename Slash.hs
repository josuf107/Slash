{-# LANGUAGE TemplateHaskell #-}
module Slash where

import Control.Lens
import Data.Word
import Graphics.Vty
import System.Posix.IO (stdInput)
import System.Posix.Terminal

data SlashInternal = SlashInternal
    { slashContent :: String
    , point :: (Word, Word)
    , oattrs :: TerminalAttributes
    , vty :: Vty
    }

type Handler a = Event -> a -> a

class Slash a where
    getInternal :: a -> SlashInternal
    setInternal :: a -> SlashInternal -> a
    getHandler :: a -> Handler a

changeInternal :: Slash a => (SlashInternal -> SlashInternal) -> a -> a
changeInternal f a = setInternal a (f $ getInternal a)

mkInternal :: IO SlashInternal
mkInternal = do
    oattrs <- getTerminalAttributes stdInput
    v <- mkVty
    return $ SlashInternal "" (0, 0) oattrs v

slash :: Slash a => a -> IO ()
slash s = do
    input $ s
    setTerminalAttributes stdInput (oattrs . getInternal $ s) Immediately

input :: Slash a => a -> IO ()
input s = do
    (update . vty $ i) $ pic_for_image (string def_attr (slashContent i))
    ev <- next_event (vty i)
    case ev of
        EvKey KEsc _ -> putStrLn "Bye"
        k -> input (h k $ s)
    where
        i = getInternal s
        h = getHandler s

changeText :: Slash a => (String -> String) -> a -> a
changeText f = changeInternal g
    where
        g :: SlashInternal -> SlashInternal
        g s = s { slashContent = f . slashContent $ s }

putKey :: Slash a => Char -> a -> a
putKey c = changeText (++[c])

delete :: Slash a => Int -> a -> a
delete n = changeText (reverse . drop n . reverse)

deleteBy :: Slash a => stuff
