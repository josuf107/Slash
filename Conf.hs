module Conf where

import Slash

import Graphics.Vty

main = do
    s <- mySlash
    slash s

mySlash :: IO MySlash
mySlash = do
    i <- mkInternal
    return $ MySlash i myHandler False

myHandler :: Handler MySlash
myHandler e s =
    if insertMode s then handleInsert e s
    else handleNormal e s

handleInsert :: Handler MySlash
handleInsert e = case e of
    EvKey (KASCII c) _ -> putKey c
    EvKey KBS _ -> delete 1
    _ -> id

handleNormal :: Handler MySlash
handleNormal e s = case e of
    EvKey (KASCII 'b') _ -> deleteBy Word 1 s
    EvKey (KASCII 'i') _ -> s { insertMode = True }

data MySlash = MySlash
    { internal :: SlashInternal
    , handler :: Handler MySlash
    , insertMode :: Bool
    }

instance Slash MySlash where
    getInternal = internal
    setInternal m s = m { internal = s }
    getHandler = handler
