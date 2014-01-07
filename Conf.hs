module Conf where

import Slash

import Graphics.Vty

data MySlash = MySlash
    { insertMode :: Bool }

main = slash mySlash myHandler

mySlash :: MySlash
mySlash = MySlash False

myHandler :: Handler (Slash MySlash)
myHandler e s =
    if (insertMode . userData $ s) then handleInsert e s
    else handleNormal e s

toggleInsert :: MySlash -> MySlash
toggleInsert (MySlash False) = MySlash True
toggleInsert (MySlash True) = MySlash False

handleInsert :: Handler (Slash MySlash)
handleInsert e s = case e of
    EvKey (KASCII c) [] -> putKey c s
    EvKey (KEnter) [] -> putKey '\n' s
    EvKey KBS _ -> delete 1 s
    EvKey (KASCII 'c') [MCtrl] -> changeUserData toggleInsert s
    _ -> s

handleNormal :: Handler (Slash MySlash)
handleNormal e s = case e of
    EvKey (KASCII 'b') _ -> deleteBy Word 1 s
    EvKey (KASCII 'i') _ -> changeUserData toggleInsert s
    _ -> s
