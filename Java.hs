module Java where

import Slash

import Graphics.Vty
import Language.Java.Parser
import Language.Java.Pretty
import Language.Java.Syntax
import Text.Parsec.Error (ParseError)

data MySlash = MySlash
    { insertMode :: Bool
    , java :: Either ParseError CompilationUnit
    }

main = slash mySlash myHandler

mySlash :: MySlash
mySlash = MySlash False undefined

myHandler :: Handler (Slash MySlash)
myHandler e s =
    if (insertMode . userData $ s) then handleInsert e s
    else handleNormal e s

toggleInsert :: MySlash -> MySlash
toggleInsert (MySlash False j) = MySlash True j
toggleInsert (MySlash True j) = MySlash False j

handleInsert :: Handler (Slash MySlash)
handleInsert e s = case e of
    EvKey (KASCII c) [] -> putKey c s
    EvKey (KEnter) [] -> putKey '\n' s
    EvKey KBS _ -> delete 1 s
    EvKey (KASCII 'c') [MCtrl] -> changeUserData toggleInsert s
    _ -> s

handleNormal :: Handler (Slash MySlash)
handleNormal e s = case e of
    EvKey (KASCII 'b') _ -> deleteBy Slash.Word 1 s
    EvKey (KASCII 'i') _ -> changeUserData toggleInsert s
    _ -> s
