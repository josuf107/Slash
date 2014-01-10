module Main where

import Slash
import Slash.Handler

import Java.AST

import Control.Lens
import Graphics.Vty as G
import Language.Java.Pretty
import Language.Java.Syntax

data ClassPart = ClassName | Class | Done deriving Eq
data Builder = ClassBuilder ClassDecl ClassPart

newClassBuilder :: Builder
newClassBuilder = ClassBuilder emptyClass Class

isBuilt :: Builder -> Bool
isBuilt (ClassBuilder _ Done) = True
isBuilt (ClassBuilder _ _) = False

builderContent :: Builder -> String
builderContent (ClassBuilder cd _) = show . pretty $ cd

data MySlash = MySlash
    { insertMode :: Bool
    , building :: Maybe Builder
    }

main :: IO ()
main = slash mySlash myHandler

mySlash :: MySlash
mySlash = MySlash False Nothing

myHandler :: Handler (Slash MySlash)
myHandler e s =
    case building . userData $ s of
        Just b -> checkBuilder b e s
        Nothing ->
            if insertMode . userData $ s then handleInsert e s
            else handleNormal e s

handleInsert :: Handler (Slash MySlash)
handleInsert =
    withKey putKey
    <+> onEnter (putKey '\n')
    <+> onBack (delete 1)
    <+> onCtrl 'c' (changeUserData toggleInsert)

setBuilder :: Builder -> MySlash -> MySlash
setBuilder b u = u { building = return b }

handleNormal :: Handler (Slash MySlash)
handleNormal =
    onKey 'b' (deleteBy Slash.Word 1)
    <+> onKey 'i' (changeUserData toggleInsert)
    <+> onKey 'c' (changeUserData . setBuilder $ newClassBuilder)

toggleInsert :: MySlash -> MySlash
toggleInsert m = m { insertMode = not . insertMode $ m }

checkBuilder :: Builder -> Handler (Slash MySlash)
checkBuilder b e =
    if isBuilt b then
        (putString . builderContent $ b)
        . changeUserData (\u -> u { building = Nothing })
    else changeUserData . setBuilder . handleBuilder e $ b

handleBuilder :: Handler Builder
handleBuilder e cb@(ClassBuilder c p) =
    case p of
        Class -> case e of
            EvKey (KASCII 'c') [MCtrl] -> ClassBuilder c Done
            EvKey (KASCII 'P') _ -> ClassBuilder (c & modifyClass <>~ [Public]) p
            EvKey (KASCII 'n') _ -> ClassBuilder c ClassName
            _ -> cb
        ClassName -> case e of
            EvKey (KASCII 'c') [MCtrl] -> ClassBuilder c Class
            EvKey KBS _ -> ClassBuilder (c & identifyClass %~ removeLast) p
            EvKey (KASCII k) _ -> ClassBuilder (c & identifyClass <>~ [k]) p
            _ -> cb
        Done -> cb

removeLast :: [a] -> [a]
removeLast = reverse . drop 1 . reverse
