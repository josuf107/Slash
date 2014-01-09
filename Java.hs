module Java where

import Slash
import Slash.Handler

import Control.Lens
import qualified Data.List as L
import Graphics.Vty as G
import Language.Java.Pretty
import Language.Java.Syntax as J

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

modifyClass :: Lens' ClassDecl [J.Modifier]
modifyClass = lens
    (\(ClassDecl ms _ _ _ _ _) -> ms)
    (\(ClassDecl _ i ts mrt rts b) ms -> ClassDecl (L.nub ms) i ts mrt rts b)

identifyClass :: Lens' ClassDecl String
identifyClass = lens
    (\(ClassDecl _ (Ident i) _ _ _ _) -> i)
    (\(ClassDecl ms _ ts mrt rts b) i -> ClassDecl ms (Ident i) ts mrt rts b)

emptyClass :: ClassDecl
emptyClass = ClassDecl [] (Ident "") [] Nothing [] emptyClassBody

emptyClassBody :: ClassBody
emptyClassBody = ClassBody []

emptyEnum :: ClassDecl
emptyEnum = EnumDecl [] (Ident "") [] emptyEnumBody

emptyEnumBody :: EnumBody
emptyEnumBody = EnumBody [] []

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
