module Java where

import Slash
import Slash.Handler

import Data.Default
import Graphics.Vty as G
import Language.Java.Parser
import Language.Java.Pretty
import Language.Java.Syntax
import Text.Parsec.Error (ParseError)

data ClassPart = ClassName | Class | Done
data Builder = ClassBuilder ClassDecl ClassPart | None
data MySlash = MySlash
    { insertMode :: Bool
    , building :: Builder
    }

instance Default ClassDecl where
    def = ClassDecl def (Ident def) def Nothing def def

instance Default ClassBody where
    def = ClassBody def

main = slash mySlash myHandler

mySlash :: MySlash
mySlash = MySlash False None

myHandler :: Handler (Slash MySlash)
myHandler e s =
    case building . userData $ s of
        (ClassBuilder cd Done) ->
            (putString . show . pretty $ cd) . (changeUserData (\u -> u { building = None })) $ s
        cb@(ClassBuilder _ _) -> changeUserData (\u -> u { building = handleBuilder e cb }) $s
        None ->
            if (insertMode . userData $ s) then handleInsert e s
            else handleNormal e s

handleInsert :: Handler (Slash MySlash)
handleInsert =
    withKey putKey
    <+> onEnter (putKey '\n')
    <+> onBack (delete 1)
    <+> onCtrl 'c' (changeUserData toggleInsert)

handleNormal :: Handler (Slash MySlash)
handleNormal =
    onKey 'b' (deleteBy Slash.Word 1)
    <+> onKey 'i' (changeUserData toggleInsert)
    <+> onKey 'c' (changeUserData (\u -> u { building = ClassBuilder def Class}))

toggleInsert :: MySlash -> MySlash
toggleInsert m = m { insertMode = not . insertMode $ m }

handleBuilder :: Handler Builder
handleBuilder e cb@(ClassBuilder c@(ClassDecl ms i@(Ident n) ts mrt rts b) p) =
    case p of
        Class -> case e of
            EvKey (KASCII 'c') [MCtrl] -> ClassBuilder c Done
            EvKey (KASCII 'P') _ -> ClassBuilder (ClassDecl (Public:ms) i ts mrt rts b) p
            EvKey (KASCII 'n') _ -> ClassBuilder c ClassName
            _ -> cb
        ClassName -> case e of
            EvKey (KASCII 'c') [MCtrl] -> ClassBuilder c Class
            EvKey KBS _ -> ClassBuilder (ClassDecl ms (Ident . reverse . drop 1 . reverse $ n) ts mrt rts b) p
            EvKey (KASCII c) _ -> ClassBuilder (ClassDecl ms (Ident . (++ [c]) $ n) ts mrt rts b) p
            _ -> cb
        Done -> cb
