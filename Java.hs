module Java where

import Slash
import Slash.Handler

import Graphics.Vty as G
import Language.Java.Pretty
import Language.Java.Syntax as J

data ClassPart = ClassName | Class | Done deriving Eq
data Builder = ClassBuilder ClassDecl ClassPart

newClassBuilder :: Builder
newClassBuilder = ClassBuilder def Class

isBuilt :: Builder -> Bool
isBuilt (ClassBuilder _ Done) = True
isBuilt (ClassBuilder _ _) = False

builderContent :: Builder -> String
builderContent (ClassBuilder cd _) = show . pretty $ cd

data MySlash = MySlash
    { insertMode :: Bool
    , building :: Maybe Builder
    }

data Modifiable a = Modifiable { modify :: J.Modifier -> a -> a }

modifiableClass :: Modifiable ClassDecl
modifiableClass = Modifiable modify'
    where
        modify' m (ClassDecl ms i ts mrt rts b) =
                ClassDecl (m:ms) i ts mrt rts b
        modify' m (EnumDecl ms i rts b) =
                EnumDecl (m:ms) i rts b

data Identifiable a = Identifiable { identify :: (String -> String) -> a -> a }

identifiableClass :: Identifiable ClassDecl
identifiableClass = Identifiable identify'
    where
        identify' f (ClassDecl ms (Ident i) ts mrt rts b) =
                ClassDecl ms (Ident . f $ i) ts mrt rts b
        identify' f (EnumDecl ms (Ident i) rts b) =
                EnumDecl ms (Ident . f $ i) rts b

-- Avoiding orphan instances
class Default a where
    def :: a

instance Default [a] where
    def = []

instance Default ClassDecl where
    def = ClassDecl def (Ident def) def Nothing def def

instance Default ClassBody where
    def = ClassBody def

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
            EvKey (KASCII 'P') _ -> ClassBuilder ((modify modifiableClass) Public c) p
            EvKey (KASCII 'n') _ -> ClassBuilder c ClassName
            _ -> cb
        ClassName -> case e of
            EvKey (KASCII 'c') [MCtrl] -> ClassBuilder c Class
            EvKey KBS _ -> ClassBuilder ((identify identifiableClass) removeLast c) p
            EvKey (KASCII k) _ -> ClassBuilder ((identify identifiableClass) (++ [k]) c) p
            _ -> cb
        Done -> cb

removeLast :: [a] -> [a]
removeLast = reverse . drop 1 . reverse
