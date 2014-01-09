module Java.AST where

import Data.List
import Control.Lens
import Language.Java.Syntax

modifyClass :: Lens' ClassDecl [Modifier]
modifyClass = lens
    (\(ClassDecl ms _ _ _ _ _) -> ms)
    (\(ClassDecl _ i ts mrt rts b) ms -> ClassDecl (nub ms) i ts mrt rts b)

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
