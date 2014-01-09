module Test where

import Slash
import Slash.Handler

import qualified Data.Map as M
import Test.HUnit
import Test.QuickCheck

runTests :: IO Counts
runTests = do
    mapM quickCheck enabledChecks
    runTestTT enabledTests

enabledTests :: Test
enabledTests = TestList
    [ testPutChar
    , testPutChars
    , testPutString
    , testPutStrings
    , testPutLines
    ]

enabledChecks :: [Property]
enabledChecks =
    [ checkPutString
    ]

labeled :: String -> Assertion -> Test
labeled l a = TestLabel l $ TestCase a

testPutChar :: Test
testPutChar = labeled "put char" $
    "c" @=? visibleContent (putKey 'c' fakeSlash)

testPutChars :: Test
testPutChars = labeled "put chars" $
    "co" @=? visibleContent (putKey 'o' . putKey 'c' $ fakeSlash)

testPutString :: Test
testPutString = labeled "put string" $
    "string" @=? visibleContent (putString "string" fakeSlash)

testPutStrings :: Test
testPutStrings = labeled "put strings" $
    "string" @=? visibleContent (putString "ing" . putString "str" $ fakeSlash)

testPutLines :: Test
testPutLines = labeled "put lines" $
    "line1\nline2" @=? visibleContent (putString "line2" . putString "line1\n" $ fakeSlash)

checkPutString :: Property
checkPutString = label "put string iso" (\s -> s == visibleContent (putString s $ fakeSlash))

fakeSlash :: Slash a
fakeSlash = Slash "" (0,0) undefined undefined undefined
