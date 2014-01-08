module Slash.Handler where

import Graphics.Vty

type Handler a = Event -> a -> a

(<+>) :: Handler a -> Handler a -> Handler a
f <+> g = \e -> (f e) . (g e)

withKey :: (Char -> a -> a) -> Handler a
withKey f e = case e of
    EvKey (KASCII c) [] -> f c
    _ -> id

onKey :: Char -> (a -> a) -> Handler a
onKey c f e = case e of
    EvKey (KASCII c) [] -> f
    _ -> id

onEnter :: (a -> a) -> Handler a
onEnter f e = case e of
    EvKey (KEnter) [] -> f
    _ -> id

onBack :: (a -> a) -> Handler a
onBack f e = case e of
    EvKey (KBS) [] -> f
    _ -> id

onShift :: Char -> (a -> a) -> Handler a
onShift c f e = case e of
    EvKey (KASCII c) [MShift] -> f
    _ -> id

onCtrl :: Char -> (a -> a) -> Handler a
onCtrl c f e = case e of
    EvKey (KASCII c) [MCtrl] -> f
    _ -> id
