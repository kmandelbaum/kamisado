module Utility where
import Control.Applicative

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

ifF :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
ifF = liftA3 if'
