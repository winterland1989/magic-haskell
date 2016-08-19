{-# LANGUAGE Rank2Types #-}

module Lens where

import Data.Functor.Identity  (Identity(..))
import Control.Applicative    (Const(..))
import Data.Function          ((&))

--------------------------------------------------------------------------------

over :: ((a -> Identity a) -> b -> Identity b) -> (a -> a) -> b -> b
over lens f x = runIdentity $ lifted x
  where
    lifted = lens (Identity . f)

set :: ((a -> Identity a) -> b -> Identity b) -> a -> b -> b
set lens a' x = over lens (\_ -> a') x

view :: ((a -> Const a a) -> b -> Const a b) -> b -> a
view lens x = getConst ((lens Const) x)

--------------------------------------------------------------------------------

data Position = Position { positionX :: Double, positionY :: Double } deriving Show

type Lens b a = forall f.Functor f => (a -> f a) -> b -> f b

xLens :: Lens Position Double
xLens f p = fmap (\x' -> setPositionX x' p) $ f (positionX p)
  where
    setPositionX :: Double -> Position -> Position
    setPositionX x' p = p { positionX = x' }

yLens :: Lens Position Double
yLens f p = fmap (\y' -> p { positionY = y' }) $ f (positionY p)

-- 中缀版本view
(^.) :: b -> Lens b a -> a
(^.) = flip view
infixl 8 ^.

-- 中缀版本over
(%~) :: Lens b a -> (a -> a) -> b -> b
(lens %~ f) x = over lens f x
infixr 4 %~

-- 中缀版本set
(.~) :: Lens b a -> a -> b -> b
(.~) = set
infixr 4 .~

main = do
    let p = Position 123 456
    putStrLn $ "orign value: " ++ show p
    putStrLn $ "over xLens negate p:     " ++ show (over xLens negate p)
    putStrLn $ "set xLens 0 p:           " ++ show (set xLens 0 p)
    putStrLn $ "view yLens p:            " ++ show (view yLens p)
    putStrLn $ "p & xLens %~ negate:     " ++ show (p & xLens %~ negate)
    putStrLn $ "p & xLens .~ 0:          " ++ show (p & xLens .~ 0)
    putStrLn $ "p ^. yLens:              " ++ show (p ^. yLens)
