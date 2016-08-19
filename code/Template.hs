{-# LANGUAGE TemplateHaskell #-}

module Template where

import MakeLenses
import Lens ((^.), (%~), (.~))
import Data.Function ((&))

data Pos = Pos { _x :: Int, _y :: Int } deriving Show
makeLenses ''Pos

main = do
    let p = Pos 1 2
    putStrLn $ "p & x %~ negate:     " ++ show (p & x %~ negate)
    putStrLn $ "p & x .~ 0:          " ++ show (p & x .~ 0)
    putStrLn $ "p ^. y:              " ++ show (p ^. y)
