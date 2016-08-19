{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Trans
import Control.Monad
import Text.Read (readMaybe)

calculator :: WriterT String (StateT Double IO) ()
calculator = do
    result <- get
    liftIO $ print result
    (op:input) <- liftIO getLine
    let opFn = case op of
            '+' -> sAdd
            '-' -> sMinus
            '*' -> sTime
            '/' -> sDivide
            _ -> const $ return ()
    case readMaybe input of
        Just x -> opFn x >> calculator
        Nothing -> tell "Illegal input.\n"
  where
    sAdd x = do
        tell $ "Add: " ++ (show x) ++ "\n"
        modify (+ x)
    sMinus x = do
        tell $ "Minus: " ++ (show x) ++ "\n"
        modify (\y -> y - x)
    sTime x = do
        tell $ "Time: " ++ (show x) ++ "\n"
        modify (* x)
    sDivide x = do
        tell $ "Divide: " ++ (show x) ++ "\n"
        modify (/ x)

main :: IO ()
main = (flip evalStateT) 0 $ do
        log <- execWriterT calculator
        liftIO $ do
            putStr "Calculator log:\n"
            putStr log
