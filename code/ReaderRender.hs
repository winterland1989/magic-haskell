module ReaderRender where

import Control.Applicative

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap f m = Reader $ \r -> f (runReader m r)

instance Applicative (Reader r) where
    pure a = Reader $ \_ -> a
    a <*> b  = Reader $ \r -> runReader a r $ runReader b r

instance Monad (Reader r) where
    return = pure
    m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

headT :: Reader String String
headT = Reader $ \name -> "Welcome! " ++ name ++ ".\n"

bodyT :: Reader String String
bodyT = Reader $ \name ->
    "Welcome to my home, "
    ++ name
    ++ ". This's best home you can ever find on this planet!\n"

footT :: Reader String String
footT = Reader $ \name -> "Now help yourself, " ++ name ++ ".\n"

data Greet = Greet {
        greetHead :: String
    ,   greetBody :: String
    ,   greetFoot :: String
    } deriving Show

renderGreeting :: Reader String Greet
renderGreeting = do
    h <- headT
    b <- bodyT
    f <- footT
    return $ Greet h b f

main :: IO ()
main = print $ runReader renderGreeting "Han"
