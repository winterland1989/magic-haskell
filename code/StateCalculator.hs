module StateCalculator where

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f fs = State $ \s ->
        let (a, s') = runState fs s
        in  (f a, s')

instance Applicative (State s) where
    pure a = State $ \s -> (a, s)
    f <*> fa = State $ \s ->
        let (fab, s0) = runState f s
            (a, s1) = runState fa s0
        in (fab a, s1)

instance Monad (State s) where
    return = pure
    fa >>= f = State $ \s ->
        let (a, s') = runState fa s
        in runState (f a) s'

-- 对“全局变量”做加减乘除
(~+) :: Double -> State Double (Double -> Double)
(~+) x = State $ \s -> ((+x), s + x)

(~-) :: Double -> State Double (Double -> Double)
(~-) x = State $ \s -> (((-)x), s - x)

(~*) :: Double -> State Double (Double -> Double)
(~*) x = State $ \s -> ((*x), s * x)

(~/) :: Double -> State Double (Double -> Double)
(~/) x = State $ \s -> ((/x), s / x)

-- 重复某个计算
(~~) :: (Double -> Double) -> State Double (Double -> Double)
(~~) f = State $ \s -> (f, f s)

--
op :: State Double (Double -> Double)
op = do
    (~+) 10
    (~*) 4
    (~-) 2
    (~/) 10
    >>= (~~)
    >>= (~~)


main :: IO ()
main = do
    let (_, result) = runState op 0
    print result
