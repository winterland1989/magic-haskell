{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}


module HList where


data HList :: [*] -> * where
    HNil  :: HList '[]
    HCons :: x -> HList xs -> HList (x ': xs)

class GetByType a xs where
    getByType :: HList xs -> a

instance {-# OVERLAPPING #-} GetByType a (a ': xs) where
    getByType (HCons x _) = x

instance GetByType a xs => GetByType a (b ': xs) where
    getByType (HCons _ xs) = getByType xs


main = do
     let hlist = HCons (2 :: Int) $ HCons "hello" $ HCons True HNil
     print (getByType hlist :: Int)
     print (getByType hlist :: String)
     print (getByType hlist :: Bool)
