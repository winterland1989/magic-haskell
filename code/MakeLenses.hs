{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module MakeLenses where

import Language.Haskell.TH
import Control.Monad

-- 帮助函数，用来生成一个Name对应的Pat和Exp
mkVars :: String -> Q (PatQ, ExpQ)
mkVars name = do
    x <- newName name
    return (varP x, varE x)

makeLenses :: Name -> DecsQ
makeLenses typeName = do
    -- 获取构造函数
    TyConI (DataD _ _ [] cons _) <- reify typeName

    -- 获取构造函数对应的数据项信息
    -- 假设记录只有一个构造函数
    [RecC conName fields] <- return cons

    -- 遍历每一个数据项生成透镜
    fmap concat $
        forM fields $ \(fieldName, _, fieldType) ->
            case nameBase fieldName of
                -- 只对`_`开头的数据项生成透镜
                ('_':rest) ->
                    makeLens typeName conName (mkName rest) fieldName fieldType
                _ -> return []

makeLens
    :: Name    -- 数据类型Name
    -> Name    -- 构造函数Name
    -> Name    -- Lens Name
    -> Name    -- 数据项Name
    -> Type    -- 数据项类型
    -> DecsQ
makeLens typeName conName lensName fieldName fieldType = do

    -- (a -> f a) -> b -> f b
    let bT = conT typeName
        aT = return fieldType
        lensT = [t|(Functor f) => ($aT -> f $aT) -> $bT -> f $bT|]

    sig <- sigD lensName lensT

    -- 新建Name和对应的Pat, Exp
    (fP, fE) <- mkVars "f"
    (bP, bE) <- mkVars "b"
    (xP, xE) <- mkVars "x"
    xE' <- xE

    let -- \x -> (\y b -> b { field = y }) x p
        lam  = [| \ $xP -> $(recUpdE bE [return (fieldName, xE')]) |]
        pats = [fP, bP]
        rhs  = [|fmap $lam ($fE ($(varE fieldName) $bE))|]
    body <- funD lensName [clause pats (normalB rhs) []]

    -- 返回[Dec]
    return [sig, body]

