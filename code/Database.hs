{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Trans.Reader
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Control.Monad.Logger
import           Database.Esqueleto as E

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

main :: IO ()
main = runNoLoggingT . withSqlitePool "test.db" 10 . runSqlPool $ do
    runMigration migrateAll

    johnId <- insert $ Person "John" (Just 18)
    johnId <- insert $ Person "Peter" (Just 20)
    johnId <- insert $ Person "Mary" (Just 30)
    johnId <- insert $ Person "Jane" (Just 14)

    people <- E.select $ E.from $ \person -> return person
    liftIO $ mapM_ (putStrLn . personName . entityVal) people

    people <-
        E.select $
        E.from $ \p -> do
        where_ (p E.^. PersonAge E.>. just (val 18))
        return p

    liftIO $ putStrLn "People older than 18 are:"
    liftIO $ mapM_ (putStrLn . personName . entityVal) people
