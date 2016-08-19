module ArgParser where

import Options.Applicative

data HostMessage = HostMessage String Message

data Message  =
    Greet { hello :: String, quiet :: Bool }
    | Farewell { bye :: String, quiet :: Bool }

hostParser =  strOption
    ( long "host"
    <> metavar "HOST"
    <> help "Who is the host" )

quietParser =  switch
    ( long "quiet"
    <> help "Whether to be quiet" )

greetParser :: Parser Message
greetParser = Greet
    <$> strOption
        ( long "hello"
        <> metavar "TARGET"
        <> help "Target for the greeting" )
    <*> quietParser

farewellParser :: Parser Message
farewellParser = Farewell
    <$> strOption
        ( long "bye"
        <> metavar "TARGET"
        <> help "Target for the farewell" )
    <*> quietParser

messageParser ::  Parser HostMessage
messageParser = HostMessage <$> hostParser <*> (greetParser <|> farewellParser)

main :: IO ()
main = do
    greet <- execParser $ info messageParser mempty
    case greet of
        HostMessage host (Greet hello False)
            -> putStrLn $ "Hello, " ++ hello ++ ", from " ++ host
        HostMessage host (Farewell bye False)
            -> putStrLn $ "Bye, " ++ bye ++ ", from " ++ host
        _                       -> return ()
