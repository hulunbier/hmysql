{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           Connection
import           Control.Monad.Trans
import           Data.ByteString.Char8    (pack)
--import           Pipes
--import qualified Pipes.Prelude            as PP
import           Protocol
import           System.Console.CmdArgs
import           System.Console.Haskeline
--import           System.Posix
import qualified Text.Show.Pretty         as Pr

main :: IO ()
main = do
    opts <- cmdArgs defalutFlags
    print opts
    -- _ <- installHandler sigPIPE Ignore Nothing
    conn <- connectDB $ defaultConnectInfo
                            { ciHost     = host opts
                            , ciSerivce  = port opts
                            , ciUser     = user opts
                            , ciPassword = pass opts
                            , ciDatabase = db opts
                            }
    repl conn

repl :: Connection -> IO ()
repl conn = runInputT defaultSettings $ do
        outputStrLn (Pr.ppShow $ connGreet conn) -- greeting packet, diagnostics
        loop
        where loop = do
                minput <- getInputLine "hmysql> "
                maybe (outputStrLn "Bye.") exec minput
                where exec input =
                        case input of
                            '!':rest -> do
                                liftIO $ showStream rest conn
                                loop
                            ':':rest -> do
                                msg <- liftIO $ run conn (pack rest ) (return . Pr.ppShow)
                                outputStrLn msg
                                loop
                            x -> do
                                msg <- liftIO $ run conn (pack x) showRow
                                outputStrLn msg
                                loop
                                where
                                showRow r = return $ case r of
                                    RSRes (ResultSetPackets _ _ _ rows _) -> Pr.ppShow rows
                                    a -> Pr.ppShow a

showStream :: String -> Connection -> IO ()
showStream qry conn =
    runS conn (pack qry) $ do
        \res ->
            case res of
                SRes (StreamResultSetPackets _ rows) ->
                        --runEffect (rows >-> PP.take 2 >-> PP.print)
                        withRows rows $ \r -> runInputT defaultSettings $ outputStrLn $ Pr.ppShow r
                        --withRows rows (cosnt $ return ())
                SResOK x ->  print $ Pr.ppShow x
                SResErr x -> print "*err*" >> (print $ Pr.ppShow x)

data Flags = Flags
    { host :: String
    , port :: String
    , user :: String
    , pass :: String
    , db   :: String
    } deriving (Show, Data, Typeable)

defalutFlags :: Flags
defalutFlags = Flags
    { host = "localhost" &= typ "hostname"
    , port = "3306"      &= typ "port"
    , user = "root"      &= typ "username"
    , pass = ""          &= typ "password"
    , db   = ""          &= typ "database"
    }
    &= summary "Silly MySQL client"
    &= program "repl"


