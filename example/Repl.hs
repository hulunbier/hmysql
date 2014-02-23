{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           Control.Monad.Trans
import           Data.ByteString.Char8            (pack)
--import           Database.MySQL.HMySQL.Connection
import           Database.MySQL.HMySQL.NewConnection
--import           Pipes
--import qualified Pipes.Prelude            as PP
import           Database.MySQL.HMySQL.Protocol
import           Network                          (PortID (..))
import           System.Console.CmdArgs
import           System.Console.Haskeline
import qualified Text.Show.Pretty                 as Pr

main :: IO ()
main = do
    opts <- cmdArgs defalutFlags
    showOpts opts
    conn <- connectDB $ buildConnectInfo opts
    repl conn

repl :: Connection -> IO ()
repl conn = runInputT defaultSettings $ do
        outputStrLn "--- Greeting message from server ---"
--        outputStrLn (Pr.ppShow $ connGreet conn) -- greeting packet, diagnostics
        outputStrLn "--- End of greeting message ---"
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
                            [] -> loop
                            x  -> do
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
    , port :: Int
    , user :: String
    , pass :: String
    , db   :: String
    , sock :: String
    } deriving (Show, Data, Typeable)

defalutFlags :: Flags
defalutFlags = Flags
    { host = "localhost" &= typ "hostname"
    , port = 3306        &= typ "port"
    , user = "root"      &= typ "username"
    , pass = ""          &= typ "password"
    , db   = ""          &= typ "database"
    , sock = ""          &= typ "unix socket file"
    }
    &= summary "Silly MySQL client"
    &= program "repl"

showOpts :: Flags -> IO ()
showOpts opts = do
    putStrLn "\n--- Options used ---\n"
    print opts
    putStrLn "\n--- End of Options ---\n"

buildConnectInfo :: Flags -> ConnectInfo
buildConnectInfo opts = defaultConnectInfo
                    { ciHost     = host opts
                    , ciPort     = let s = (sock opts)
                                   in if s /= ""
                                      then (UnixSocket s)
                                      else (PortNumber $ 3306)
                    , ciUser     = user opts
                    , ciPassword = pass opts
                    , ciDatabase = db opts
                    }
