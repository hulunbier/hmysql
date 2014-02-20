{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.MySQL.HMySQL.NewConnection
where

import           Control.Applicative            ((<$>))
import           Control.Exception              (Exception, bracketOnError,
                                                 throw, throwIO)
import           Control.Monad.Error
import           Control.Monad.State.Strict
import qualified Crypto.Hash.SHA1               as SHA1
import qualified Data.Binary.Put                as P
import           Data.Bits                      (xor)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as B
import           Data.ByteString.Char8          (pack)
import           Data.ByteString.Lazy           (fromStrict, toStrict)
import           Data.IORef                     (IORef, newIORef, readIORef,
                                                 writeIORef)
import           Data.Typeable
import           Database.MySQL.HMySQL.Protocol
import           Network                        (PortID (Service), connectTo)
import           Pipes
import qualified Pipes.Binary                   as PB
import qualified Pipes.ByteString               as PBS
import           System.IO                      (Handle, hClose)
import Control.Monad.Reader

type RowStream = Producer (Packed RowData) IO ()
type BytesProducer = Producer ByteString IO ()

data ConnState = ConnState
    { csHandle :: Handle
    , csSeqNum :: PBS.Word8 
    , csProd   :: BytesProducer 
    }

data ConnectInfo = ConnectInfo
               { ciHost       :: String
               , ciPort       :: PortID
               , ciDatabase   :: String
               , ciUser       :: String
               , ciPassword   :: String
               , ciBufferSize :: Int
               } deriving Show

defaultConnectInfo :: ConnectInfo
defaultConnectInfo = ConnectInfo "localhost" (Service "3306") "" "root" "" 4096

data Connection = Connection (IORef (ConnState, ConnectInfo))

newtype HMySQL a = HMySQL {
      runMysql :: ReaderT ConnectInfo (StateT ConnState IO) a
    } deriving (Monad, MonadIO, MonadReader ConnectInfo,
                MonadState ConnState)

resetSeqNum :: HMySQL ()
resetSeqNum = modify (\x -> x {csSeqNum = 0})

data ConnFaliure = AuthFaliure ERR deriving (Typeable, Show)
instance Exception ConnFaliure

connectDB :: ConnectInfo -> IO Connection
connectDB ci@(ConnectInfo host portId _ _ _ bufSz) =
    bracketOnError conn hClose
        (\handle -> do
            let prod = PBS.hGetSome bufSz handle
                initStat = ConnState handle 0 prod
            (a,s) <- runStateT (runReaderT (runMysql $ dialog connect') ci) initStat
            case a of 
                AuthAck (Left err) -> throw $ AuthFaliure err 
                AuthAck (Right _) -> Connection <$> newIORef (s, ci)
        )
    where
        conn = connectTo host portId


closeConn :: Connection -> IO ()
closeConn (Connection ref) = readIORef ref >>= \(s, _) -> hClose $ csHandle s

run :: Connection -> ByteString -> (TextResponse -> IO a) -> IO a
run (Connection ref ) qry io = do
    (cs, ci) <- readIORef ref
    (a, cs')<- runStateT (runReaderT (runMysql $ dialog (run' qry io)) ci) cs 
    writeIORef ref (cs', ci)
    return a


run' :: ByteString -> (TextResponse -> IO a) -> HMySQL a
run' qry io = do
    sendQry qry
    p <- gets csProd
    (r,_) <- liftIO $ runStateT (PB.decodeGet getTextResponse) p
    either (liftIO . throwIO) (liftIO . io) r

withRows :: RowStream -> (RowData -> IO ()) -> IO ()
withRows rows io = runEffect $ for rows $ lift . io . payload


dialog :: HMySQL a -> HMySQL a
dialog act =  do
    res <- act
    resetSeqNum
    return res

connect' :: HMySQL AuthAck 
connect' = do
    greeting <- readPacket
    mkAuth greeting >>= sendAuth
    readPacket 

mkAuth :: Greeting -> HMySQL Auth
mkAuth greet  = do
    db <- asks ciDatabase
    user <- asks ciUser
    pass <- asks ciPassword
    let salt = B.append (salt1 greet) (salt2 greet)
        scambleBuf = scramble salt (pack pass)
    return $ Auth defaultClientCap
                    defaultClientMaxPacketSize
                    defaultClientCharset
                    (fromStrict (pack user)) scambleBuf (pack db)

sendAuth :: Auth -> HMySQL ()
sendAuth auth = do
        h <- gets csHandle
        let login = P.runPut $ putAuth auth
            buffer = P.runPut $ mysqlPack 1 login
        liftIO $ B.hPut h $ toStrict buffer
        incrSeqNum

incrSeqNum :: HMySQL ()
incrSeqNum = modify (\x -> x {csSeqNum = csSeqNum x + 1})

readPacket :: (LenConstrained a) => HMySQL a
readPacket = do
    p <- gets csProd
    seqNum <- gets csSeqNum
    (r, p') <- liftIO $ runStateT (PB.decodeGet (getPackedC seqNum)) p
    modify (\x -> x {csProd = p', csSeqNum = csSeqNum x + 1})
    either throw (return . payload) r

sendQry :: ByteString -> HMySQL ()
sendQry qry = do
    h   <- gets csHandle
    seqNum <- gets csSeqNum
    let cmd    = CmdQry $ fromStrict qry
        cmdStr = P.runPut $ putMySqlCmd cmd
        buffer = P.runPut $ mysqlPack seqNum cmdStr
    liftIO (B.hPut h $ toStrict buffer)
    incrSeqNum

scramble :: ByteString -> ByteString -> ByteString
scramble salt pass
    | B.null pass = B.empty
    | otherwise   = B.pack $ B.zipWith xor sha1pass withSalt
            where sha1pass = SHA1.hash pass
                  withSalt = SHA1.hash
                            $ B.append salt
                            $ SHA1.hash sha1pass
