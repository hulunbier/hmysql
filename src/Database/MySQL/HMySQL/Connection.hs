{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Database.MySQL.HMySQL.Connection
     ( ConnectInfo(..)
     , defaultConnectInfo
     , connectDB
     , closeConn
     , Connection
     , StreamTextResponse(..)
     , StreamResultSetPackets(..)
     , ResultSetNotFullyConsumed
     , run
     , runS
     , withRows
     , connGreet -- TODO
     , ConnFaliure
     )
where

import           Control.Applicative            ((<$>), (<*>))
import           Control.Exception              (Exception, bracketOnError,
                                                 throw, throwIO)
import           Control.Monad.Error
import           Control.Monad.State.Strict
import qualified Crypto.Hash.SHA1               as SHA1
import           Data.Binary.Get
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

data ConnectInfo = ConnectInfo
               { ciHost       :: String
               , ciPort       :: PortID
               , ciDatabase   :: String
               , ciUser       :: String
               , ciPassword   :: String
               , ciBufferSize :: Int
               } deriving Show

type RowStream = Producer (Packed RowData) IO ()

data StreamResultSetPackets = StreamResultSetPackets
    { srColDefs :: [Packed ColDef]
    , srRows    :: RowStream
    }

data StreamTextResponse = SRes    StreamResultSetPackets
                        | SResErr (Packed ERR)
                        | SResOK  (Packed OK)

defaultConnectInfo :: ConnectInfo
defaultConnectInfo = ConnectInfo "localhost" (Service "3306") "" "root" "" 4096

type BytesProducer = Producer ByteString IO ()

data ConnFaliure = AuthFaliure ERR deriving (Typeable, Show)
instance Exception ConnFaliure

data Connection = Connection
    { _cs       :: Handle
    -- Not wrap BytesProducer within sth like IORef here
    -- or thread this state around, because after each invocation
    -- of 'run' or 'runS', reponses (bytes from server) are fully
    -- consumed.

    -- This looks a little weird, though.

    -- TODO But what if we should provide some function like "fetchRow conn"?

    , _cp       :: BytesProducer
    , connGreet :: Greeting}

connectDB :: ConnectInfo -> IO Connection
connectDB (ConnectInfo host portId db user pass bufSize ) =
    bracketOnError connSock hClose
        (\sock -> do
            (res, prod) <- parseGreeting sock bufSize
            either throwIO (\g -> doAuth g sock prod user pass db) res
        )
      where
        connSock = connectTo host portId
        greeting =  PB.decodeGet (decodeGreeting <$> getPacket)
        parseGreeting s sz = runStateT greeting (PBS.hGetSome sz s )

closeConn :: Connection -> IO ()
closeConn (Connection s _ _) = hClose s

run :: Connection -> ByteString -> (TextResponse -> IO a) -> IO a
run (Connection s p _) qry io = do
    sendQry s qry
    (r,_) <- runStateT (PB.decodeGet getTextResponse) p
    either throwIO io r

withRows :: RowStream -> (RowData -> IO ()) -> IO ()
withRows rows io = runEffect $ for rows $ lift . io . payload

probResp :: Get (Maybe StreamTextResponse)
probResp = do
    pkt <- lookAhead getPacket
    build pkt
    where build p
            | isErrP  p  = Just <$> SResErr
                                <$> (Packed
                                        <$> getPacketHeader
                                        <*> getERR (pLen p))
            | isOKP   p  = Just <$> (SResOK <$> getPacked)
            | otherwise  = return Nothing

data ResultSetNotFullyConsumed = ResultSetNotFullyConsumed deriving (Typeable, Show)
instance Exception ResultSetNotFullyConsumed

runS
    :: Connection
    -> ByteString
    -> (StreamTextResponse -> IO ())
    -> IO ()
runS (Connection s p _) qry io = do
    sendQry s qry
    (r, p') <- runStateT (PB.decodeGet probResp) p
    let buildRes x = case x of
            Just a -> io a
            Nothing -> do
                ref <- newIORef False
                (r', p'') <- runStateT (PB.decodeGet getResultSetHeader) p'
                let feedRows (ResultSetHeader rc rd _) = do
                        io $ SRes (StreamResultSetPackets
                                    rd
                                    (rowStream p'' (payload rc) ref))
                        drained <- readIORef ref
                        unless drained $ throwIO ResultSetNotFullyConsumed
                either throwIO feedRows r'
    either throwIO buildRes r

rowStream
    :: BytesProducer
    -> ColCount
    -> IORef Bool
    -> Producer (Packed RowData) IO ()

rowStream p (ColCount cc) st =
    let getRow = do
            e <- lookAheadM getMaybePackedEOF
            case e of
                Just _  -> return Nothing
                Nothing -> Just <$> (Packed
                                     <$> getPacketHeader
                                     <*> getRowData cc)

        fetch prod = do
            (rRowData, p') <- liftIO $ runStateT (PB.decodeGet getRow) prod
            case rRowData of
                Right (Just rd) -> yield rd >> fetch p'
                Right Nothing   -> liftIO $ writeIORef st True >> return ()
                Left e          -> throw e
    in fetch p


sendQry :: Handle -> ByteString -> IO ()
sendQry s qry =
    let cmd    = CmdQry $ fromStrict qry
        cmdStr = P.runPut $ putMySqlCmd cmd
        buffer = P.runPut $ mysqlPack 0 cmdStr
    in B.hPut s $ toStrict buffer

scramble :: ByteString -> ByteString -> ByteString
scramble salt pass
    | B.null pass = B.empty
    | otherwise   = B.pack $ B.zipWith xor sha1pass withSalt
            where sha1pass = SHA1.hash pass
                  withSalt = SHA1.hash
                            $ B.append salt
                            $ SHA1.hash sha1pass
sendAuth
    :: Handle
    -> Greeting
    -> ByteString
    -> ByteString
    -> ByteString
    -> IO ()
sendAuth h greet user pass db =
        let salt = B.append (salt1 greet) (salt2 greet)
            scambleBuf = scramble salt pass
            auth = Auth defaultClientCap
                        defaultClientMaxPacketSize
                        defaultClientCharset
                        (fromStrict user) scambleBuf db
            login = P.runPut $ putAuth auth
            -- sequence number is always 1 while sending auth to server
            buffer = P.runPut $ mysqlPack 1 login
        in B.hPut h $ toStrict buffer

doAuth :: Greeting
    -> Handle
    -> BytesProducer
    -> String
    -> String
    -> String
    -> IO Connection
doAuth g h p user pass db =
    let buildConn packet p'
            | isOKP packet = Connection h p' g
            | otherwise =
                throw $ AuthFaliure (decodeErr packet)
    in
    sendAuth h g (pack user) (pack pass) (pack db)
    >> parseAuthAck p
    >>= \(x, p') -> return $ either throw buildConn x p'

parseAuthAck :: BytesProducer
             -> IO (Either PB.DecodingError Packet, BytesProducer)
parseAuthAck = runStateT (PB.decodeGet getPacket)

decodeErr :: Packet -> ERR
decodeErr pkt = runGet (getERR $ pLen pkt) (pBody pkt)

--------------------------------------------------

