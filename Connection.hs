{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Connection
     ( ConnectInfo(..)
     , defaultConnectInfo
     , connectDB
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

import           Control.Applicative        ((<$>), (<*>))
import           Control.Exception          (Exception, onException, throw,
                                             throwIO)
import           Control.Monad.Error
import           Control.Monad.State.Strict
import qualified Crypto.Hash.SHA1           as SHA1
import           Data.Binary.Get
import qualified Data.Binary.Put            as P
import           Data.Bits                  (xor)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import           Data.ByteString.Char8      (pack)
import           Data.ByteString.Lazy       (fromStrict, toStrict)
import           Data.IORef                 (IORef (), newIORef, readIORef,
                                             writeIORef)
import           Data.Typeable
import           Network.Socket
import           Network.Socket.ByteString  (sendAll)
import           Pipes
import qualified Pipes.Binary               as PB
import qualified Pipes.Network.TCP          as PN
import           Protocol

data ConnectInfo = ConnectInfo
               { ciHost       :: String
               , ciSerivce    :: String
               , ciDatabase   :: String
               , ciUser       :: String
               , ciPassword   :: String
               , ciBufferSize :: Int
               , ciNoDelay    :: Int
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
defaultConnectInfo = ConnectInfo "localhost" "3306" "" "root" "" 4096 1

type BytesProducer = Producer ByteString IO ()

data ConnFaliure = AuthFaliure ERR deriving (Typeable, Show)
instance Exception ConnFaliure

data Connection = Connection
    { _cs        :: Socket
    , _cp        :: BytesProducer
    , connGreet :: Greeting}

connectDB :: ConnectInfo -> IO Connection
connectDB (ConnectInfo host service db user pass bs nd) = do
    sock <- connSock
    onException (do
        (res, prod) <- parseGreeting sock bs
        case res of
            Right g ->
                sendAuth sock g (pack user) (pack pass) (pack db)
                >> parseAuthAck prod
                >>= \(x, p') ->
                    let buildConn packet
                            | isOKP packet = Connection sock p' g
                            | otherwise =
                                throw (AuthFaliure $
                                        runGet (getERR (pLen packet))
                                            (pBody packet))
                    in return $ either throw buildConn x
            Left e -> throwIO e)
        $ sClose sock
    where
    connSock = do
            addrs <- getAddrInfo Nothing (Just host) (Just service)
            let addr = head addrs
            sock <- socket (addrFamily $ head addrs) Stream defaultProtocol
            setSocketOption sock NoDelay nd
            connect sock (addrAddress addr)
            return sock
    parseGreeting s sz = runStateT greeting (PN.fromSocket s sz)
    parseAuthAck = runStateT (PB.decodeGet getPacket)
    greeting =  PB.decodeGet (decodeGreeting <$> getPacket)


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
            | isErrP  p  = Just <$> (SResErr
                                    <$> (Packed
                                         <$> getPacketHeader
                                         <*> (getERR $ pLen p)))
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
                                    (rowStream p'' (payload rc) (ref)))
                        drained <- readIORef ref
                        when (not drained) $ throwIO ResultSetNotFullyConsumed
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
                                     <*> (getRowData cc))

        fetch prod = do
            (rRowData, p') <- liftIO $ runStateT (PB.decodeGet getRow) prod
            case rRowData of
                Right (Just rd) -> yield rd >> fetch p'
                Right Nothing   -> liftIO $ writeIORef st True >> return ()
                Left e          -> throw e
    in fetch p


sendQry :: Socket -> ByteString -> IO ()
sendQry s qry =
    let cmd    = CmdQry $ fromStrict qry
        cmdStr = P.runPut $ putMySqlCmd cmd
        buffer = P.runPut $ mysqlPack 0 cmdStr
    in sendAll s $ toStrict buffer

scramble :: ByteString -> ByteString -> ByteString
scramble salt pass
    | B.null pass = B.empty
    | otherwise   = B.pack $ B.zipWith xor sha1pass withSalt
            where sha1pass = SHA1.hash pass
                  withSalt = SHA1.hash
                            $ B.append salt
                            $ SHA1.hash sha1pass
sendAuth
-- :: MonadIO m =>
    :: Socket
    -> Greeting
    -> ByteString
    -> ByteString
    -> ByteString
    -> IO ()
sendAuth sock greet user pass db =
        let salt = B.append (salt1 greet) (salt2 greet)
            scambleBuf = scramble salt pass
            auth = Auth defaultClientCap
                        defaultClientMaxPacketSize
                        defaultClientCharset
                        (fromStrict user) scambleBuf db
            login = P.runPut $ putAuth auth
            buffer = P.runPut $ mysqlPack 1 login
        in sendAll sock $ toStrict buffer


--------------------------------------------------

