{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Database.MySQL.HMySQL.Protocol where

import           Control.Applicative   ((*>), (<$>), (<*), (<*>))
import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import qualified Data.Binary.Put       as P
import           Data.Bits
import qualified Data.ByteString       as B
import           Data.ByteString.Char8 hiding (reverse)
import qualified Data.ByteString.Lazy  as L

-- types

data Packet = Packet
    { pLen  :: !Int
    , pSeq  :: !Word8
    , pBody :: !L.ByteString
    } deriving Show

data PacketHeader = PacketHeader
    { hLen :: !Int
    , hSeq :: !Word8
    } deriving Show

data Packed a = Packed
    { header  :: !PacketHeader
    , payload :: !a
    } deriving Show

data LenEncStr = LCS B.ByteString | Null deriving Show

-- general packets

data OK = OK 
    { okAffectedRows :: Int
    , okLastInsertID :: Int
    , okStatus       :: Word16
    , okWarnings     :: Word16
    } deriving Show

data ERR = ERR
    { errCode  :: !Word16
    , sqlState :: !L.ByteString
    , errMsg   :: !L.ByteString
    } deriving (Show)

------------
-- Authentications

data Greeting = Greeting
    { protocol :: !Word8
    , version  :: !B.ByteString
    , tid      :: !Word32
    , salt1    :: !B.ByteString --TODO rename
    , caps     :: !Word16
    , lang     :: !Word8
    , status   :: !Word16
    , salt2    :: !B.ByteString
    } deriving (Show)

data Auth = Auth
    { acaps     :: !Word32
    , maxPacket :: !Word32
    , charset   :: !Word8
    , name      :: !L.ByteString
    , password  :: !ByteString
    , schema    :: !ByteString
    } deriving (Show)
-----------------
-- resultset

data EOF = EOF
    { warningCount :: !Word16
    , statusFlags  :: !Word16
    } deriving Show

data ColCount = ColCount Int deriving Show

data ColDef = ColDef
    { fdb, ftbl, forgTbl, fName, forgName :: !LenEncStr
    , fcharset                            :: !Word16
    , flen                                :: !Word32
    , ftype                               :: !Word8
    , flags                               :: !Word16
    , fdecimals                           :: !Word8
    } deriving Show

data ResultSetHeader = ResultSetHeader
    { rhColCount :: !(Packed ColCount)
    , rhColDefs  :: ![Packed ColDef]
    , rhEOF1     :: !(Packed EOF)
    } deriving Show

data ResultSetPackets = ResultSetPackets
    { rColCount :: !(Packed ColCount)
    , rColDefs  :: ![Packed ColDef]
    , rEOF1     :: !(Packed EOF)
    , rRows     :: ![Packed RowData]
    , rEOF2     :: !(Packed EOF)
    } deriving Show

data TextResponse = RSRes  ResultSetPackets
                  | ErrRes (Packed ERR)
                  | OKRes  (Packed OK)
                  deriving Show

-----------------------


-- https://dev.mysql.com/doc/internals/en/integer.html#packet-Protocol::LengthEncodedInteger
getLen :: Word8 -> Get Int
getLen l
     | l <= 250  = return $ fromIntegral l
     --l == 251    NULL Str
     | l == 252  = fromIntegral <$> getWord16le
     | l == 253  = getWord24
     | l == 254  = fromIntegral <$> getWord64le
     | otherwise = fail $ "invalid length val " ++ show l

-- length encoded int
getLenEncInt:: Get Int
getLenEncInt = getWord8 >>= getLen

getWord24 :: Get Int
getWord24 = do
    a <- fromIntegral <$> getWord8
    b <- fromIntegral <$> getWord8
    c <- fromIntegral <$> getWord8
    return $ a .|. (b `shiftL` 8) .|. (c `shiftL` 16)

putWord24 :: Int -> Put
putWord24 v = do
    putWord8 $ fromIntegral $ v `shiftR` 16
    putWord8 $ fromIntegral $ v `shiftR` 8
    putWord8 $ fromIntegral $ v

decodeCmd :: Packet -> Cmd
decodeCmd p = runGet getMySqlCmd $ pBody p

decodeGreeting :: Packet -> Greeting
decodeGreeting p = runGet getGreeting $ pBody p

encodePacket :: Packet -> L.ByteString
encodePacket (Packet _ s b)  = P.runPut $ mysqlPack s b --todo len is not used

data MySqlCmd = CmdQry L.ByteString
               | CmdPing
               | CmdKill Word32
               | CmdQuit
               | CmdUnsupported
               deriving (Show)

getMySqlCmd :: Get MySqlCmd
getMySqlCmd = do
            cmdId <- getWord8
            case cmdId of
                0x03 -> do body <- getRemainingLazyByteString -- TODO size limits
                           return $ CmdQry body
                0x0e -> return CmdPing
                0x01 -> return CmdQuit
                -- 0x0c -> getWord32le >>= return . CmdKill
                0x0c -> CmdKill <$> getWord32le
                _    -> return CmdUnsupported

type Cmd = MySqlCmd

putMySqlCmd :: MySqlCmd -> P.Put
putMySqlCmd (CmdQry q) = P.putWord8 0x03 >> P.putLazyByteString q
putMySqlCmd CmdPing = P.putWord8 0x0e
putMySqlCmd CmdQuit = P.putWord8 0x01
putMySqlCmd (CmdKill sid) = P.putWord8 0x0c >> P.putWord32le sid
putMySqlCmd CmdUnsupported = fail "CmdUnsupported"  -- yuck


data Field = Field LenEncStr FieldType

data RowData = RowData [LenEncStr] deriving Show

getRowData :: Int -> Get RowData
getRowData l = RowData <$> getMany l

instance Binary ColDef where
    get = getColDef
    put = undefined --TODO

getColDef :: Get ColDef
getColDef = ColDef
        <$> (skip 4      -- const "DEF"
         *> get)         -- db
        <*> get          -- tbl
        <*> get          -- org_tbl
        <*> get          -- name
        <*> get          -- org_name
        <*  skip 1       -- const 0x0c (seems it's a length indicator)
        <*> getWord16le  -- charset,
        <*> getWord32le  -- length
        <*> get          -- type
        <*> getWord16le  -- flags
        <*> get          --decimals
        <*  skip 2


instance Binary ColCount where
    -- get = ColCount <$> getLenEncInt
    get = getColCount
    put = undefined --TODO

getColCount :: Get ColCount
getColCount = ColCount <$> getLenEncInt

getTextResponse :: Get TextResponse
getTextResponse = do
    p' <- lookAhead getPacket
    mk p'
    where mk p
            | isErrP  p  = ErrRes <$> (Packed <$> get <*> (getERR $ pLen p))
            | isOKP   p  = OKRes <$> get
            | otherwise  = RSRes <$> getResultSetPackets


getResultSetPackets :: Get ResultSetPackets
getResultSetPackets = do
      colCount@(Packed _ (ColCount c)) <- get
      colDef <- getMany c
      eof1 <- get
      (rows, eof2) <- endBy (Packed <$> get <*> getRowData c) getMaybePackedEOF
      return $ ResultSetPackets colCount colDef eof1 rows eof2

getResultSetHeader :: Get ResultSetHeader
getResultSetHeader = do
      colCount@(Packed _ (ColCount c)) <- get
      colDef <- getMany c
      eof1 <- get
      return $ ResultSetHeader colCount colDef eof1

-- go strict like getMany
endBy :: Get a -> Get (Maybe b) -> Get ([a], b)
endBy i t =  endBy' []
      where
      endBy' ls = do
            h <- lookAheadM t
            case h of
                Just a -> return  $! (reverse ls, a)
                Nothing -> do
                        x <- i
                        x `seq` endBy' (x : ls)
{--
endBy i t = do
        h <- lookAheadM t
        case h of
            Just a -> return ([], a)
            Nothing -> do
                    x <- i
                    ~(xs, a') <- endBy i t
                    return (x : xs, a')
                            --}

-- shamelessly copied from binary
-- | 'getMany n' get 'n' elements in order, without blowing the stack.
getMany :: Binary a => Int -> Get [a]
getMany = go []
 where
    go xs 0 = return $! reverse xs
    go xs i = do x <- get
                 -- we must seq x to avoid stack overflows due to laziness in
                 -- (>>=)
                 x `seq` go (x:xs) (i-1)
{-# INLINE getMany #-}


isErrP :: Packet -> Bool
isErrP p = L.index (pBody p) 0 == (0xff :: Word8)

isOKP :: Packet -> Bool
isOKP p = L.index (pBody p) 0 == (0x00 :: Word8)

isEOFP :: Packet -> Bool
isEOFP p = L.index (pBody p) 0 == (0xfe :: Word8)

mysqlPack :: Word8 -> L.ByteString -> P.Put
mysqlPack seqNo s = do
    let l = L.length s
    let a = l `shiftR` 24
    let b = l `shiftR` 16
    P.putWord8 $ fromIntegral l
    P.putWord8 $ fromIntegral a
    P.putWord8 $ fromIntegral b
    P.putWord8 seqNo
    P.putLazyByteString s

client_long_password, client_long_flag, client_connect_with_db :: Word32
client_long_password   = 0x00000001
--client_found_rows      = 0x00000002
client_long_flag       = 0x00000004
client_connect_with_db = 0x00000008

client_protocol_41, client_transactions, client_secure_connection :: Word32
client_protocol_41       = 0x00000200
client_transactions      = 0x00002000
client_secure_connection = 0x00008000

client_plugin_auth :: Word32
client_plugin_auth       = 0x00080000


defaultClientCap :: Word32
defaultClientCap =  client_long_password
                .|. client_connect_with_db
                .|. client_protocol_41
                .|. client_transactions
                .|. client_secure_connection

defaultClientMaxPacketSize :: Word32
defaultClientMaxPacketSize = 0x00ffffff :: Word32
defaultClientCharset :: Word8
defaultClientCharset = 0x21 :: Word8

-- binary instances

getPacket :: Get Packet
getPacket = do
    (PacketHeader l s) <- getPacketHeader
    body <- getLazyByteString $ fromIntegral l
    return $ Packet l s body

--
instance Binary PacketHeader where
    get = getPacketHeader
    put = putPacketHeader

getPacketHeader :: Get PacketHeader
getPacketHeader = PacketHeader <$> getWord24 <*> get

putPacketHeader :: PacketHeader -> P.Put
putPacketHeader (PacketHeader l s) = putWord24 l >> putWord8 s

--
instance (Binary a) => Binary (Packed a) where
    get = getPacked
    put = putPacked

getPacked :: Binary a => Get (Packed a)
getPacked = Packed <$> get <*> get

putPacked :: Binary a => Packed a -> Put
putPacked (Packed h b) = put h >> put b  -- TODO buggy

getPackedS :: Binary a => Word8 -> Get (Packed a)
getPackedS seqNum = Packed <$> (getPacketHeaderS seqNum) <*> get

getPackedC :: LenConstrained a => Word8 -> Get (Packed a)
getPackedC seqNum = do 
    h <- getPacketHeaderS seqNum
    a <- getLenConstrained (hLen h)
    return $ Packed h a

getPacketHeaderS :: Word8 -> Get PacketHeader
getPacketHeaderS s = do 
        len <- getWord24 
        s'   <- getWord8
        if s /= s'
          then fail ("sequence number mismatch, expecting "
                        ++ show s
                        ++ ", got " ++ show s'
                    )
          else return $ PacketHeader len s 
--
instance Binary LenEncStr where
    put = putLenEncStr
    get = getLenEncStr


putLenEncStr :: LenEncStr -> P.Put

putLenEncStr (LCS c) = do
        let l = B.length c
        P.putWord8 $ fromIntegral l
        P.putByteString c

putLenEncStr Null = P.putWord8 0xfb

getLenEncStr :: Get LenEncStr
getLenEncStr = do
    b <- lookAhead getWord8
    if b == 0xfb
      then
        getWord8 >> return Null
      else do
        len <- getLenEncInt
        str <- getByteString $ fromIntegral len
        return $ LCS str
--
instance Binary OK where
    get = getOK
    put = putOK

getOK :: Get OK
--TODO verify first byte 
getOK = OK <$> (getWord8 *> getLenEncInt)
           <*> getLenEncInt 
           <*> getWord16le
           <*> getWord16le

putOK :: OK -> P.Put
putOK _ = putWord8 0x00

--
getERR :: Int -> Get ERR
getERR len = do
    _  <- getWord8    -- todo verify this
    c  <- getWord16le
    skip 1
    st  <- getLazyByteString 5
    msg <- getLazyByteString $ fromIntegral (len - 9)
    return (ERR c st msg)


--instance Binary ERR where
--    get = undefined
--    put = undefined

class LenConstrained a where
    getLenConstrained :: Int -> Get a

instance LenConstrained ERR where
    getLenConstrained = getERR

instance LenConstrained OK where
    getLenConstrained = const getOK

instance LenConstrained Greeting where
    getLenConstrained = const getGreeting


newtype AuthAck = AuthAck (Either ERR OK)

instance LenConstrained AuthAck where
    getLenConstrained = getAuthAck

getAuthAck :: Int -> Get AuthAck
getAuthAck len = do
        f <- lookAhead getWord8
        if f == 0x00
          then do 
                ok <- getOK
                return $ AuthAck (Right ok) 
          else do
                err <- getERR len 
                return $ AuthAck (Left err)
--
instance Binary EOF where
    put (EOF w s) = do
        putWord8  0xfe
        P.putWord16le w
        P.putWord16le s
    get = getEOF


getEOF :: Get EOF
getEOF = do
    flag <- getWord8
    if flag == 0xfe
      then EOF <$> getWord16le <*> getWord16le
      else fail "not a eof packet"

getMaybePackedEOF :: Get (Maybe (Packed EOF))
getMaybePackedEOF = do
    h <- getPacketHeader
    flag <- getWord8
    if flag == 0xfe
    then do
        w <- getWord16le
        s <- getWord16le
        return $ Just (Packed h (EOF w s))
    else return Nothing

--
putGreeting :: Greeting -> P.Put
putGreeting (Greeting p v t s1 c l st s2) = do
    P.putWord8 p
    P.putByteString v
    P.putWord8 0
    P.putWord32le t
    P.putByteString s1
    P.putWord16le c
    P.putWord8 l
    P.putWord16le st
    replicateM_ 13 $ P.putWord8 0
    P.putByteString s2

getGreeting :: Get Greeting
getGreeting = do
    p  <- getWord8
    v  <- getLazyByteStringNul
    t  <- getWord32le
    s1 <- getLazyByteStringNul
    c  <- getWord16le
    l  <- getWord8
    st <- getWord16le
    skip 13
    s2 <- getLazyByteStringNul
    _ <- getLazyByteStringNul
    return $ Greeting p (L.toStrict v) t (L.toStrict s1) c l st $ L.toStrict s2

--instance Binary Greeting where
--    get = getGreeting
--    put = putGreeting

--

getAuth :: Get Auth
getAuth = do
    a <- getWord32le
    m <- getWord32le
    c <- getWord8
    skip 23
    n <- getLazyByteStringNul --TODO buggy!
    return $ Auth a m c n B.empty B.empty

putAuth :: Auth -> P.Put
putAuth (Auth cap m c n p s) = do
    P.putWord32le cap
    P.putWord32le m
    P.putWord8 c
    replicateM_ 23 $ P.putWord8 0
    P.putLazyByteString n
    P.putWord8 0
    P.putWord8 $ fromIntegral $ B.length p
    P.putByteString p
    P.putByteString s -- TODO buggy
    P.putWord8 0

instance Binary Auth where
    get = getAuth
    put = putAuth

--- constants
-- TODO
data FieldType = DECIMAL     -- 0x00
               | TINY        -- 0x01
               | SHORT       -- 0x02
               | LONG        -- 0x03
               | FLOAT       -- 0x04
               | DOUBLE      -- 0x05
               | NULL        -- 0x06
               | TIMESTAMP   -- 0x07
               | LONGLONG    -- 0x08
               | INT24       -- 0x09
               | DATE        -- 0x0a
               | TIME        -- 0x0b
               | DATETIME    -- 0x0c
               | YEAR        -- 0x0d
               | NEWDATE     -- 0x0e
               | VARCHAR     -- 0x0f
               | BIT         -- 0x10
               | TIMESTAMP2  -- 0x11
               | DATETIME2   -- 0x12
               | TIME2       -- 0x13
               | NEWDECIMAL  -- 0xf6
               | ENUM        -- 0xf7
               | SET         -- 0xf8
               | TINY_BLOB   -- 0xf9
               | MEDIUM_BLOB -- 0xfa
               | LONG_BLOB   -- 0xfb
               | BLOB        -- 0xfc
               | VAR_STRING  -- 0xfd
               | STRING      -- 0xfe
               | GEOMETRY    -- 0xff
               deriving (Show, Enum)
{--
CLIENT_LONG_PASSWORD 0x00000001
CLIENT_FOUND_ROWS 0x00000002
CLIENT_LONG_FLAG 0x00000004
CLIENT_CONNECT_WITH_DB 0x00000008
CLIENT_NO_SCHEMA 0x00000010
CLIENT_COMPRESS 0x00000020
CLIENT_ODBC 0x00000040
CLIENT_LOCAL_FILES 0x00000080
CLIENT_IGNORE_SPACE 0x00000100
CLIENT_PROTOCOL_41 0x00000200
CLIENT_INTERACTIVE 0x00000400
CLIENT_SSL 0x00000800
CLIENT_IGNORE_SIGPIPE 0x00001000
CLIENT_TRANSACTIONS 0x00002000
CLIENT_RESERVED 0x00004000
CLIENT_SECURE_CONNECTION 0x00008000
CLIENT_MULTI_STATEMENTS 0x00010000
CLIENT_MULTI_RESULTS 0x00020000
CLIENT_PS_MULTI_RESULTS 0x00040000
CLIENT_PLUGIN_AUTH 0x00080000
CLIENT_CONNECT_ATTRS 0x00100000
CLIENT_PLUGIN_AUTH_LENENC_CLIENT_DATA 0x00200000
--}


