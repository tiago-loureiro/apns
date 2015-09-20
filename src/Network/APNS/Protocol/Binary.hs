{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}

module Network.APNS.Protocol.Binary where

import Control.Applicative
import Control.Monad
import Data.Serialize.Get hiding (Result)
import Data.Serialize.Put
import Data.Word
import Network.APNS.Protocol.Types

import qualified Data.ByteString.Base16       as B16
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as BS
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Text.Encoding           as T

parseFeedback :: Get ApnsFeedback
parseFeedback = ApnsFeedback
    <$> getWord32be
    <*> getWord16be
    <*> liftM (Token . T.decodeLatin1 . B16.encode) (getByteString 32)

parseResponse :: Get ApnsResponse
parseResponse = ApnsResponse
    <$> getWord8
    <*> parseErrorResponse
    <*> (NotificationId <$> getWord32be)
  where
    parseErrorResponse = getWord8 >>= \e -> case e of
        0   -> return NoError
        1   -> return ProcessingError
        2   -> return MissingDeviceToken
        3   -> return MissingTopic
        4   -> return MissingPayload
        5   -> return InvalidTokenSize
        6   -> return InvalidTopicSize
        7   -> return InvalidPayloadSize
        8   -> return InvalidToken
        10  -> return Shutdown
        255 -> return Unknown
        n   -> fail $ "unexpected response: " ++ show n -- bad response type

-- PDU format can be checked at http://goo.gl/wtHDnm
buildPDU :: BS.ByteString -> BL.ByteString -> Word32 -> Word32 -> Put
buildPDU token pload expiry nId = do
    -- Command
    putWord8 2
    -- Frame length
    putWord32be (fromIntegral frameLength)
    ---- Frame data
    -- Device token
    putWord8 1
    putWord16be (fromIntegral $ B.length token)
    putByteString token
    -- Payload
    putWord8 2
    putWord16be (fromIntegral $ BL.length pload)
    putLazyByteString pload
    -- Notification ID
    putWord8 3
    putWord16be 4
    putWord32be nId
    -- Expiration
    putWord8 4
    putWord16be 4
    putWord32be expiry
    -- Priority
    putWord8 5
    putWord16be 1
    putWord8 10
  where
    frameLength = item1Len + item2Len + item3Len + item4Len + item5Len
    item1Len = 1 + 2 + (B.length token)
    item2Len = 1 + 2 + (fromIntegral $ BL.length pload)
    item3Len = 1 + 2 + 4
    item4Len = 1 + 2 + 4
    item5Len = 1 + 2 + 1

-- getExpiryTime :: Integer -> IO Word32
-- getExpiryTime delta = fromIntegral . (delta +) . round <$> getPOSIXTime
