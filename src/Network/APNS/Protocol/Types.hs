-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Network.APNS.Protocol.Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.Word
import Data.Text (Text)

import qualified Data.HashMap.Strict as M

newtype Token = Token
  { unToken :: Text } deriving (Eq, Show)
newtype NotificationId = NotificationId
  { unNotify :: Word32 } deriving (Eq, Show)

data Aps = Aps
  -- TODO: apsAlert could be more strongly typed, i.e., string or dictionary
  { apsAlert            :: !Value
  , apsBadge            :: !(Maybe Int)
  , apsSound            :: !(Maybe Text)
  , apsContentAvailable :: !Int
  , apsCategory         :: !(Maybe Text)
  } deriving (Eq, Show)

instance ToJSON Aps where
    toJSON (Aps a b s ca c) = object
      $ "alert"             .= a
      # "badge"             .= b
      # "sound"             .= s
      # "content-available" .= ca
      # "category"          .= c
      # []

data ApnsNotification = ApnsNotification
  { apnsAps   :: !Aps
  , apnsExtra :: !(Maybe Object)
  } deriving (Eq, Show)

instance ToJSON ApnsNotification where
    toJSON (ApnsNotification a e) =
      let (Object o) = object [ "aps" .= a ]
      in maybe (Object o) (\e' -> Object $  o `M.union` e') e

-- | Possible error types returned by apple when sending notifications

data ErrorResponse = NoError
                   | ProcessingError
                   | MissingDeviceToken
                   | MissingTopic
                   | MissingPayload
                   | InvalidTokenSize
                   | InvalidTopicSize
                   | InvalidPayloadSize
                   | InvalidToken
                   | Shutdown
                   | Unknown
                   deriving (Eq, Show)

data ApnsResponse = ApnsResponse
  { arCommand :: Word8
  , arStatus  :: ErrorResponse
  , arIdt     :: NotificationId
  } deriving (Eq, Show)

-- data ApnsResponse = ApnsResponse
--   { arInternal :: ApnsResponseInternal
--   , arToken    :: Token
--   } deriving (Eq, Show)

-- | Feedback that comes from apple services

data ApnsFeedback = ApnsFeedback
  { afTime  :: Word32
  , afLen   :: Word16
  , afToken :: Token
  } deriving (Eq, Show)

-- Misc util
append :: Pair -> [Pair] -> [Pair]
append (_, Null) pp = pp
append p         pp = p:pp
{-# INLINE append #-}

infixr 5 #

(#) :: Pair -> [Pair] -> [Pair]
(#) = append
{-# INLINE (#) #-}
