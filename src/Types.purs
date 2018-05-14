module Types where

import Prelude

import Control.Monad.Eff (kind Effect)
import Data.Newtype (class Newtype)

data Cmd = GetType | Eval

newtype Expression = Expression String

derive instance newtypeExpression :: Newtype Expression _

newtype Message = Message String

derive instance newtypeMessage :: Newtype Message _

newtype ChannelId = ChannelId String

derive instance newtypeChannelId :: Newtype ChannelId _

type Channel =
  { id :: ChannelId
  , name :: String
  , is_channel :: Boolean
  , created :: Int
  , creator :: String
  , is_archived :: Boolean
  , is_general :: Boolean
  , name_normalized :: String
  , is_shared :: Boolean
  , is_org_shared :: Boolean
  , is_member :: Boolean
  , is_private :: Boolean
  , is_mpim :: Boolean
  , members :: Array String
  , topic ::
    { value :: String
    , creator :: String
    , last_set :: Int
    }
  , purpose ::
    { value :: String
    , creator :: String
    , last_set :: Int
    }
  , previous_names :: Array String
  , num_members :: Int
  }

type ListResult =
  { ok :: Boolean
  , channels :: Array Channel
  , acceptedScopes :: Array String
  }

type SentMessage =
  { ok :: Boolean
  , reply_to :: Int
  , ts :: String
  , text :: Message
  }

type ReceivedMessage =
  { type :: String
  , channel :: String
  , user :: String
  , text :: Message
  , ts :: String
  }

type Error =
  { ok :: Boolean
  , error :: String
  }

newtype Token = Token String

derive instance newtypeToken :: Newtype Token _

foreign import data RTMClient :: Type

foreign import data WebClient :: Type

type Input = {user :: String, cmd :: Cmd, expr :: Expression, imports :: Array Import}

data Import = Array | List | StrMap | Map | Set | String | SemigroupV | SemiringV

derive instance eqImport :: Eq Import