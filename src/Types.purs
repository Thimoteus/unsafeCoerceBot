module Types where

import Prelude

import Control.Monad.Eff (kind Effect)
import Control.Monad.IO (IO)
import Data.Array as A
import Data.Either (Either(Right, Left), either)
import Data.Newtype (class Newtype, un)
import Data.String as S
import Types.Eval (evalExpr)
import Utils (allSpace)

foreign import kind Command

foreign import data GetType :: Command

foreign import data Eval :: Command

data CommandP (c :: Command) = CommandP

newtype Expression = Expression String

derive instance newtypeExpression :: Newtype Expression _

newtype Message = Message String

derive instance newtypeMessage :: Newtype Message _

newtype ChannelId = ChannelId String

derive instance newtypeChannelId :: Newtype ChannelId _

class Evaluate (c :: Command) i where
  eval :: forall proxy. proxy c -> i -> IO String

instance evalTypeExpr :: Evaluate GetType Expression where
  eval _ = evalExpr (either parseError parseSuccess) <<< Left <<< un Expression
    where
    parseError contents =
      let
        lines = S.split (S.Pattern "\n") contents
        dropped = A.dropEnd 6 lines
        taken = A.takeEnd 1 dropped
        trimmed = S.trim <$> taken
        final = S.joinWith "" trimmed
      in
        final
    parseSuccess contents =
      let
        lines = S.split (S.Pattern "\n") contents
        dropped = A.dropWhile (_ /= "  The inferred type of x was:") lines
        focus = A.takeWhile (_ /= "  in value declaration x") $ A.drop 2 dropped
        trimmed = map S.trim focus
        last = S.joinWith " " trimmed
      in
        last

instance evalEvalExpr :: Evaluate Eval Expression where
  eval _ = evalExpr (either parseError parseSuccess) <<< Right <<< un Expression
    where
    parseError contents =
      let
        lines = S.split (S.Pattern "\n") contents
        trimFront = A.drop 7 lines
        trimBack = A.dropEnd 5 trimFront
        filtered = A.filter (not <<< allSpace) trimBack
        trimmed = S.trim <$> filtered
        final = S.joinWith " " trimmed
      in
        final
    parseSuccess contents =
      let
        lines = S.split (S.Pattern "\n") contents
        last = S.joinWith "" $ A.takeEnd 2 lines
      in
        last

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