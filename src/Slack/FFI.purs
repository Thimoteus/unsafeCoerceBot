module Slack.FFI where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, EffFn5, mkEffFn1, runEffFn1, runEffFn2, runEffFn3, runEffFn5)
import Control.Monad.IOSync (IOSync, runIOSync)
import Types (ChannelId, Error, ListResult, Message, RTMClient, ReceivedMessage, SentMessage, Token, WebClient)

newRTMClient :: Token -> IOSync RTMClient
newRTMClient = liftEff <<< runEffFn1 newRTMClientFFI

startRTM :: RTMClient -> IOSync Unit
startRTM = liftEff <<< runEffFn1 startRTMFFI

newWebClient :: Token -> IOSync WebClient
newWebClient = liftEff <<< runEffFn1 newWebClientFFI

listChannels :: WebClient -> (Error -> IOSync Unit) -> (ListResult -> IOSync Unit) -> IOSync Unit
listChannels web onErr onSucc =
  let
    lc = runEffFn3 listChannelsFFI web
      (mkEffFn1 (runIOSync <<< onErr))
      (mkEffFn1 (runIOSync <<< onSucc))
  in
    liftEff lc

sendMessage :: RTMClient -> Message -> ChannelId -> (Error -> IOSync Unit) -> (SentMessage -> IOSync Unit) -> IOSync Unit
sendMessage rtm msg cid onErr onSucc =
  let
    sm = runEffFn5 sendMessageFFI rtm msg cid
      (mkEffFn1 (runIOSync <<< onErr))
      (mkEffFn1 (runIOSync <<< onSucc))
  in
    liftEff sm

onMessage :: RTMClient -> (ReceivedMessage -> IOSync Unit) -> IOSync Unit
onMessage rtm cb =
  let
    om = runEffFn2 onMessageFFI rtm
      (mkEffFn1 (runIOSync <<< cb))
  in
    liftEff om

foreign import activeUserId :: RTMClient -> String
foreign import onMessageFFI :: forall e. EffFn2 e RTMClient (EffFn1 e ReceivedMessage Unit) Unit
foreign import sendMessageFFI :: forall e. EffFn5 e RTMClient Message ChannelId (EffFn1 e Error Unit) (EffFn1 e SentMessage Unit) Unit
foreign import listChannelsFFI :: forall e. EffFn3 e WebClient (EffFn1 e Error Unit) (EffFn1 e ListResult Unit) Unit
foreign import newWebClientFFI :: forall e. EffFn1 e Token WebClient
foreign import startRTMFFI :: forall e. EffFn1 e RTMClient Unit
foreign import newRTMClientFFI :: forall e. EffFn1 e Token RTMClient