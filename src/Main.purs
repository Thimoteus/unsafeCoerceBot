module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.IO (INFINITY, launchIO)
import Control.Monad.IOSync (runIOSync)
import Control.Monad.IOSync.Class (liftIOSync)
import Data.Foldable as Array
import Data.Maybe (fromMaybe, maybe, maybe')
import Data.Newtype (un, unwrap)
import Node.Process (chdir, exit, lookupEnv)
import Partial.Unsafe (unsafeCrashWith)
import Pipeline (pipeIn, pipeOut)
import Slack.FFI (activeUserId, listChannels, newRTMClient, newWebClient, onMessage, sendMessage, startRTM)
import Types (Message(..), Token(Token))
import Utils (print, printErr)

main :: Eff (infinity :: INFINITY) Unit
main = runIOSync do
  liftEff $ chdir "bot"
  tokenM <- liftEff $ lookupEnv "SLACK_TOKEN"
  let token = maybe' (\_ -> unsafeCrashWith "No token! Make sure to create an environment variable") Token tokenM
  print $ fromMaybe "No token" tokenM
  rtm <- newRTMClient token
  print "RTM client created"
  startRTM rtm
  print "RTM client started"
  web <- newWebClient token
  print "Web client created"
  listChannels web onErr (onSucc rtm web)

  where

    onErr {error} = printErr error

    onSucc rtm web {channels} = do
      let
        chanM = Array.find _.is_member channels
        chanEff = maybe (liftEff $ exit 1) pure chanM
      chan <- chanEff
      onMessage rtm \ msg -> do
        print $ "Message received: " <> un Message msg.text
        print (activeUserId rtm)
        let
          inp = pipeIn (activeUserId rtm) msg
          outp = liftIOSync <<< post rtm chan.id <=< pipeOut
        maybe (pure unit) (launchIO <<< outp) inp

    post rtm chanId msg =
      sendMessage
        rtm
        msg
        chanId
        (\{error} -> printErr error)
        (\{ok} -> if ok then print ("Sent message: " <> unwrap msg) else printErr "Couldn't send message!")
