module App where

import Prelude

import Control.Monad.IO (IO)
import Data.Char.Unicode (isSpace)
import Data.Either (Either(Right, Left), either)
import Data.Maybe (Maybe(..), maybe)
import Data.String as S
import Types (CommandP(CommandP), Eval, Expression(Expression), GetType, eval)

parseInput :: String -> Maybe {cmd :: Either (CommandP GetType) (CommandP Eval), inp :: Expression}
parseInput s = case S.takeWhile (not <<< isSpace) s of
  str | S.contains (S.Pattern str) ":type" ->
    let
      cmd = Left CommandP
      inp = Expression (S.trim (S.drop 2 s))
    in
      Just {cmd, inp}
  "&gt;" ->
    let
      cmd = Right CommandP
      inp = Expression (S.trim (S.drop 4 s))
    in
      Just {cmd, inp}
  _ -> Nothing

runInput :: (String -> IO Unit) -> String -> IO Unit
runInput k i = do
  let
    parsed = parseInput i
  maybe (pure unit) runInp parsed
  where
  runInp {cmd, inp} =
    either (eval <@> inp) (eval <@> inp) cmd >>= k