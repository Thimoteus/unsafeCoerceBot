module Types.Eval where

import Prelude

import Control.Monad.Aff (attempt)
import Control.Monad.IO (IO(..))
import Data.Either (Either(..), either)
import Data.Newtype (over)
import Data.NonEmpty ((:|))
import Snail (cat, exec, file, (+>))
import Utils (modl)

type ErrStr = String
type SuccStr = String
type Expr = String

evalExpr :: (Either ErrStr SuccStr -> String) -> Either Expr Expr -> IO String
evalExpr k e = do
  modl e +> file "src/Main.purs"
  res <- over IO attempt $ exec $ "bash" :| ["run.sh"]
  s <- cat (file "output.txt")
  pure $ either (\_ -> k (Left s)) (\_ -> k (Right s)) res