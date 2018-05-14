module Utils where 

import Prelude

import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as Console
import Control.Monad.IO (IO)
import Control.Monad.IOSync (IOSync)
import Data.Char.Unicode (isAlphaNum, isDigit, isSpace)
import Data.Foldable (intercalate)
import Data.Maybe (maybe)
import Data.Moldy (moldMap)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (un)
import PscIde.Command (PursuitCompletion(PursuitCompletion))

allAlphaNum :: String -> Boolean
allAlphaNum = allPredicate isAlphaNum

allSpace :: String -> Boolean
allSpace = allPredicate isSpace

allDigit :: String -> Boolean
allDigit = allPredicate isDigit

allPredicate :: (Char -> Boolean) -> (String -> Boolean)
allPredicate p = un Conj <<< moldMap (Conj <<< p)

showPursuitInfo :: PursuitCompletion -> String
showPursuitInfo (PursuitCompletion {type', identifier, module', package, text}) = 
  intercalate "\n"
    [ maybe "" (\t -> "Type: " <> t) type'
    , "Identifier: " <> identifier
    , "Module: " <> module'
    , "Package: " <> package
    , "Text: " <> text
    ]

print :: String -> IOSync Unit
print = liftEff <<< Console.log

printErr :: String -> IOSync Unit
printErr = liftEff <<< Console.error

printIO :: String -> IO Unit
printIO = liftAff <<< log