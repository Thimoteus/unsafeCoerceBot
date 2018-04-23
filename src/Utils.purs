module Utils where 

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as Console
import Control.Monad.IOSync (IOSync)
import Data.Char.Unicode (isAlphaNum, isDigit, isSpace)
import Data.Either (Either, either)
import Data.Foldable (intercalate)
import Data.Maybe (fromMaybe, maybe)
import Data.Moldy (moldMap)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (un)
import Data.String.Yarn (unlines)
import PscIde.Command (ImportResult(..), Message(..), ModuleList(..), PursuitCompletion(..), TypeInfo(..), TypePosition(..))

allAlphaNum :: String -> Boolean
allAlphaNum = allPredicate isAlphaNum

allSpace :: String -> Boolean
allSpace = allPredicate isSpace

allDigit :: String -> Boolean
allDigit = allPredicate isDigit

allPredicate :: (Char -> Boolean) -> (String -> Boolean)
allPredicate p = un Conj <<< moldMap (Conj <<< p)

showTypeInfo :: TypeInfo -> String
showTypeInfo (TypeInfo ti) =
  intercalate "\n"
    [ "Defined at: " <> maybe "" showTypePosition ti.definedAt
    , "Type: " <> ti.type'
    , "Module: " <> ti.module'
    , "Exported from: " <> intercalate ", " ti.exportedFrom
    ]

showModuleList :: ModuleList -> String
showModuleList (ModuleList xs) = show xs

showTypePosition :: TypePosition -> String
showTypePosition (TypePosition {name}) = name

showPursuitInfo :: PursuitCompletion -> String
showPursuitInfo (PursuitCompletion {type', identifier, module', package, text}) = 
  intercalate "\n"
    [ "Type: " <> fromMaybe "" type'
    , "Identifier: " <> identifier
    , "Module: " <> module'
    , "Package: " <> package
    , "Text: " <> text
    ]

showImportResult :: ImportResult -> String
showImportResult = case _ of
  SuccessFile (Message m) -> m
  SuccessText arr -> intercalate "; " arr
  MultipleResults arr -> intercalate "; " $ map showTypeInfo arr

modl :: Either String String -> String
modl x = unlines
  [ "module Main where"
  , "import Botlude"
  , either (append "x = ") (const "") x
  , "main :: Eff (console :: CONSOLE) Unit"
  , "main = " <> either (const "pure unit") (append "logShow $ ") x
  ]

print :: String -> IOSync Unit
print = liftEff <<< Console.log

printErr :: String -> IOSync Unit
printErr = liftEff <<< Console.error