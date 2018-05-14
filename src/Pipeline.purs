module Pipeline where

import Prelude

import Control.Monad.Aff (attempt)
import Control.Monad.IO (IO(..))
import Data.Array as A
import Data.Either (isLeft)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (ala, over)
import Data.NonEmpty ((:|))
import Data.String as S
import Data.String.Yarn (unlines)
import Data.Tuple (Tuple(..), lookup)
import Snail (cat, exec, file, (+>))
import Types (Cmd(..), Expression(..), Input, Message(..), ReceivedMessage, Import(..))
import Utils (allSpace)

{-- type Pipe i o c f m = {in :: i -> f c, out :: c -> m o} --}

{-- type AppPipe = Pipe ReceivedMessage Message Input Maybe IO --}

pipeIn :: String -> ReceivedMessage -> Maybe Input
pipeIn myId m@{text: Message text, user} = do
  msg <- unescape <<< S.trim <$> S.stripPrefix (S.Pattern $ "<@" <> myId <> ">") text
  {cmd, expr} <- getCmd msg
  let imports = findImports expr
  pure {cmd, expr, user, imports}

getCmd :: String -> Maybe {cmd :: Cmd, expr :: Expression}
getCmd s
  | Just e <- stripCmd GetType (S.Pattern ":type") s = Just e
  | Just e <- stripCmd GetType (S.Pattern ":typ") s = Just e
  | Just e <- stripCmd GetType (S.Pattern ":ty") s = Just e
  | Just e <- stripCmd GetType (S.Pattern ":t") s = Just e
  | Just e <- stripCmd Eval (S.Pattern ">") s = Just e
  | otherwise = Nothing

stripCmd :: Cmd -> S.Pattern -> String -> Maybe {cmd :: Cmd, expr :: Expression}
stripCmd cmd p s = {expr: _, cmd: _} <<< Expression <$> S.stripPrefix p s <@> cmd

findImports :: Expression -> Array Import
findImports (Expression e) = foldMap hasImport allImports
  where
    hasImport (Tuple i s) = if S.contains (S.Pattern s) e then [i] else []

unescape :: String -> String
unescape = stay
  where
  stay = ala Endo foldMap
    [ S.replaceAll (S.Pattern "&gt;") (S.Replacement ">")
    , S.replaceAll (S.Pattern "&lt;") (S.Replacement "<")
    , S.replaceAll (S.Pattern "&amp;") (S.Replacement "&")
    ]

type ErrStr = String
type SuccStr = String

pipeOut :: Input -> IO Message
pipeOut {imports, cmd, expr, user} = do
  modl imports cmd expr +> file "src/Main.purs"
  res <- over IO attempt $ exec $ "bash" :| ["run.sh"]
  s <- cat (file "output.txt")
  pure $ botify user if isLeft res
    then case cmd of
      GetType -> getTypeOnErr s
      Eval -> evalOnErr s
    else case cmd of
      GetType -> getTypeOnSucc s
      Eval -> evalOnSucc s

getTypeOnSucc :: String -> String
getTypeOnSucc contents =
  let
    lines = S.split (S.Pattern "\n") contents
    dropped = A.dropWhile (_ /= "  The inferred type of x was:") lines
    focus = A.takeWhile (_ /= "  in value declaration x") $ A.drop 2 dropped
    trimmed = map S.trim focus
    last = S.joinWith " " trimmed
  in
    last

getTypeOnErr :: String -> String
getTypeOnErr contents
  | hasHole contents = getHoles contents
  | otherwise =
    let
      lines = S.split (S.Pattern "\n") contents
      dropped = A.dropEnd 6 lines
      taken = A.takeEnd 1 dropped
      trimmed = S.trim <$> taken
      final = S.joinWith "" trimmed
    in
      final

evalOnSucc :: String -> String
evalOnSucc contents =
  let
    lines = S.split (S.Pattern "\n") contents
    last = S.joinWith "" $ A.takeEnd 2 lines
  in
    last

evalOnErr :: String -> String
evalOnErr contents
  | hasHole contents = getHoles contents
  | otherwise =
    let
      lines = S.split (S.Pattern "\n") contents
      trimFront = A.drop 7 lines
      trimBack = A.dropEnd 5 trimFront
      filtered = A.filter (not <<< allSpace) trimBack
      trimmed = S.trim <$> filtered
      final = S.joinWith " " trimmed
    in
      final

hasHole :: String -> Boolean
hasHole = S.contains (S.Pattern "has the inferred type")

getHoles :: String -> String
getHoles contents =
  let
    lines = S.split (S.Pattern "\n") contents
    maybeIdxs = A.mapWithIndex <@> lines $ \ i l ->
      if hasHole l then Just (i + 2) else Nothing
    idxs = A.catMaybes maybeIdxs
    holes = A.index lines <$> idxs
    final = S.joinWith "; " $ S.trim <$> A.catMaybes holes
  in
    final

botify :: String -> String -> Message
botify u s = Message (":robot_face: `<@" <> u <> ">: " <> S.trim s <> "`")

modl :: Array Import -> Cmd -> Expression -> String
modl imports cmd (Expression e) = unlines (topImports <> manualImports <> makeCmd)
  where
    topImports =
      [ "module Main where"
      , "import Botlude"]
    manualImports = makeImport <$> A.catMaybes (lookup <@> allImports <$> imports)
    makeImport i = "import " <> i <> " as " <> i
    mainType = "main :: Eff (console :: CONSOLE) Unit"
    makeCmd = case cmd of
      GetType ->
        [ "x = " <> e
        , mainType
        , "main = pure unit" ]
      Eval ->
        [ mainType
        , "main = logShow $ " <> e]

array = Tuple Array "Data.Array" :: Tuple Import String
list = Tuple List "Data.List" :: Tuple Import String
strMap = Tuple StrMap "Data.StrMap" :: Tuple Import String
mapI = Tuple Map "Data.Map" :: Tuple Import String
set = Tuple Set "Data.Set" :: Tuple Import String
string = Tuple String "Data.String" :: Tuple Import String
semigroupV = Tuple SemigroupV "Data.Validation.Semigroup" :: Tuple Import String
semiringV = Tuple SemiringV "Data.Validation.Semiring" :: Tuple Import String

allImports :: Array (Tuple Import String)
allImports = [array, list, strMap, mapI, set, string, semigroupV, semiringV]
