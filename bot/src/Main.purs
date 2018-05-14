module Main where
import Botlude
x =  (?map ?trav 5) :: String
main :: Eff (console :: CONSOLE) Unit
main = pure unit