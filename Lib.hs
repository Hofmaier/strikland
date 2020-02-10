module Lib where

import qualified Data.Map as M
import Data.List

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Tree = Node String [Tree]
  deriving Show

data Rule = Rule [String] String
  deriving Show 

--instance Show Tree where
showt :: Tree -> String
showt t = "String representation of tree"

w0 :: Tree
w0 =  Node "W0" [ch, w1]

ch = Node "CH" []
w1 = Node "W1" []

zdb :: [String]
zdb = ["W0"]

qdb :: [String]
qdb = ["W1", "CH"]

mappingRules :: Tree -> [String] -> [String] -> [Rule]
mappingRules tree qdb zdb = let
  directMappings = exampleRule
  resolvedMappings = exampleRule
  in directMappings ++ resolvedMappings
  
exampleRule = [Rule ["CH", "W1"] "W0"]

tree2map :: Tree -> M.Map String [String]
tree2map t = M.singleton "W0" ["CH"]

tree2list ::Tree -> [(String,[String])]
tree2list (Node v children) = let
  strs = map tree2string children
  in (v, strs) : concat ( map tree2list children)

tree2string :: Tree -> String
tree2string (Node str children) = str
