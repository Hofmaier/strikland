module Lib where

import qualified Data.Map as M
import Data.List
import Data.Maybe

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Tree = Node String [Tree]

data Rule = Rule [String] String
  deriving Show 

instance Show Tree where
  show tree  = case tree of
    Node str childs -> let strlst = map extractStrFromTree childs
                           joinedStrs = intercalate " - " strlst
                       in  (str ++  "\n") ++ joinedStrs

extractStrFromTree :: Tree -> String
extractStrFromTree tree = case tree of
  Node str childs -> str

showt :: Tree -> String
showt t = "String representation of tree"

tree :: Tree
tree =  Node "W0" [ch, w1]

ch = Node "CH" []
w1 = Node "W1" [w12, w190]
w12 = Node "W12" []
w190 = Node "W190"[]

zdb :: [String]
zdb = ["W0"]

qdb1 :: [String]
qdb1 = ["W1", "CH"]

qdb2 :: [String]
qdb2 = ["CH","W12","W190"]

mappingRules :: Tree -> [String] -> [String] -> [Rule]
mappingRules tree qdb zdb = let
  diff = qdb \\ zdb
  directMappings = directMapping tree qdb diff
  resolvedMappings = exampleRule
  in directMappings ++ resolvedMappings
  
exampleRule = [Rule ["CH", "W1"] "W0"]

directMapping :: Tree -> [String] -> [String] -> [Rule]
directMapping tree qdb codesToResolve = let
  maybes = resolveCodes tree qdb codesToResolve
  in catMaybes maybes

tree2map :: Tree -> M.Map String [String]
tree2map t = M.singleton "W0" ["CH"]

tree2list ::Tree -> [(String,[String])]
tree2list (Node v children) = let
  strs = map tree2string children
  in (v, strs) : concat ( map tree2list children)

tree2string :: Tree -> String
tree2string (Node str children) = str

resolveCodes :: Tree -> [String] -> [String] -> [Maybe Rule]
resolveCodes tree qdb codesToResolve = let 
  resolveTreeFunction = resolveCode tree qdb
  in map resolveTreeFunction codesToResolve

resolveCode :: Tree -> [String] -> String ->  Maybe Rule
resolveCode tree qdb codeToResolve = let
  codeMap = tree2map tree
  children = M.lookup codeToResolve codeMap
  in createRule children codeToResolve

createRule :: Maybe [String] -> String -> Maybe Rule
createRule children target = fmap (str2Rule target) children

str2Rule :: String -> [String] -> Rule
str2Rule target childrenCodes = Rule childrenCodes target 
