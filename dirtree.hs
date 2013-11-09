module Main (main) where

import Control.Exception
import GHC.IO.Exception
import Control.Monad
import Data.List
import Data.Tree
import System.Directory
import System.Environment


-- Some convenient type synonyms

type Path       = String           -- path
type DentName   = String           -- directory-entry name
type DirNode    = (Path, DentName) -- directory-path/dentname pair
type DirTree    = Tree DentName    -- file-system tree


-- High-level program logic:  get args and print a tree for each

main :: IO ()
main = do
    args <- getArgs
    mapM_ traverseAndPrint (if null args then ["."] else args)

traverseAndPrint :: Path -> IO ()
traverseAndPrint path =
    putStr . showTree =<< fsTraverse root path
  where
    root = if "/" `isPrefixOf` path then "" else "."


-- Effectful tree-builder for file-system hierarchies

fsTraverse :: Path -> DentName -> IO DirTree
fsTraverse = curry (unfoldTreeM fsTraverseStep)

fsTraverseStep :: DirNode -> IO (DentName, [DirNode])
fsTraverseStep (path, node) =
    (,) node `liftM` fsGetChildren (path ++ "/" ++ node)


-- Helper to get traversable directory entries

fsGetChildren :: Path -> IO [DirNode]
fsGetChildren path = do
    contents <- getDirectoryContents path `catch` const (return [])
    let visibles = sort . filter (`notElem` [".", "..", ".git", ".gitignore", ".hdevtools.sock", "node_modules"]) $ contents
    return (map ((,) path) visibles)


-- Purely functional tree-to-string formatting

showTree :: Tree String -> String
showTree t = unlines (showNode "" "" "" t)

showNode :: String -> String -> String -> Tree String -> [String]
showNode leader tie arm node =
    nodeRep : showChildren node (leader ++ extension)
  where
    nodeRep   = leader ++ arm ++ tie ++ rootLabel node
    extension = case arm of ""  -> ""; "`" -> "    "; _   -> "|   "

showChildren :: Tree String -> String -> [String]
showChildren node leader =
    let children = subForest node
        arms     = replicate (length children - 1) "|" ++ ["`"]
    in  concat (zipWith (showNode leader "-- ") arms children)