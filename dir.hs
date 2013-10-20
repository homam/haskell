module Main (main) where

import Control.Monad
import Data.List
import System.Directory
import System.Environment

main = do
    args <- getArgs
    mapM_ tlist (if null args then ["."] else args)

tlist path =
    visit (if "/" `isPrefixOf` path then "" else ".") "" "" "" path

visit :: String -> String -> String -> String -> String -> IO ()
visit path leader tie arm node = do
    putStrLn (leader ++ arm ++ tie ++ node)
    visitChildren (path ++ "/" ++ node) (leader ++ extension)
  where
    extension = case arm of ""  -> ""; "`" -> "    "; _   -> "|   "

visitChildren path leader =
    whenM (doesDirectoryExist path) $ do
        contents <- getDirectoryContents path
            `catch` (\e -> return [show e])
        let visibles = sort . filter (`notElem` [".", "..", ".git", "node_modules"]) $ contents
            arms = replicate (length visibles - 1) "|" ++ ["`"]
        zipWithM_ (visit path leader "-- ") arms visibles

whenM mtest ma = mtest >>= flip when ma