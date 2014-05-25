import System.Process
import System.IO

main :: IO ()
main = do
	(_, Just hout, _, _) <- createProcess (proc "ls" ["-la"]){ cwd = Just "/Users/homam", std_out = CreatePipe }
	o <- hGetContents hout
	print $ lines o
	rawSystem "ls" ["-l", "/usr"]
	s <- readProcess "ls" ["-la","."] ""
	print s