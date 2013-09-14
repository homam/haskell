main = do
	contents <- getContents
	--print $ length $ lines contents
	putStr $ shortLinesOnly contents
	putStr "-----bye-----\n"

shortLinesOnly =
	unlines . filter ((<30) . length) . lines

shortLinesOnly2 input =
	unlines $ filter (\line -> length line < 30) $ lines input