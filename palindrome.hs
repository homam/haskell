-- use:
-- echo "apple\nanna\nboob\nfood" | ./palindrome

main = interact palindromes

palindromes =
	--unlines . map (\t -> (if (snd t) then "P: " else "N: ") ++ (fst t)) . map (\line -> (line, line == reverse line)) . lines
	foldr (\x acc -> (if x == reverse x then "P" else "N")++": "++x++"\n"++acc) [] . lines
