-- string distance algorithm

data Edit = 	Change Char |
				Copy |
				Delete |
				Insert Char |
				Kill
				deriving (Show, Eq)

--			  original  replacement   changes
transoform :: String -> String -> [Edit]
transoform [] [] = []
transoform os [] = [Kill]
transoform [] rs = map Insert rs
transoform (o:os) (r:rs)
	| o == r 	= Copy : (transoform os rs)
	| otherwise	= best [ Delete : transoform os (r:rs),
						 Insert r : transoform (o:os) rs,
						 Change r : transoform os rs ]

-- find the most cost-effective list of changes
best :: [[Edit]] -> [Edit]
best [x] = x
best (x:xs)
	| cost x <= cost b = x
	| otherwise        = b
	where b = best xs

cost :: [Edit] -> Int
cost = length . filter (/=Copy)


-- transoform "homam" "roham"