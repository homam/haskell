{-# LANGUAGE GADTs #-}

type TestId = Int
data Group = A | B | C | D | E | F deriving (Eq, Show)
data AB = AB TestId Group deriving (Show, Eq)


data TestGroup subject where 
	TestGroup :: (Eq subject) => Group -> [subject] -> TestGroup subject

instance Show subject => Show (TestGroup subject) where
	show (TestGroup g subjects) = show g ++ show subjects

type Test subject = [TestGroup subject]



membersOf :: Group -> Test subject -> [subject]
membersOf group [] = [] -- todo: return an error
membersOf group ((TestGroup g subjects):gs)
	| g == group = subjects
	| otherwise = membersOf group gs

testGroups :: Test subject -> [Group]
testGroups test = [g | (TestGroup g _) <- test]

findSubjectsGroup :: subject -> Test subject -> Maybe Group
findSubjectsGroup s [] = Nothing
findSubjectsGroup s ((TestGroup g subjects):gs)
	| s `elem` subjects = Just g
	| otherwise =  findSubjectsGroup s gs

addSubject :: subject -> Test subject -> Test subject
addSubject subject test = test


main :: IO ()
main = do
	let test = [(TestGroup A [1,2,3]), (TestGroup B [4,5,6])]::Test Int
	print test
	print $ testGroups test
	print $ findSubjectsGroup 5 test
	print $ findSubjectsGroup 12 test
	print $ membersOf B test
