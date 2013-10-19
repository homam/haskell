type Person = String
type Book = String
type Database = [(Person, Book)]

exampleBase :: Database
exampleBase = [("Alice","Tintin"),("Anna","Little Women"),("Alice","Asterix"),("Rory","Tintin")]

-- Books borrowed by a Person
books :: Database -> Person -> [Book]
books database person = [b | (p, b) <- database, p == person]

-- Persons borrowed a Book
borrowers :: Database -> Book -> [Person]
borrowers database book = [p | (p, b) <- database, b == book]

-- True if the Book is borrowed
-- borrowed :: Database -> Book -> Bool

-- Number of Books a Person borrowed
-- numBorrowed :: Database -> Person -> Int

-- Loan a Book
makeLoan :: Database -> Person -> Book -> Database
makeLoan database person book = [(person, book)] ++ database

-- Return a Loan
returnLoan :: Database -> Person -> Book -> Database
returnLoan database person book = [pair | pair <- database, pair /= (person, book)]

