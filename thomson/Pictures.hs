--------------------------------------------------------------------
-- 
--   Pictures.hs
-- 
--   Simon Thompson
-- 
--   June 1998
--   Last modified 28 September 2000
-- 
-- An implementation of a type of rectangular pictures using lists of 
-- lists of characters.
-- 
--------------------------------------------------------------------

-- The basics
-- ^^^^^^^^^^

module Pictures where

type Picture = [[Char]]

-- The example used in Craft2e: a polygon which looks like a horse. Here
-- taken to be a 16 by 12 rectangle.

horse :: Picture

horse = [".......##...",
         ".....##..#..",
         "...##.....#.",
         "..#.......#.",
         "..#...#...#.",
         "..#...###.#.",
         ".#....#..##.",
         "..#...#.....",
         "...#...#....",
         "....#..#....",
         ".....#.#....",
         "......##...."]

-- Make a white Picture of the equal size of the input
makeWhite :: Picture -> Picture

makeWhite = map (map (\c -> '.')) 

-- A completely white picture.

white :: Picture

white = makeWhite horse 

black :: Picture

black = invertColour white


--

-- True if the given Picture is empty
isEmpty :: Picture -> Bool

isEmpty p = 0 == sum (map (length) p)


--checkers = foldl (\acc a -> if isEmpty(acc) then a else sideBySide acc a) [[]] $ take 4 $ repeat $ sideBySide white (invertColour  white)

checkerizeDirection :: (Picture -> Picture -> Picture) -> Int -> Picture -> Picture
checkerizeDirection direction number pic = 
    (foldl (\acc a -> if isEmpty(acc) then a else direction acc a) [[]] . (take number . repeat)) $ direction pic (invertColour  pic)

checkerize :: Int -> Picture -> Picture
checkerize number pic = (checkerizeDirection above number) . (checkerizeDirection sideBySide number) $ pic

checkers = checkerize 4 horse --checkerize above 4 (checkerize sideBySide 4 horse)

-- rotate 90

--rot90 :: Picture -> Picture
--rot90 pic = 

-- [length of each column]
lengths :: Picture -> [Int]
lengths pic = [length (pic!!row) | row <- [0..(length pic)-1]]


mat = [(i, [j | (cell, j) <- zip row [0..]]) | (row, i) <- (zip horse [0..])]
rotatedHorse = [[horse!!fromIntegral(j)!!fromIntegral(11-i) | j <- js] | (i,js) <- mat]

-- Small black and white pictures.

sb, sw :: Picture

sb = ["##","##"]
sw = ["..",".."]

-- Getting a picture onto the screen.

printPicture :: Picture -> IO ()

printPicture = putStr . concat . map (++"\n")


-- Transformations of pictures.
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Reflection in a vertical mirror.

flipV :: Picture -> Picture

flipV = map reverse

-- Reflection in a horizontal mirror.

flipH :: Picture -> Picture

flipH = reverse

-- Rotation through 180 degrees, by composing vertical and horizontal
-- reflection. Note that it can also be done by flipV.flipH, and that we
-- can prove equality of the two functions.

rotate :: Picture -> Picture

rotate = flipH . flipV

-- One picture above another. To maintain the rectangular property,
-- the pictures need to have the same width.

above :: Picture -> Picture -> Picture

above = (++)

-- One picture next to another. To maintain the rectangular property,
-- the pictures need to have the same height.

sideBySide :: Picture -> Picture -> Picture

sideBySide = zipWith (++)

-- Superimose one picture above another. Assume the pictures to be the same
-- size. The individual characters are combined using the combine function.

superimpose :: Picture -> Picture -> Picture

superimpose = zipWith (zipWith combine)

-- For the result to be '.' both components have to the '.'; otherwise
-- get the '#' character.

combine :: Char -> Char -> Char

combine topCh bottomCh
  = if (topCh == '.' && bottomCh == '.') 
    then '.'
    else '#'

-- Inverting the colours in a picture; done pointwise by invert...

invertColour :: Picture -> Picture

invertColour = map (map invert)

-- ... which works by making the result '.' unless the input is '.'.

invert :: Char -> Char

invert ch = if ch == '.' then '#' else '.'


