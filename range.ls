{ min, max } = require 'prelude-ls'

# :: (a, a) -> [a, a]
range = (x1, x2) -> [(min x1, x2), (max x1, x2)]

# :: [a] -> a
lenR = ([x1, x2]) -> x2 - x1

# :: (Range, Range) -> [Range, Range]
sort-by-first = ([x1, x2], [y1, y2]) ->
	| x1 < y1 => [[x1, x2], [y1, y2]]
	| otherwise => [[y1, y2], [x1, x2]]

_intersection-sorted = ([x1, x2], [y1, y2]) ->
	| x2 < y1 => [0, 0]
	| otherwise => [(min y1, x2), (max y1, x2)]

intersection = (rx, ry) -> 
	[rx, ry] = sort-by-first rx, ry
	_intersection-sorted rx, ry

union = ([x1, x2], [y1, y2]) -> [(min x1, x2), (max y1, y2)]

intersection-fraction = (r1, r2) -> (lenR <| intersection r1, r2) / (lenR <| union r1, r2)

exports.range = range
exports.intersection-fraction = intersection-fraction


console.log <| lenR range(2,5)
console.log <| intersection range(2,5), range(3,7)
console.log <| union range(2,5), range(3,7)
console.log <| intersection-fraction range(2,5), range(3,7)

