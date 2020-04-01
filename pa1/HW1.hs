module HW1 (
    form,
    constGrid,
    flatten,
    access,
    slice,
    vcat,
    hcat,
    without,
    matches2d
) where

-- do not modify the module declaration above!	
-- this will ensure that you cannot load (compile)
-- the module without implementing all of the functions.

-- If you have functions you do not want to implement,
-- leave them as undefined or make them have another
-- default value. If you fully remove any of their definitions,
-- that will be a compilation error during evaluation,
-- and you will be eligible for (yay!) a 5 point deduction
-- (that's bad for your grade). Runtime errors in your code 
-- (rather than compilation errors) are acceptable and will simply
-- result in you getting zero from the specific test case causing
-- an error.

-------------------------
-- Fellowship of the Grid (25, 5, 5, 5 points)
form :: [a] -> (Int, Int) -> [[a]] 
form list (1, _) = [list]
form list (x, y) = take y list: form (drop y list) (x-1, y)

constGrid :: a -> (Int, Int) -> [[a]]
constGrid item (1, times) = [ [item | loop <- [1..times]] ]
constGrid item (x, times) = [ [item | loop <- [1..times]] ] ++ constGrid item (x-1, times)

flatten :: [[a]] -> [a]
flatten list = [item | subList <- list, item <- subList]

access :: [[a]] -> (Int, Int) -> a
access list (x, y) = list !! x !! y
----------------------------
-- The Two Signatures (10, 5, 5, 10 points)
sliceHelper :: [a] -> (Int, Int) -> [a]
sliceHelper list (x, y) = [list !! index | index <- [x..y-1]]

slice :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]
slice biggerList (x, y) (z, t) = [sliceHelper (biggerList !! index) (z, t) | index <- [x..y-1]] 

vcat :: [[a]] -> [[a]] -> [[a]]
vcat firstGrid secondGrid = firstGrid ++ [piece | piece <- secondGrid]

hcat :: [[a]] -> [[a]] -> [[a]]

hcat _ [] = []
hcat [] _ = []
hcat firstGrid secondGrid = [(head firstGrid) ++ (head secondGrid)] ++ hcat (tail firstGrid) (tail secondGrid)

withoutLilHelper :: [a] -> (Int, Int) -> Int -> [a]
withoutLilHelper [] (x, y) number = []
withoutLilHelper list (x, y) number = if (number >= x && number < y) then withoutLilHelper (tail list) (x, y) (number+1) else [head list] ++ withoutLilHelper (tail list) (x, y) (number+1)

withoutBiggerHelper :: [[a]] -> (Int, Int) -> Int -> [[a]]
withoutBiggerHelper [] (x, y) number = []
withoutBiggerHelper list (x, y) number = if (number >= x && number < y) then withoutBiggerHelper (tail list) (x, y) (number+1) else [head list] ++ withoutBiggerHelper (tail list) (x, y) (number+1)

without :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]
without list (x, y) (z, t) = [withoutLilHelper innerList (z, t) 0| innerList <- (withoutBiggerHelper list (x, y) 0)]
----------------------------
-- Return of the Non-trivial (30 points, 15 subject to runtime constraints)

helperHorizontal :: Eq a => [[a]] -> [[a]] -> Int -> Int -> Int -> Int -> [(Int, Int)]

helperHorizontal grid pattern x y gridWidth patternWidth = do 
    
                                                         if ((y + patternWidth) > gridWidth) then []

                                                         else
                                                             if ((map (take patternWidth) grid) == pattern) then [(x,y)] ++ (helperHorizontal (map (drop 1) grid) pattern x (y+1) gridWidth patternWidth)

                                                             else helperHorizontal (map (drop 1) grid) pattern x (y+1) gridWidth patternWidth

helperVertical :: Eq a => [[a]] -> [[a]] -> Int ->Int -> Int -> [(Int, Int)]

helperVertical grid pattern x y gridDepth = let patternDepth = (length pattern)
                                                patternWidth = (length (head pattern))
                                                gridWidth = (length (head grid)) in

                                        if (x + patternDepth > gridDepth) then []
                                        
                                        else (helperHorizontal (take patternDepth grid) pattern x y gridWidth patternWidth) ++ (helperVertical (tail grid) pattern (x+1) y gridDepth)

matches2d :: Eq a => [[a]] -> [[a]] -> [(Int, Int)]
matches2d grid pattern = helperVertical grid pattern 0 0 (length grid)

-- What is undefined? Just a value that will cause an error
-- when evaluated, from the GHC implementation:
-- undefined = error "Prelude.undefined"
-- But it allows your module to be compiled
-- since the function definitions will exist.
