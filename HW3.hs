--A list of lists with all the suffixes
skips :: [a] -> [[a]]
skips [] = []
skips (x:xs) = [x : xs] ++ skips xs

--A local maxima is an element bigger than both of its neighbours
--If it has no neighbours, then the element cannot be a local maxima
--localMaxima finds all the maximas in the list of integers
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_,_] = []
localMaxima (x:y:z:xs) = (if y > x && y > z then [y] else []) ++ localMaxima (y : z : xs)

--Given a list of size 10, the i-th position representing how many
--times is i in the sequence. plotHistogram will create a histogram
--based on this list the following format: plotHistogram [1,1,1,5] = 
--` *        `
--` *        `
--` *   *    ` 
--`==========`
--`0123456789`
--The second argument will represent the current height
plotHistogram :: [Integer] -> Integer -> String
plotHistogram _ 0 = "==========\n0123456789\n"
plotHistogram xs h = (map (\cnt -> if cnt < h then ' ' else '*') xs) ++ "\n" ++ plotHistogram xs (h - 1)

--Given a list, returns another list where the i-th element is how
--many times i is present in the original list
countList :: [Integer] -> [Integer]
countList xs = map toInteger (map (\d -> length (filter (==d) xs)) [0..9])

--In this case we need to increase the count of digit d for every d
--in the list
histogram :: [Integer] -> String
histogram xs = plotHistogram cnt (maximum cnt)
        where   cnt = countList xs
