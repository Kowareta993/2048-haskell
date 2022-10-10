module String (
    toStringArr
)
where

toStringX :: Int -> String              
toStringX x = if x /= 0
    then show x
    else " "

toStringRow :: [Int] -> String
toStringRow row = "|" ++ x1 ++ "|\t|" ++ x2 ++ "|\t|" ++ x3 ++ "|\t|" ++ x4 ++ "|"
    where
        x1 = toStringX (row!!0)
        x2 = toStringX (row!!1)
        x3 = toStringX (row!!2)
        x4 = toStringX (row!!3)

toStringArr :: [[Int]] -> String
toStringArr arr = r1 ++ "\n" ++ r2 ++ "\n" ++ r3 ++ "\n" ++ r4 ++ "\n"
    where
        r1 = toStringRow (arr!!0)
        r2 = toStringRow (arr!!1)
        r3 = toStringRow (arr!!2)
        r4 = toStringRow (arr!!3)





