module Game (
    playGameRnd,
    playGameUser
) where

import Data.List
import Random
import String
import System.Random
import Control.Monad.State

move :: [Int] -> [Int]
move row = reverse (take 4 r)
    where
        r = filtered ++ [0, 0, 0, 0]
        filtered = filter (/= 0) (reverse row)

merge :: (Int, Int, Int, Int) -> (Int, Int, Int, Int)
merge (a1, b1, c1, d1) = if z 
        then if x 
            then (0, 0, a1 + b1, c1 + d1)
            else (0, a1, b1, c1 + d1)
        else if y 
            then (0, a1, b1 + c1, d1)
            else if z 
                then (0, a1 + b1, c1, d1)                    
                else (a1, b1, c1, d1)
        where
            x = a1 == b1
            y = b1 == c1
            z = c1 == d1
            
moveRow :: [Int] -> [Int]
moveRow row2 = [a2, b2, c2, d2]
    where
        row = move row2
        a1 = row !! 0
        b1 = row !! 1
        c1 = row !! 2
        d1 = row !! 3
        (a2, b2, c2, d2) = merge (a1, b1, c1, d1)

moveRowT :: [Int] -> [Int]
moveRowT row = reverse $ moveRow $ reverse row
        
moveLeft :: [[Int]] -> [[Int]]
moveLeft arr = map (moveRowT) arr

moveRight :: [[Int]] -> [[Int]]
moveRight arr = map (moveRow) arr

moveDown :: [[Int]] -> [[Int]]
moveDown arr = transpose $ map (moveRow) $ transpose arr

moveUp :: [[Int]] -> [[Int]]
moveUp arr = transpose $ map (moveRowT) $ transpose arr

rndFreeLoc :: [[Int]] -> StdGen -> ((Int, Int), StdGen)
rndFreeLoc arr seed = if cell == 0
    then ((i, j), seed2)
    else rndFreeLoc arr seed2
    where
        (x, seed2) = rndList 2 (0, 3) seed
        i = x !! 0
        j = x !! 1
        cell = (arr !! i) !! j

addRnd :: [[Int]] -> StdGen -> ([[Int]], StdGen)
addRnd arr seed = (before ++ [new] ++ after, seed2)
    where
        ((i, j), seed3) = rndFreeLoc arr seed
        (y, seed2) = rndList 1 (1, 2) seed3
        num = (y!!0) * 2
        (before, selected:after) = splitAt (i) arr
        (rb, _:ra) = splitAt (j) selected
        new = rb ++ [num] ++ ra

nextRndTurn :: [[Int]] -> StdGen -> ([[Int]], StdGen)
nextRndTurn arr seed = if isPossible arr choice
    then nextTurn arr choice seed
    else nextRndTurn arr seed2
    where
        (choice, seed2) = rndInt (0, 3) seed
        
nextTurn :: [[Int]] -> Int -> StdGen -> ([[Int]], StdGen)
nextTurn arr move seed = case move of
    0 -> nextU
    1 -> nextD
    2 -> nextL
    3 -> nextR
    where
        arrU = moveUp arr
        arrL = moveLeft arr
        arrR = moveRight arr
        arrD = moveDown arr
        nextU = addRnd arrU seed
        nextL = addRnd arrL seed
        nextR = addRnd arrR seed
        nextD = addRnd arrD seed

isPossible :: [[Int]] -> Int -> Bool
isPossible arr move = case move of
    0 -> up
    1 -> down
    2 -> left
    3 -> right
    _ -> False
    where
        arrL = moveLeft arr
        arrR = moveRight arr
        arrU = moveUp arr
        arrD = moveDown arr
        left = arrL /= arr
        right = arrR /= arr
        up = arrU /= arr
        down = arrD /= arr

gameFinished :: [[Int]] -> Bool
gameFinished arr = if left && right && up && down
    then True
    else False
    where
        arrL = moveLeft arr
        arrR = moveRight arr
        arrU = moveUp arr
        arrD = moveDown arr
        left = arrL == arr
        right = arrR == arr
        up = arrU == arr
        down = arrD == arr


playGameRnd :: StdGen -> StateT [[Int]] IO ()
playGameRnd seed = do
    arr <- get
    liftIO $ putStr "\ESC[2J"
    liftIO $ putStrLn $ toStringArr arr
    if gameFinished arr
        then do
            liftIO $ putStrLn "Finished!"
            put arr
            return ()
        else do
            x <- liftIO $ getChar
            let (arrn, seed2) = nextRndTurn arr seed
            put arrn
            playGameRnd seed2

playGameUser :: StdGen -> StateT [[Int]] IO ()
playGameUser seed = do
    arr <- get
    liftIO $ putStr "\ESC[2J"
    liftIO $ putStrLn $ toStringArr arr
    if gameFinished arr
        then do
            liftIO $ putStrLn "Finished!"
            put arr
            return ()
        else do
            x <- liftIO $ getChar
            case x of
                'w' -> if isPossible arr 0
                    then do
                        let (arrn, seed2) = nextTurn arr 0 seed
                        put arrn
                        playGameUser seed2
                    else playGameUser seed
                'a' -> if isPossible arr 2
                    then do
                        let (arrn, seed2) = nextTurn arr 2 seed
                        put arrn
                        playGameUser seed2
                    else playGameUser seed
                's' -> if isPossible arr 1
                    then do
                        let (arrn, seed2) = nextTurn arr 1 seed
                        put arrn
                        playGameUser seed2
                    else playGameUser seed
                'd' -> if isPossible arr 3
                    then do
                        let (arrn, seed2) = nextTurn arr 3 seed
                        put arrn
                        playGameUser seed2
                    else playGameUser seed
                _ -> playGameUser seed