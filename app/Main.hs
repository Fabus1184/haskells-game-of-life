module Main where

import Control.Concurrent (threadDelay)
import Data.List (nub)
import System.Console.ANSI (clearScreen, clearScreenCode)

neighbours :: [(Int, Int)] -> (Int, Int) -> Int
neighbours field (a, b) =
    length . filter ((a, b) /=) . filter (`elem` field) . concat $
        [[(a + x, b + y) | x <- [-1 .. 1]] | y <- [-1 .. 1]]

liveorletdie :: [(Int, Int)] -> [(Int, Int)]
liveorletdie field = filter (flip elem [2, 3] . neighbours field) field

born :: [(Int, Int)] -> [(Int, Int)]
born field =
    nub $
        filter ((==) 3 . neighbours field) . filter (not . flip elem field) $
            concat [[(x, y) | x <- [minx .. maxx]] | y <- [miny .. maxy]]
  where
    minx = minimum (map fst field) - 1
    maxx = maximum (map fst field) + 1
    miny = minimum (map snd field) - 1
    maxy = maximum (map snd field) + 1

next :: [(Int, Int)] -> [(Int, Int)]
next field = nub $ liveorletdie field ++ born field

toString :: Int -> Int -> [(Int, Int)] -> String
toString w h field =
    concatMap
        (flip (++) "\n" . concatMap (\p -> if p `elem` field then "◻️ " else "◼️ "))
        [[(x, y) | x <- [(- w) .. w]] | y <- reverse [(- h) .. h]]

main :: IO ()
main = do
    let ms_delay = 250
    let size = (12, 12)

    -- GLIDER:
    -- let field =
    --         [ (-1, -1)
    --         , (0, -1)
    --         , (1, -1)
    --         , (1, 0)
    --         , (0, 1)
    --         ]

    -- PULSATOR:
    let field =
            [ -- lower row
              (-3, -1)
            , (-2, -1)
            , (-1, -1)
            , (0, -1)
            , (1, -1)
            , (2, -1)
            , (3, -1)
            , (4, -1)
            , -- middle row
              (-3, 0)
            , (-1, 0)
            , (0, 0)
            , (1, 0)
            , (2, 0)
            , (4, 0)
            , -- upper row
              (-3, 1)
            , (-2, 1)
            , (-1, 1)
            , (0, 1)
            , (1, 1)
            , (2, 1)
            , (3, 1)
            , (4, 1)
            ]

    mapM_
        ( \x -> do
            threadDelay $ ms_delay * 1000
            clearScreen
            putStrLn . uncurry toString size $ x
        )
        $ iterate next field
