module Main where

import Control.Concurrent (threadDelay)
import System.Console.ANSI (clearScreen, clearScreenCode)

step :: [(Int, Int)] -> [(Int, Int)]
step field =
    filter
        ( \(a, b) ->
            let n =
                    ( length . filter ((a, b) /=) . filter (`elem` field) . concat $
                        [[(a + x, b + y) | x <- [-1 .. 1]] | y <- [-1 .. 1]]
                    )
             in (n == 3 || (n == 2 && (a, b) `elem` field))
        )
        $ concat [[(x, y) | x <- [minx .. maxx]] | y <- [miny .. maxy]]
  where
    minx = minimum (map fst field) - 1
    miny = minimum (map snd field) - 1
    maxx = maximum (map fst field) + 1
    maxy = maximum (map snd field) + 1

toString :: Int -> Int -> [(Int, Int)] -> String
toString w h field =
    concatMap
        ( flip (++) "\n"
            . concatMap
                ( \p ->
                    if p `elem` field
                        then "◻️ "
                        else "◼️ "
                )
        )
        [[(x, y) | x <- [(- w) .. w]] | y <- reverse [(- h) .. h]]

main :: IO ()
main = do
    let ms_delay = 250
    let size = (12, 12)

    -- PENTADECATHLON
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
        $ iterate step field
