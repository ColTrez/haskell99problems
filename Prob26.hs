import System.Random
import Data.List

main = do
    gen <- getStdGen
    let l = ['a'..'z']
    putStrLn $ show $ shuffle l gen

shuffle :: [a] -> StdGen -> [a]
shuffle as gen = let n = length as
                     rands = take n $ randoms gen :: [Int]
                     pairs = zip as rands
                 in map fst $ sortOn snd pairs
