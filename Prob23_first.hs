import System.Random

main = do
    gen <- getStdGen
    putStrLn "Enter number of elements to select: "
    --this is not a safe way to do this
    n <- readLn :: IO Int
    putStrLn $ show $ randomSelect gen n
    

l = [1..10000]

randomSelect :: StdGen -> Int -> [Int]
randomSelect gen 0 = []
randomSelect gen n = map (\n -> l !! (n `mod` length l))  (take n $ randoms gen)
