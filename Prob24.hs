import System.Random

main = do
    gen <- getStdGen
    putStrLn "Enter M - number of elements in list"
    m <- readLn :: IO Int
    putStrLn "Enter N - number to select"
    n <- readLn :: IO Int
    let l = [1..m]
    let selected = lottoSelect n l gen
    putStrLn "The selected elements are:"
    putStrLn $ show selected

lottoSelect :: Int -> [a] -> StdGen -> [a]
lottoSelect 0 _ _ = []
lottoSelect n as gen = let (randomNum, newGen) = random gen
                           index = (randomNum `mod` length as + 1)
                           start = take index as
                       in (last start) :
                         (lottoSelect (n-1) ((init start) ++ (drop index as)) newGen)
                        -- In the line below, doing take (index-1) caused issues when
                        -- index was 0 or less. Doing take index would have caused a similar
                        --problem because take 0 returns the empty list
                        --in (as !! index) :
                        --     (lottoSelect (n-1) ((take (index-1) as)++(drop index as)) newGen)
