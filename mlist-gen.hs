import Data.List
import System.Random hiding (split)
import Data.String.Utils
import System.Environment

parseInput :: String -> (Int, [Int])
parseInput input =
 let ls          =  lines input
     nvars       =  read (head ls) :: Int
     protNames   :: [Int]
     protNames   =  map (\ъ -> read ъ :: Int) . split ";" . head . tail $ ls
 in  (nvars, protNames)

readBaseLines :: IO [[(Int, [String])]]
readBaseLines = do
 bases <- sequence $ map readFile ["./Base_lines/Base_line_" ++ (show i) ++ ".csv" | i <- [1..20]]
 return $ map readBaseLine bases where
  readBaseLine :: String -> [(Int, [String])]
  readBaseLine file = zip protNames taskLists where
   protNames   :: [Int]
   taskLists   :: [[String]]
   protNames   =  map (\ъ ->read ъ :: Int) . tail . split ";" . head . lines $ file
   taskLists   =  map (filter (/= "")) . map (map strip) . transpose . map tail . map (split ";") . tail . lines $ file

randPerm :: StdGen -> [a] -> (StdGen, [a])
randPerm g [] = (g, [])
randPerm g xs = let (n, g') = randomR (0, length xs - 1) g
                    front   = xs !! n
                in  (g', front : (snd $ randPerm g' (take n xs ++ drop (n+1) xs)))

getRandElts :: StdGen -> Int -> [String] -> (StdGen,  [String])
getRandElts g n [] = (g, replicate n "!ERROR!") where
getRandElts g n l =
 let replicateList     :: Int -> [String] -> [String]
     replicateList n l =  take n . concat . repeat $ l
     (g', l')          =  randPerm g l
     l''               =  replicateList n l'
     (g'', l''')       =  randPerm g' l''
 in  (g'', l''')

getRandLists :: StdGen -> Int -> [[String]] -> (StdGen, [[String]])
getRandLists g _ []     = (g  , [])
getRandLists g n (l:ls) = (g'', (l':ls')) where
 (g' , l' ) = getRandElts  g  n l
 (g'', ls') = getRandLists g' n ls

-- из названий прототипов тащим списки задач этих прототипов
getTaskLists :: [Int] -> [[(Int, [String])]] -> [[String]]
getTaskLists protNums bases = zipWith (\i b -> concat . map snd . filter (\p -> fst p == i) $ b) protNums bases

formatOutput :: Int -> [[String]] -> String
formatOutput n tasks = "n\\v;" ++ (join ";" . map show $ [1..n] ) ++ "\n" ++ (unlines . map (\l -> (show . snd $ l) ++ ";" ++ fst l) $ n_tasks) where n_tasks = zip (map (join ";" ) tasks) [1..]

composeVars :: StdGen -> [[(Int, [String])]] -> String -> (StdGen, String)
composeVars g baseLines input = (g', formatOutput n tasks {-unlines . map (join "\t") $ taskLists-}) where
 (n, protNames) = parseInput input
 taskLists      = getTaskLists protNames baseLines
 (g', tasks)    = getRandLists g n taskLists

main :: IO ()
main = do --interact (composeVars baseLines) where
 args <- getArgs
 let fname = if   null args
             then "in.csv"
             else head args
 g <- getStdGen
 baseLines <- readBaseLines
 inputCSV <- readFile fname
 let (g', output) = composeVars g baseLines inputCSV
 putStrLn output
