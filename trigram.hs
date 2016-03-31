import Data.List
import Data.Map (Map, empty, insertWith, keys, findWithDefault)
import System.IO
import System.Random
import System.Environment
toCascade :: [String] -> [[String]]
toCascade a = a:toCascade (tail a)

toCascadex :: Int -> [String] -> [[String]]
toCascadex x a = foldr (zipWith (:)) (repeat []) $ take x $ toCascade a

f :: Map [String] [String] -> [String] -> Map [String] [String]
f m array = insertWith (++) (init array) [last array] m

arrayToMap :: Int -> [String] -> Map [String] [String]
arrayToMap x array = foldl f empty $ toCascadex x array

randomInArray :: StdGen -> [String] -> (StdGen, String)
randomInArray g a = (g2, a !! s)
  where (s, g2) = (randomR (0, (length a)-1) g)

f2 :: Map [String] [String] -> StdGen -> [String] -> (StdGen, String)
f2 m g a = randomInArray g (findWithDefault [""] a m)

generate :: StdGen -> Int -> Map [String] [String] -> [String]
generate g x m = all
  where
    all = initials ++ (snd (mapAccumL (f2 m) g (toCascadex (x-1) all)))
    ks = keys m
    initials = ks !! (fst (randomR (0, (length ks)-1) g))




main = do
  input <- getContents
  g <- getStdGen
  args <- getArgs
  let (arg1:arg2:rest) = args
  let dimension = read arg1 :: Int
  let limit = read arg2 :: Int
  putStrLn $ unwords $ take limit $ generate g dimension $ arrayToMap dimension $ words input
