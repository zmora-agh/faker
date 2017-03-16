{-|
Module        : Faker.Utils
Description   : Module with helper functions for all other 'Faker' modules
Copyright     : (c) Alexey Gaziev, 2015
License       : MIT
Maintainer    : alex.gaziev@gmail.com
Stability     : experimental
Portability   : POSIX

Fake data
-}
module Faker.Utils
(
-- * Data types
  Faker(..)

-- * Helper functions for other 'Faker' modules
, toGen
, runFaker
, randomInt
, randomValue
, replaceSymbols
, evalRegex
)
where

import Test.QuickCheck

import Gimlh
import Data.List.Split (splitOn)
import Data.List (intercalate)
import System.IO.Unsafe

import Paths_faker

-- | Stateful type for faker values
newtype Faker a = Faker { unFaker :: SimpleGiml -> Gen a }

instance Functor Faker where
  fmap f (Faker r) = Faker $ \d -> fmap f (r d)

instance Applicative Faker where
  pure a = Faker $ const (pure a)
  Faker f <*> Faker a = Faker $ \d -> f d <*> a d

instance Monad Faker where
  Faker a >>= b = Faker $ \d -> a d >>= ((`unFaker` d) . b)

loadGimlData :: IO SimpleGiml
loadGimlData = do
    filePath <- getDataFileName "data/en.giml"
    contents <- parseFile filePath
    return $ simplifyGiml contents

-- | Function for run 'Faker' functions
runFaker :: Faker a -> IO a
runFaker f = do
  d <- loadGimlData
  generate $ unFaker f d
  
toGen :: Faker a -> Gen a
toGen f = unFaker f (unsafePerformIO loadGimlData)

readFromGiml :: String -> Faker [String]
readFromGiml thing = Faker $ \d -> case fetch d thing of
  Just x -> return $ val2List x
  Nothing -> error "no element and sucky error handling"

randomInt :: (Int, Int) -> Faker Int
randomInt range = Faker $ const (choose range)

-- | Internal function, used in other 'Faker' modules
-- to fetch specific value from data storage by namespace and
-- value type:
--
-- >>> runFaker $ randomValue "name" "first_name"
-- "John"
randomValue :: String -> String -> Faker String
randomValue namespace valType = do
  valList <- readFromGiml (namespace ++ "$" ++ valType)
  (valList !!) <$> randomInt (0, length valList - 1)

-- | Internal function, used in other 'Faker' modules
-- to replace special chars '#' with random numbers
--
-- >>> runFaker $ replaceSymbols "##-##"
-- "12-48"
replaceSymbols :: String -> Faker String
replaceSymbols [] = return ""
replaceSymbols (x:xs) = do
    restOfLine <- replaceSymbols xs
    randInt <- randomInt (0, 9)
    return $ case x of
               '#' -> show (randInt :: Int) ++ restOfLine
               _   -> x : restOfLine

-- | Internal function, used in other 'Faker' modules
-- to eval special regex and turn them into 'Faker String'
--
-- >>> runFaker $ evalRegex "/5[1-5]-#{3,5}/"
-- "555-6384"
--
-- >>> runFaker $ evalRegex "/5[1-5]-#{3,5}/"
-- "5555-177"
evalRegex :: String -> Faker String
evalRegex regex = do
    let preparedRegex = if head regex == '/' && last regex == '/'
                          then init $ tail regex
                          else regex
    replaceExpressions preparedRegex >>= replaceSymbols

replaceExpressions :: String -> Faker String
replaceExpressions [] = return ""
replaceExpressions [a] = return [a]
replaceExpressions (x:y:xs) = case y of
      '{' -> replicateChars x (y:xs) >>= replaceExpressions
      _   -> case x of
               '[' -> randomizeChar (x:y:xs) >>= replaceExpressions
               _   -> do
                        rest <- replaceExpressions (y:xs)
                        return $ x : rest

replicateChars :: Char -> String -> Faker String
replicateChars char rest = do
  let splittedLine = splitOn "}" rest
      range = read $ "(" ++ tail (head splittedLine) ++ ")" :: (Int, Int)
  randInt <- randomInt range
  return $ replicate randInt char ++ intercalate "}" (tail splittedLine)

randomizeChar :: String -> Faker String
randomizeChar rest = do
  let splittedLine = splitOn "]" rest
      rangeNumbers = intercalate "," (splitOn "-" (tail $ head splittedLine))
      range = read $ "(" ++ rangeNumbers ++ ")" :: (Int, Int)
  randInt <- randomInt range
  return $ show randInt ++ intercalate "]" (tail splittedLine)
