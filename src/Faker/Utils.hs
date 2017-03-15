{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
, runFaker
, randomValue
, randomInt
, replaceSymbols
, evalRegex
)
where

import System.Random (newStdGen, StdGen, randomR)
import Gimlh
import Data.List.Split (splitOn)
import Data.List (intercalate)
import Control.Monad.State
import Control.Applicative
import Paths_faker

-- | Fake data storage, contains locale data and stdGen
data FakerData = FakerData {
    localeData :: SimpleGiml,        -- ^ Fake data for locale
    stdGen :: StdGen                 -- ^ Generator for fetching random values from data
  }

-- | Stateful type for faker values
newtype Faker a = Faker (State FakerData a)
  deriving (Functor, Monad, Applicative, MonadState FakerData)

loadGimlData :: IO SimpleGiml
loadGimlData = do
    filePath <- getDataFileName "data/en.giml"
    contents <- parseFile filePath
    return $ simplifyGiml contents

-- | Function for run 'Faker' functions with default 'US' locale
runFaker :: Faker a -> IO a
runFaker (Faker action) = do
  defaultLocaleData <- loadGimlData
  stdGen <- newStdGen
  let fakerData = FakerData { localeData = defaultLocaleData
                            , stdGen = stdGen }

  return $ evalState action fakerData

readFromGiml :: String -> Faker [String]
readFromGiml thing = do
  d <- gets localeData
  case fetch d thing of
    Just x -> return $ val2List x
    Nothing -> error "no element and sucky error handling"

-- | Internal function, used in other 'Faker' modules
-- to fetch specific value from data storage by namespace and
-- value type:
--
-- >>> runFaker $ randomValue "name" "first_name"
-- "John"
randomValue :: String -> String -> Faker String
randomValue namespace valType = do
    valList <- readFromGiml (namespace ++ "$" ++ valType)
    ind <- randomInt (0, length valList - 1)
    return $ valList !! ind

-- | Internal function, used in other 'Faker' modules
-- to get random number inside provided bounds:
--
-- >>> runFaker $ randomInt (1,4)
-- 3
randomInt :: (Int, Int) -> Faker Int
randomInt bounds = do
  state <- get

  let (int, newGen) = randomR bounds (stdGen state)

  put (state { stdGen = newGen })

  return int

-- | Internal function, used in other 'Faker' modules
-- to replace special chars '#' with random numbers
--
-- >>> runFaker $ replaceSymbols "##-##"
-- "12-48"
replaceSymbols :: String -> Faker String
replaceSymbols [] = return ""
replaceSymbols (x:xs) = do
    restOfLine <- replaceSymbols xs
    randInt <- randomInt (0,9)
    return $ case x of
               '#' -> show randInt ++ restOfLine
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
