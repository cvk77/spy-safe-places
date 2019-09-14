module SafeSpaces
  ( convertCoordinates
  , findSafeSpaces
  , adviceForAlex
  , SearchResult(..)
  )
where

import           Text.Read                      ( readMaybe )
import           Control.Monad                  ( mfilter )
import           Data.Maybe                     ( catMaybes )
import           Data.Char                      ( chr )
import           Data.Array                     ( array
                                                , Array
                                                , accum
                                                , assocs
                                                , inRange
                                                )

-- Search result and advice for Alex
data SearchResult = NoSafeSpaces          -- ^ There are no safe spaces in the city
                  | WholeCityIsSafe       -- ^ There are no agents in the city
                  | SafeSpaces [ String ] -- ^ A list of safe spaces
    deriving (Show, Eq)

-- Zero-based representation of the coordinates (e.g. (3,4))
type Coordinates = (Int, Int)

-- The grid
type Grid = Array (Int, Int) Int

-- | This method should convert an one dimensional Array with alphanumeric coordinates (e.g. ["A1"]) to a
-- list of tuples containing a zero-based representation of the coordinates
-- For example the input of ["A6"] should lead to [(0,5)].
convertCoordinates
  :: [String]      -- ^ An alphanumeric representation of coordinates e.g. ["A1", "D1"]
  -> [Coordinates] -- ^ Zero-based representation of the coordinates (e.g. [(0,0),(3,0)]
convertCoordinates = catMaybes . map convertCoordinate
 where
  convertCoordinate (y : x) = (,) <$> convertY y <*> convertX x
  convertCoordinate _       = Nothing
  convertY c | inRange ('A', 'J') c = Just $ (fromEnum c) - 65
             | otherwise            = Nothing
  convertX = fmap (subtract 1) . mfilter (inRange (1, 10)) . readMaybe

-- | This method should take a two-dimensional, zero-based representation of coordinates for the agents locations and
-- find the safest places for Alex in a two-dimensional, zero-based representation of coordinates
findSafeSpaces
  :: [Coordinates] -- ^ a list of coordinates for the agent locations
  -> [Coordinates] -- ^ a list of coordinates for the safest places for alex
findSafeSpaces agents =
  fmap fst $ filter (\(_, v) -> v == safest && v > 0) $ assocs grid
 where
  grid   = foldl gridFor emptyGrid agents
  safest = maximum grid
  emptyGrid =
    array ((0, 0), (9, 9)) [ ((y, x), 99) | y <- [0 .. 9], x <- [0 .. 9] ]

-- | Build a grid for a single agent position
gridFor :: Grid -> Coordinates -> Grid
gridFor baseGrid agentPosition = accum
  (min)
  baseGrid
  [ ((y, x), distance (y, x) agentPosition) | y <- [0 .. 9], x <- [0 .. 9] ]
  where distance (y1, x1) (y2, x2) = abs (x2 - x1) + abs (y2 - y1)

-- | This method should take an array of alphanumeric agent locations and offer advice to Alex for where she
-- should hide out in the city, with special advice for edge cases
adviceForAlex
  :: [String]     -- ^ alphanumeric coordinates (e.g. ["A5", "B1"])
  -> SearchResult -- ^ search result with the proper information for Alex
adviceForAlex agentPositions = case length safeSpaces of
  100 -> WholeCityIsSafe
  0   -> NoSafeSpaces
  _   -> SafeSpaces safeSpaces
 where
  agentPositions' = convertCoordinates agentPositions
  safeSpaces      = fmap showCoordinates $ findSafeSpaces agentPositions'
  showCoordinates (y, x) = chr (y + 65) : (show $ x + 1)
