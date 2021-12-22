--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2021

--EXERCISE A4 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (neighbours) where

import Data.List
import Data.Function

type Point a = (a,a)
type Metric a = (Point a) -> (Point a) -> Double

-- Exercise A4

applyMetric :: Metric a -> Point a -> [Point a] -> [(Point a, Double)]
applyMetric d p [] = []
applyMetric d p xs = (head xs, d p (head xs)) : applyMetric d p (tail xs)

removeDistances :: [(Point a, Double)] -> [Point a]
removeDistances [] = []
removeDistances points_distances = fst (head points_distances) : removeDistances (tail points_distances)

neighbours :: Int -> Metric a -> Point a -> [Point a] -> [Point a]
neighbours k d p xs | k >= 0 = take k $ removeDistances $ sortBy (compare `on` snd) $ applyMetric d p xs
    | otherwise = error "Negative value k"