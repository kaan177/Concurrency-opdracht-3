{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE TypeOperators     #-}

module Quickhull (

  Point, Line, SegmentedPoints,
  quickhull,

  -- Exported for display
  initialPartition,
  partition,

  -- Exported just for testing
  propagateL, shiftHeadFlagsL, segmentedScanl1,
  propagateR, shiftHeadFlagsR, segmentedScanr1,

) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Debug.Trace
import qualified Prelude                      as P
import GHC.IO.Handle (BufferMode(LineBuffering))
import Data.Array.Accelerate.Smart (Exp(Exp), undef)
import GHC.Base (VecElem(Int16ElemRep))


-- Points and lines in two-dimensional space
--
type Point = (Int, Int)
type Line  = (Point, Point)

-- This algorithm will use a head-flags array to distinguish the different
-- sections of the hull (the two arrays are always the same length).
--
-- A flag value of 'True' indicates that the corresponding point is
-- definitely on the convex hull. The points after the 'True' flag until
-- the next 'True' flag correspond to the points in the same segment, and
-- where the algorithm has not yet decided whether or not those points are
-- on the convex hull.
--
type SegmentedPoints = (Vector Bool, Vector Point)


-- Core implementation
-- -------------------

-- Initialise the algorithm by first partitioning the array into two
-- segments. Locate the left-most (p₁) and right-most (p₂) points. The
-- segment descriptor then consists of the point p₁, followed by all the
-- points above the line (p₁,p₂), followed by the point p₂, and finally all
-- of the points below the line (p₁,p₂).
--
-- To make the rest of the algorithm a bit easier, the point p₁ is again
-- placed at the end of the array.
--
-- We indicate some intermediate values that you might find beneficial to
-- compute.
--


initialPartition :: Acc (Vector Point) -> Acc SegmentedPoints
initialPartition points =
  let
      p1, p2 :: Exp Point
      -- locate the left-most point
      p1 = the $ fold (\a@(T2 xa _) b@(T2 xb _) -> cond (xa < xb) a b) (constant (maxBound::Int, 0)) points
      -- locate the right-most point
      p2 = the $ fold (\a@(T2 xa _) b@(T2 xb _) -> cond (xa > xb) a b) (constant (minBound::Int, 0)) points

      isUpper :: Acc (Vector Bool)
      -- determine which points lie above the line (p₁, p₂)
      isUpper = map (pointIsLeftOfLine (T2 p1 p2)) points

      isLower :: Acc (Vector Bool)
      isLower = map (pointIsLeftOfLine (T2 p2 p1)) points


      --offsetUpper is net zo lang als de originele array. En alle false values hebben een -1 als index
      offsetUpper :: Acc (Vector Int)
      countUpper  :: Acc (Scalar Int)
      T2 offsetUpper countUpper =
        let
          mapped = map (\b -> if b then constant (1 :: Int) else constant 0) isUpper
          count = fold (+) 0 mapped

          mapped' = map (\_ -> constant (1 :: Int)) isUpper

          -- vlgns mij groeit dit getal exponentieel? 
          -- Heb eronder een alternatief geschreven die misschien wel de correcte index geeft.
          indexarray = scanl1 (+) mapped'
          indexarray' = tail (scanl (\a _ -> a + 1) (constant (-1)) mapped')
          
          adjustedIndexArray = zipWith aBitOfHelp indexarray isUpper
        in
          T2 adjustedIndexArray count

      aBitOfHelp :: Exp Int -> Exp Bool -> Exp Int
      aBitOfHelp index bool = if bool then index else constant (-1)

      offsetLower :: Acc (Vector Int)
      countLower  :: Acc (Scalar Int)
      T2 offsetLower countLower =
        let
          mapped = map (\b -> if b then constant (1 :: Int) else constant 0) isLower
          count = fold (+) 0 mapped

          mapped' = map (\_ -> constant (1 :: Int)) isLower
          indexarray = scanl1 (+) mapped'
          adjustedIndexArray = zipWith aBitOfHelp indexarray isLower
        in
          T2 adjustedIndexArray count

      destination :: Acc (Vector (Maybe DIM1))
      destination = 
        let
          startUpper = constant (1 :: Int)
          startLower = startUpper +  constant (1 :: Int) -- + countUpper 

          zipOfsets = zip offsetUpper offsetLower

          mapped = map halp1 zipOfsets

        in mapped --error "TODO: compute the index in the result array for each point (if it is present)"
        
      
      halp1 :: Exp (Int, Int) -> Exp (Maybe DIM1)
      halp1 (T2 num1 num2)  = 
        if ((num1 /= (constant (-1))))
          then constant (Just (Z:. (num1 + (constant 1))))
          else
            if ((num2 /= -1))
              then constant(Just (Z:. (num1 + 2 + )))
              else
                constant Nothing

      makeDIM1 :: Exp Int -> Exp (Maybe DIM1)
      makeDIM1 num1  = constant (Just (Z:. (num1)))

      newPoints :: Acc (Vector Point)
      newPoints = error "TODO: place each point into its corresponding segment of the result"

      headFlags :: Acc (Vector Bool)
      headFlags = error "TODO: create head flags array demarcating the initial segments"
  in
  T2 headFlags newPoints


-- The core of the algorithm processes all line segments at once in
-- data-parallel. This is similar to the previous partitioning step, except
-- now we are processing many segments at once.
--
-- For each line segment (p₁,p₂) locate the point furthest from that line
-- p₃. This point is on the convex hull. Then determine whether each point
-- p in that segment lies to the left of (p₁,p₃) or the right of (p₂,p₃).
-- These points are undecided.
--
partition :: Acc SegmentedPoints -> Acc SegmentedPoints
partition (T2 headFlags points) =
  let

    -- this creates a list of booleans that are true if the point is left of the segment that it is in.
    distances =
      let
        zipped = zip3 (propagateL headFlags points) (propagateR headFlags points) points
        isLeftOfLine = map (\(T3 l1 l2 point) -> pointIsLeftOfLine (T2 l1 l2) point) zipped
      in
        map (\(T3 l1 l2 point) -> nonNormalizedDistance (T2 l1 l2) point) zipped

    maxFlags =
      let
        -- I propagate the max value to the left and right in each segment
        test1 = segmentedScanl1 max headFlags distances
        test2 = segmentedScanr1 max headFlags distances
      in
        -- Whenever the a and b are the same, the value is the highest value in that segment
        -- not sure if checking for 0 is nessecary here
        -- I'll look into it later on if we have time left
        zipWith (curry (\(T2 a b) -> cond (a == b && (a /= constant (0::Int)) && (b /= constant (0::Int))) (constant True) (constant False))) test1 test2
    
    newFlags = zipWith (curry (\(T2 a b) -> cond (a || b) (constant True) (constant False))) headFlags maxFlags


    canBePartOfNewHull =
      let 
        zipped = zip3 (propagateL newFlags points) (propagateR newFlags points) points
        -- left = propagateR 


      in
        map (\(T3 l1 l2 point) -> pointIsLeftOfLine (T2 l1 l2) point) zipped
    
    -- indexarray' = scanl (\a _ -> a + 1) (constant (0::Int)) headFlags
    

    -- this should be done after things have been removed
    -- offset for each segment
    -- segmentedOffset = propagateR indexarray' newFlags
    offsetUpper :: Acc (Vector Int)
    countUpper  :: Acc (Scalar Int)
    T2 offsetUpper countUpper =
        let
          mapped = map (\b -> if b then constant (1 :: Int) else constant 0) canBePartOfNewHull
          count = fold (+) 0 mapped

          mapped' = map (\_ -> constant (1 :: Int)) canBePartOfNewHull

          -- vlgns mij groeit dit getal exponentieel? 
          -- Heb eronder een alternatief geschreven die misschien wel de correcte index geeft.
          indexarray = scanl1 (+) mapped'
          indexarray' = tail (scanl (\a _ -> a + 1) (constant (-1)) mapped')
          
          adjustedIndexArray = undefined
          --adjustedIndexArray = zipWith aBitOfHelp indexarray isUpper
        in
          T2 adjustedIndexArray count 

    newHeadFlagIndexes :: Acc (Vector Int)
    newHeadFlagIndexes = undefined


    sizeOfNewArray = 
      let 
        newPoints = countTrue canBePartOfNewHull
        flagCount = countTrue newFlags
      in 
        newPoints + flagCount
    

    -- eindigen met een scatter functie
    -- yay = segmentedScanl1 (\(T3 l1 l2 _) (T3 flag b2 point) -> T3 flag b2 c2) headFlags zipped
  in
    error "TODO: partition"


-- The completed algorithm repeatedly partitions the points until there are
-- no undecided points remaining. What remains is the convex hull.
--
quickhull :: Acc (Vector Point) -> Acc (Vector Point)
quickhull =
  error "TODO: quickhull"

-- Helper functions
-- ----------------



propagateL :: Elt a => Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
propagateL = segmentedScanl1 const

propagateR :: Elt a => Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
propagateR = segmentedScanr1 const

-- these functions can be improved by using the permute function
shiftHeadFlagsL :: Acc (Vector Bool) -> Acc (Vector Bool)
shiftHeadFlagsL flags =
  generate (index1 (length flags)) $ \ix ->
    let
      i = unindex1 ix
    in
      cond (i == length flags - 1) (constant True) (flags ! index1 (i + 1))

shiftHeadFlagsR :: Acc (Vector Bool) -> Acc (Vector Bool)
shiftHeadFlagsR flags =
  generate (index1 (length flags)) $ \ix ->
    let
      i = unindex1 ix
    in
      cond (i == 0) (constant True) (flags ! index1 (i - 1))

segmentedScanl1 :: Elt a => (Exp a -> Exp a -> Exp a) -> Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
segmentedScanl1 f headFlags values =
  let
    zipped = zip headFlags values
    scanned = scanl1 (segmented f) zipped
    in
      map snd scanned

segmentedScanr1 :: Elt a => (Exp a -> Exp a -> Exp a) -> Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
segmentedScanr1 f headFlags values =
  let
    zipped = zip headFlags values
    scanned = scanr1 (flip (segmented f)) zipped
    in
      map snd scanned


--Functies die je mag gebruiken (niet compleet)
--map
--Stencil
--Gather
--Scatter
--fold
--scan


-- Given utility functions
-- -----------------------

pointIsLeftOfLine :: Exp Line -> Exp Point -> Exp Bool
pointIsLeftOfLine (T2 (T2 x1 y1) (T2 x2 y2)) (T2 x y) = nx * x + ny * y > c
  where
    nx = y1 - y2
    ny = x2 - x1
    c  = nx * x1 + ny * y1

nonNormalizedDistance :: Exp Line -> Exp Point -> Exp Int
nonNormalizedDistance (T2 (T2 x1 y1) (T2 x2 y2)) (T2 x y) = nx * x + ny * y - c
  where
    nx = y1 - y2
    ny = x2 - x1
    c  = nx * x1 + ny * y1

segmented :: Elt a => (Exp a -> Exp a -> Exp a) -> Exp (Bool, a) -> Exp (Bool, a) -> Exp (Bool, a)
segmented f (T2 aF aV) (T2 bF bV) = T2 (aF || bF) (bF ? (bV, f aV bV))



countTrue = 
  let
    ones = fold (\a b -> cond (a == constant True) (b + 1) a) (constant (0::Int))
  in 
    ones