{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}

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
import Data.Array.Accelerate.Interpreter (run)


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


initialPartition :: Acc (Vector Point) ->  Acc SegmentedPoints
initialPartition points =
  let

      leftMostTest a@(T2 xa ya) b@(T2 xb yb) = cond (xa < xb) a (cond (xa == xb) (cond (ya < yb) a b) b)
      rightMostTest a@(T2 xa ya) b@(T2 xb yb) = cond (xa > xb) a (cond (xa == xb) (cond (ya < yb) a b) b)
      p1, p2 :: Exp Point
      -- locate the left-most point
      p1 = the $ fold leftMostTest (constant (maxBound::Int, 0)) points
      -- locate the right-most point
      p2 = the $ fold rightMostTest (constant (minBound::Int, 0)) points

      isUpper :: Acc (Vector Bool)
      -- determine which points lie above the line (p₁, p₂)
      isUpper = map (pointIsLeftOfLine (T2 p1 p2)) points

      isLower :: Acc (Vector Bool)
      isLower = map (pointIsLeftOfLine (T2 p2 p1)) points

      indexedArray = generate (index1 (length points)) unindex1

      --offsetUpper is net zo lang als de originele array. En alle false values hebben een -1 als index
      offsetUpper :: Acc (Vector Int)
      countUpper  :: Acc (Scalar Int)
      T2 offsetUpper countUpper =
        let
          mapped = map (\b -> if b then constant (1 :: Int) else constant 0) isUpper
          count = fold (+) 0 mapped
          adjustedIndexArray = scanl1 (+) mapped
          adjustedIndexArray' = map (+ (-1)) adjustedIndexArray
          yay = zipWith (\a isUpperCondition -> cond isUpperCondition a (-1)) adjustedIndexArray' isUpper
          --yay = scanl1 (\a b -> cond (a < b) b (-1)) adjustedIndexArray'
        in
          T2 yay count

      aBitOfHelp :: Exp Int -> Exp Bool -> Exp Int
      aBitOfHelp index bool = if bool then index else constant (-1)

      offsetLower :: Acc (Vector Int)
      countLower  :: Acc (Scalar Int)
      T2 offsetLower countLower =
        let
          mapped = map (\b -> if b then constant (1 :: Int) else constant 0) isLower
          count = fold (+) 0 mapped

          -- mapped' = map (\_ -> constant (1 :: Int)) isLower
          -- indexarray = scanl1 (+) mapped'
          -- adjustedIndexArray = zipWith aBitOfHelp indexedArray isLower
          adjustedIndexArray = scanl1 (+) mapped
          adjustedIndexArray' = map (+ (-1)) adjustedIndexArray
          yay = zipWith (\a isLowerCondition -> cond isLowerCondition a (-1)) adjustedIndexArray' isLower
          --yay = tail (scanl (\a b -> cond (a < b) b (-1)) (-1) adjustedIndexArray')

        in
          T2 yay count

      destination :: Acc (Vector (Maybe DIM1))
      destination =
        let
          zipOfsets = zip offsetUpper offsetLower
          mapped = map halp1 zipOfsets
        in mapped

      halp1 :: Exp (Int, Int) -> Exp (Maybe DIM1)
      halp1 (T2 upperNum lowerNum)  =
        if upperNum /= -1
          then lift (Just (Z:. upperNum + 1))
          else
            if lowerNum /= -1
              then lift (Just (Z:. (lowerNum + 2 + unlift (the countUpper))))
              else
                constant Nothing

      totalLength = (3 + the countUpper + the countLower)

      newPoints :: Acc (Vector Point)
      newPoints =
        let
          list = fill (index1 totalLength) undef
          listWithoutPoints = permute const list (destination !) points
        in
          imap (\ix b -> cond
          (unindex1 ix == constant 0 ||
          ix == index1 (totalLength - 1)) p1
          (cond (ix == index1 (1 + the countUpper)) p2 b))
            --(newPoints ! ix == undef)
               listWithoutPoints



      headFlags :: Acc (Vector Bool)
      headFlags =
          generate (index1 totalLength)
          (\ix -> cond (unindex1 ix    == constant 0 ||
                        ix             == index1 (totalLength - 1) ||
                        newPoints ! ix == p2)
                          (constant True) (constant False))

  in
  T2 headFlags newPoints

  --T2 headFlags newPoints


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

    -- distances contains a list of postive distances
    -- if not left of the list, it is -1
    -- not sure if checking for isLeftOfLine is ever useful
    -- could probably be removed. (we'll have to think about it tomorrow)
    distances :: Acc (Vector Int)
    distances =
      let
        zipped = zip3 (propagateL headFlags points) (propagateR headFlags points) points
        isLeftOfLine = map (\(T3 l1 l2 point) -> pointIsLeftOfLine (T2 l1 l2) point) zipped
      in
        zipWith (\(T3 l1 l2 point) b -> cond (b == constant True) (nonNormalizedDistance (T2 l1 l2) point) (constant (-1))) zipped isLeftOfLine

    -- flags of the positions where the distances is highest
    maxFlags =
      let
        -- I propagate the max value to the left and right in each segment

        -- I have 0 fucking clue why this works
        -- If there are any problems later on, it is going to be here probably

        test1 = segmentedScanl1 max headFlags distances
        test2 = segmentedScanr1 max headFlags distances
        zipped = zipWith (curry (\(T2 a b) -> cond (a == b && (a /= constant (0::Int)) && (b /= constant (0::Int))) (constant True) (constant False))) test1 test2
        headFlagsToFalse = zipWith (\flag maxBool -> cond flag (constant False) maxBool) headFlags zipped
        onlyTrueOnMaxValues = segmentedScanl1 (\a b -> cond a (constant False) b) headFlags headFlagsToFalse
        fuckThisShit = segmentedScanl1 (\a b -> cond a a b) headFlags onlyTrueOnMaxValues
        end = zipWith (\a b -> cond (b && a == constant False) a b) (shiftHeadFlagsL fuckThisShit) onlyTrueOnMaxValues
      in
        end

    -- a complete list of all the new flags at their old positions
    -- check if not -1 -1
    newFlags = zipWith (curry (\(T2 a b) -> cond (a || b) (constant True) (constant False))) headFlags maxFlags

    newFlagCount =
      let
        boolToOne = map (\b -> if b then constant (1 :: Int) else constant 0) newFlags
      in
        fold (+) 0 boolToOne

    badPoint :: Exp Point
    badPoint = constant (-1, -1)

    leftLineInEachSegment :: Acc (Vector (Point, Point))
    leftLineInEachSegment =
      let
        test1 = propagateR newFlags points
        test2 = propagateL (shiftHeadFlagsR headFlags) test1
        zipped = zip (propagateL headFlags points) test2
      in
        imap (\ix (T2 a b) -> cond (headFlags ! ix) (T2 badPoint badPoint) (T2 a b)) zipped

    rightLineInEachSegment :: Acc (Vector (Point, Point))
    rightLineInEachSegment =
      let
        test1 = propagateL newFlags points
        test2 = propagateR (shiftHeadFlagsL headFlags) test1
        zipped = zip test2 (propagateR headFlags points)
      in
        imap (\ix (T2 a b) -> cond (headFlags ! ix) (T2 badPoint badPoint) (T2 a b)) zipped

    offsetLower :: Acc (Vector Int)
    segmentedCountLower  :: Acc (Vector Int)
    totalCountLower :: Acc (Scalar Int)
    T3 offsetLower segmentedCountLower totalCountLower =
      let
        isInLowerHalf = zipWith pointIsLeftOfLine leftLineInEachSegment points :: Acc (Vector Bool)
        mapped = map (\b -> if b then constant (1 :: Int) else constant 0) isInLowerHalf :: Acc (Vector Int)
        offsetLower' = map (+ (-1 )) (segmentedScanl1 (+) headFlags mapped)

        count = propagateR (shiftHeadFlagsL headFlags) (map (+ 1) offsetLower')
        count' = imap (\ix num -> cond (headFlags ! ix) (-1) num) count

        totalCount = fold (+) 0 mapped
      in
        T3 offsetLower' count' totalCount

    offsetUpper :: Acc (Vector Int)
    segmentedCountUpper :: Acc (Vector Int)
    totalCountUpper :: Acc (Scalar Int)
    T3 offsetUpper segmentedCountUpper totalCountUpper =
      let
        isInUpperHalf = zipWith pointIsLeftOfLine rightLineInEachSegment points
        mapped = map (\b -> if b then constant (1 :: Int) else constant 0) isInUpperHalf
        offsetUpper' = map (+ (-1 )) (segmentedScanl1 (+) headFlags mapped)

        count = propagateR (shiftHeadFlagsL headFlags) (map (+1) offsetUpper')
        -- sets the count to -1 when the index is a headFlag
        count' = imap (\ix num -> cond (headFlags ! ix) (-1) num) count

        totalCount = fold (+) 0 mapped
      in
        T3 offsetUpper' count' totalCount

    totalSize :: Exp Int
    totalSize =
      let
        flags = the newFlagCount
        lower = the totalCountLower
        upper = the totalCountUpper
      in
        flags + lower + upper


    newHeadFlagIndexes :: Acc (Vector Int)
    newHeadFlagIndexes = 
      let
        doLowerStuff = zipWith (\flag lowerCount -> cond flag 0 lowerCount) newFlags segmentedCountLower
        doUpperStuff = zipWith (\flag upperCount -> cond flag 0 upperCount) newFlags segmentedCountUpper

        test :: Exp DIM1 -> Exp Int -> Exp Int
        test ix a = 
          if newFlags ! ix
            then 
              if maxFlags ! ix && segmentedCountLower ! ix /= (-1)
                then
                  segmentedCountLower ! ix + a
                else
                  if headFlags ! ix && segmentedCountUpper ! ix /= (-1)
                    then
                      if ix == index1 0
                        then a
                      else
                        -- this takes the index at ix-1
                        (segmentedCountUpper ! index1 (unindex1 ix + constant (-1))) + a
                    else
                      a
            else
              a
        mapped = map (\b -> if b then constant (1 :: Int) else constant 0) newFlags
        mapped' = imap test mapped
        flagsSeenAtEachLocation = map (+ (-1)) (scanl1 (+) mapped')
      in
        flagsSeenAtEachLocation
    

    test = generate (index1 totalSize) (\_ -> badPoint)
    test1 = generate (index1 5) (\_ -> constant False)
  in
    atrace "" $
    atraceArray "start" points $
    atrace "" $
    atraceArray "distances" distances $
    atraceArray "max" maxFlags $
    atraceArray "leftLineSegmented" leftLineInEachSegment $
    atraceArray "rightLineSegmented" rightLineInEachSegment $
    atrace "" $
    atraceArray "offsetLower" offsetLower $
    atraceArray "segmentedCountLower" segmentedCountLower $
    atraceArray "totalCountLower" totalCountLower $
    atrace "" $
    atraceArray "offsetUpper" offsetUpper $
    atraceArray "segmentedCountUpper" segmentedCountUpper $
    atraceArray "totalCountUpper" totalCountUpper $
    atrace "" $
    atraceArray "size" (unit totalSize) $
    atraceArray "test" (newHeadFlagIndexes) $
    T2 test1 test


-- TESTING STUFF

testFlags = use (fromList (Z:.8) [True, False, False, False, True, False, False, True])
testList :: Acc (Vector Int)
testList = use (fromList (Z:.8) [0, 1, 1, 0, 0, 1, 0, 0])
leftLineInEachSegment :: Acc (Array DIM1 (Int, Int))
leftLineInEachSegment =
  let
    headFlags = use (fromList (Z:.8) [True, False, True, False, True, False, False, True])
    newFlags = use (fromList (Z:.8) [True, True, False, False, True, False, True, True])
    points = use (fromList (Z:.8) [0, 1, 2, 3, 4, 5, 6, 7])

    test1 = propagateR newFlags points
    test2 = propagateL (shiftHeadFlagsR headFlags) test1
    zipped = zip (propagateL headFlags points) test2
  in
    imap (\ix (T2 a b) -> cond (headFlags ! ix) (T2 (-1) (-1)) (T2 a b)) zipped

rightLineInEachSegment :: Acc (Array DIM1 (Int, Int))
rightLineInEachSegment =
  let
    headFlags = use (fromList (Z:.8) [True, False, False, False, True, False, False, True])
    newFlags = use (fromList (Z:.8) [True, False, False, True, True, True, False, True])
    points = use (fromList (Z:.8) [0, 1, 2, 3, 4, 5, 6, 7])

    test1 = propagateL newFlags points
    test2 = propagateR (shiftHeadFlagsL headFlags) test1
    zipped = zip test2 (propagateR headFlags points)
  in
    imap (\ix (T2 a b) -> cond (headFlags ! ix) (T2 (-1) (-1)) (T2 a b)) zipped

-- The completed algorithm repeatedly partitions the points until there are
-- no undecided points remaining. What remains is the convex hull.
--
quickhull :: Acc (Vector Point) -> Acc (Vector Point)
quickhull points =
  let
    initial = initialPartition points
  in  asnd (whileLoopPartition initial)

whileLoopPartition :: Acc SegmentedPoints -> Acc SegmentedPoints
whileLoopPartition = awhile (fold (&&) (constant True) . afst) partition
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

shiftL :: Acc (Vector Bool) -> Acc (Vector Int) -> Acc (Vector Int)
shiftL flags list =
  generate (index1 (length flags)) $ \ix ->
    let
      i = unindex1 ix
    in
      cond (i == length flags - 1) (constant 0) (list ! index1 (i + 1))

shiftR :: Acc (Vector Bool) -> Acc (Vector Int) -> Acc (Vector Int)
shiftR flags list =
  generate (index1 (length flags)) $ \ix ->
    let
      i = unindex1 ix
    in
      cond (i == length flags - 1) (constant 0) (list ! index1 (i + 1))




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



-- countTrue =
--   let
--     ones = fold (\a b -> cond (a == constant True) (b + 1) a) (constant (0::Int))
--   in
--     ones