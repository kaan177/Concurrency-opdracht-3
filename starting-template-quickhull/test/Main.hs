{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

module Main where

import Quickhull

import Data.List                                                    hiding ( partition )
import Prelude                                                      as P
import qualified Data.Set                                           as Set

import Data.Array.Accelerate                                        ( Z(..), (:.)(..), toList, fromList )
#if defined(ACCELERATE_LLVM_NATIVE_BACKEND)
import Data.Array.Accelerate.LLVM.Native
#elif defined(ACCELERATE_LLVM_PTX_BACKEND)
import Data.Array.Accelerate.LLVM.PTX
#else
import Data.Array.Accelerate.Interpreter
#endif

import Hedgehog
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Range                                     as Range

import Test.Gen
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.Hedgehog
import GHC.Stack


main :: IO ()
main
  = defaultMain
  $ localOption (NumThreads 1)                        -- run each test sequentially with many cores
  $ localOption (mkTimeout 60000000)                  -- timeout each test after 60 s
  $ localOption (HedgehogTestLimit (Just 1000))       -- number of each test to run
  $ localOption (HedgehogDiscardLimit (Just 10000))   -- maximum number of discard cases before a test fails
  $ testGroup "quickhull"
    [ testProperty "propagateL" prop_propagateL
    , testProperty "propagateR" prop_propagateR
    , testProperty "shiftHeadFlagsL" prop_shiftHeadFlagsL
    , testProperty "shiftHeadFlagsR" prop_shiftHeadFlagsR
    , testProperty "segmentedScanl1" prop_segmentedScanl1
    , testProperty "segmentedScanr1" prop_segmentedScanr1
    , testProperty "initialPartition" prop_initialPartition
    , testProperty "partition" prop_partition
    , testProperty "distinct" prop_distinct
    , testProperty "subset" prop_subset
    , testProperty "trapezoid" prop_trapezoid
    , testProperty "random" prop_random
    , testProperty "line" prop_line
    ]

prop_propagateL :: Property
prop_propagateL = property $ do
  p  <- forAll int
  n  <- forAll $ Gen.int (Range.linear 0 _MAX_SIZE)
  fs <- forAll $ Gen.list (Range.singleton n) Gen.bool
  ps <- forAll $ Gen.list (Range.singleton n) int
  let
      flags  = True : fs
      points = p : ps
      sh     = Z :. n+1
      !go    = runN propagateL
  --
  toList (go (fromList sh flags) (fromList sh points)) === propagateL_ref (zip flags points)

prop_propagateR :: Property
prop_propagateR = property $ do
  p  <- forAll int
  n  <- forAll $ Gen.int (Range.linear 0 _MAX_SIZE)
  fs <- forAll $ Gen.list (Range.singleton n) Gen.bool
  ps <- forAll $ Gen.list (Range.singleton n) int
  let
      flags  = fs ++ [True]
      points = ps ++ [p]
      sh     = Z :. n+1
      !go    = runN propagateR
  --
  toList (go (fromList sh flags) (fromList sh points)) === propagateR_ref (zip flags points)

prop_shiftHeadFlagsL :: Property
prop_shiftHeadFlagsL = property $ do
  sh <- forAll $ (Z :.) <$> Gen.int (Range.linear 1 _MAX_SIZE)
  xs <- forAll $ array sh Gen.bool
  let
      !go       = runN shiftHeadFlagsL
      expected  = tail (toList xs) ++ [True]
  --
  toList (go xs) === expected

prop_shiftHeadFlagsR :: Property
prop_shiftHeadFlagsR = property $ do
  sh <- forAll $ (Z :.) <$> Gen.int (Range.linear 1 _MAX_SIZE)
  xs <- forAll $ array sh Gen.bool
  let
      !go       = runN shiftHeadFlagsR
      expected  = [True] ++ init (toList xs)
  --
  toList (go xs) === expected

prop_segmentedScanl1 :: Property
prop_segmentedScanl1 = property $ do
  n     <- forAll $ Gen.int (Range.linear 0 _MAX_SIZE)
  flags <- forAll $ Gen.list (Range.singleton n) Gen.bool
  x     <- forAll int
  xs    <- forAll $ Gen.list (Range.singleton n) int
  let
      !go    = runN (segmentedScanl1 (+))
      flags' = True : flags
      xs'    = x : xs
  --
  toList (go (fromList (Z :. n+1) flags') (fromList (Z :. n+1) xs')) === segmentedScanl1_ref (+) flags' xs'

prop_segmentedScanr1 :: Property
prop_segmentedScanr1 = property $ do
  n     <- forAll $ Gen.int (Range.linear 0 _MAX_SIZE)
  flags <- forAll $ Gen.list (Range.singleton n) Gen.bool
  x     <- forAll int
  xs    <- forAll $ Gen.list (Range.singleton n) int
  let
      !go    = runN (segmentedScanr1 (+))
      flags' = flags ++ [True]
      xs'    = x : xs
  --
  toList (go (fromList (Z :. n+1) flags') (fromList (Z :. n+1) xs')) === segmentedScanr1_ref (+) flags' xs'

prop_initialPartition :: Property
prop_initialPartition = property $ do
  a  <- forAll point
  b  <- forAll (point `except` \p -> p == a)
  let
      l@(minx,_) = min a b
      r@(maxx,_) = max a b
      point'     = (,) <$> Gen.int (Range.linear minx maxx) <*> int
  --
  ps <- forAll $ Gen.set (Range.linear 0 _MAX_SIZE) (point' `except` \p -> p <= l || p >= r)
  let
      !go     = runN initialPartition
      n       = Set.size ps + 2
      input'  = l : r : Set.toList ps
  input <- forAll $ Gen.shuffle input'
  let
      result  = go (fromList (Z :. n) input)
      flags   = toList (fst result)
      points  = toList (snd result)
      fp      = zip flags points
      (us,vs) = span (not . fst) (tail fp)
      (ws,xs) = span (not . fst) (tail vs)
      r1      = map snd us
      r2      = map snd ws
      -- Left-most point should either be 'l' or 'lAlt'
      lAlt    = maximum [(x, y) | (x, y) <- points, x == fst l]
      rAlt    = minimum [(x, y) | (x, y) <- points, x == fst r]
  --
  length points === length flags
  length (filter id flags) === 3
  head points `oneOf` [l, lAlt]
  last points === head points
  head flags === True
  last flags === True
  head vs `oneOf` [(True, r), (True, rAlt)]
  xs === [(True, head points)]
  all (pointIsLeftOfLine (head points, snd $ head vs)) r1 === True
  all (pointIsLeftOfLine (snd $ head vs, head points)) r2 === True


prop_partition :: Property
prop_partition = property $ do
  a <- forAll point
  b <- forAll (point `except` \p -> p == a)
  let
      p1@(minx,_) = min a b
      p2@(maxx,_) = max a b
      point'     = (,) <$> Gen.int (Range.linear minx maxx) <*> int
  --
  ps <- forAll $ Gen.set (Range.linear 0 _MAX_SIZE) (point' `except` \p -> p <= p1 || p >= p2)
  let
      us      = Set.filter (pointIsLeftOfLine (p1,p2)) ps
      vs      = Set.filter (pointIsLeftOfLine (p2,p1)) ps
  us' <- forAll $ Gen.shuffle $ Set.toList us
  vs' <- forAll $ Gen.shuffle $ Set.toList vs

  let
      nus     = Set.size us
      nvs     = Set.size vs
      n       = nus + nvs + 3
      flags   = True : replicate nus False ++ [True] ++ replicate nvs False ++ [True]
      points  = p1   : us'                 ++ [p2]   ++ vs'                 ++ [p1]
  annotate (show points)
  annotate (show flags)

  let
      !go     = runN partition
      result  = go (fromList (Z :. n) flags, fromList (Z :. n) points)
      flags'  = toList (fst result)
      points' = toList (snd result)
      fp      = zip flags' points'

      check_partition expected p1' p2' actual =
        if Set.size expected == 0
           then do
             let (s12',rest) = span (not . fst) (tail actual)
             --
             s12' === []
             head rest === (True, p2')
             return rest
           else do
             let (s13',rest1) = span (not . fst) (tail actual)
                 (s32',rest2) = span (not . fst) (tail rest1)
                 p3'          = snd $ maximum $ map (\x -> (nonNormalizedDistance (p1',p2') x, x)) $ Set.toList expected
                 p3''         = snd $ foldl1 maxMin $ map (\x -> (nonNormalizedDistance (p1',p2') x, x)) $ Set.toList expected
             --
             fst (head rest1) === True
             snd (head rest1) `oneOf` [p3', p3'']
             head rest2 === (True, p2')
             let p3 = snd $ head rest1
             all (pointIsLeftOfLine (p1', p3) . snd) s13' === True
             all (pointIsLeftOfLine (p3, p2') . snd) s32' === True
             return rest2
  --
  length flags' === length points'
  head points' === p1
  last points' === p1
  head flags' === True
  last flags' === True
  r1 <- check_partition us p1 p2 fp
  r2 <- check_partition vs p2 p1 r1
  r2 === [(True, p1)]

prop_trapezoid :: Property
prop_trapezoid = property $ do
  x1 <- forAll int
  x2 <- forAll (int `except` \x -> x == x1)
  y12 <- forAll int
  yOther <- forAll (int `except` \y -> y == y12)
  let
      minx = min x1 x2
      maxx = max x1 x2
      point' = (,) <$> Gen.int (Range.linear minx maxx) <*> pure yOther
  --
  ps <- forAll $ Gen.set (Range.linear 2 64) point'
  ps' <- forAll $ Gen.shuffle $ Set.toList ps
  let
      !go     = runN quickhull
      n       = length ps' + 2
      input   = (x1, y12) : (x2, y12) : ps'
      result  = go (fromList (Z :. n) input)
  annotate $ show input
  annotate $ show result
  annotate "If this test fails, you probably have a problem in partition"
  length (toList result) === 4

prop_distinct :: Property
prop_distinct = property $ do
  ps <- forAll $ Gen.set (Range.linear 2 _MAX_SIZE) point
  ps' <- forAll $ Gen.shuffle $ Set.toList ps
  let
      input      = fromList (Z :. Set.size ps) ps'
      result     = toList (go input)
      duplicates = nub (result \\ nub result)
      !go        = runN quickhull
  --
  duplicates === []

prop_subset :: Property
prop_subset = property $ do
  ps <- forAll $ Gen.set (Range.linear 2 _MAX_SIZE) point
  ps' <- forAll $ Gen.shuffle $ Set.toList ps
  let
      !go     = runN quickhull
      input   = fromList (Z :. Set.size ps) ps'
      result  = toList (go input)
      wrong   = result \\ toList input
  --
  assert (sort result `isSubsequenceOf` sort (toList input))
  wrong === []

prop_random :: Property
prop_random = property $ do
  ps <- forAll $ Gen.set (Range.linear 2 _MAX_SIZE) point
  ps' <- forAll $ Gen.shuffle $ Set.toList ps
  let
      !go     = runN quickhull
      input   = ps'
      result  = toList . go . fromList (Z :. Set.size ps) $ input
  --
  sort result === sort (quickhull_ref input)

prop_line :: Property
prop_line = property $ do
  a  <- forAll int
  b  <- forAll int
  xs <- forAll $ Gen.set (Range.linear 2 _MAX_SIZE) int
  xs' <- forAll $ Gen.shuffle $ Set.toList xs
  let
      !go     = runN quickhull
      input   = [ (x, a * x + b) | x <- xs' ]
      result  = toList . go . fromList (Z :. Set.size xs) $ input
  --
  sort result === [ minimum input, maximum input ]

oneOf :: (HasCallStack, MonadTest m, Show a, Eq a) => a -> [a] -> m ()
oneOf observed expected@(e : _)
  | observed `elem` expected = success
  | otherwise = withFrozenCallStack $ diff observed (\_ _ -> False) e
oneOf _ _ = error "Expected non-empty list"

-- Returns the tuple whose first element is the largest.
-- If these are equal, it returns the one with the smallest second element.
maxMin :: (Ord a, Ord b) => (a, b) -> (a, b) -> (a, b)
maxMin l@(a1, b1) r@(a2, b2)
  | a1 > a2 = l
  | a1 == a2 && b1 < b2 = l
  | otherwise = r

-- Reference implementation
-- ------------------------

propagateL_ref :: [(Bool,a)] -> [a]
propagateL_ref []            = []
propagateL_ref ((_, y) : ys) = y : map (const y) this ++ propagateL_ref rest
  where
    (this, rest) = span (not . fst) ys

-- NOTE: Do not implement propagateR in this manner! There is a more
-- efficient way to do it, so if you use 'reverse' you will receive no marks.
--
propagateR_ref :: [(Bool,a)] -> [a]
propagateR_ref = reverse . propagateL_ref . reverse

segmentedScanl1_ref :: (a -> a -> a) -> [Bool] -> [a] -> [a]
segmentedScanl1_ref f flags values =
  let
      go []     = []
      go (x:xs) = let (us,vs) = span (not . fst) xs
                      vs'     = go vs
                   in
                   scanl1 f (snd x : map snd us) ++ vs'
  in
  go (zip flags values)

-- NOTE: Do not implement segmentedScanr1 in this manner! There is a more
-- efficient way to do it, so if you use 'reverse' you will receive no marks.
--
segmentedScanr1_ref :: (a -> a -> a) -> [Bool] -> [a] -> [a]
segmentedScanr1_ref f flags values
  = reverse
  $ segmentedScanl1_ref (flip f) (reverse flags) (reverse values)

quickhull_ref :: [Point] -> [Point]
quickhull_ref xs = [left] ++ findhull (left, right) upper ++ [right] ++ findhull (right, left) lower
  where
    left  = minimum xs
    right = maximum xs
    upper = filter (pointIsLeftOfLine (left, right)) xs
    lower = filter (pointIsLeftOfLine (right, left)) xs

findhull :: Line -> [Point] -> [Point]
findhull _          [] = []
findhull l@(p1, p2) xs = findhull (p1, furthest) upper ++ [furthest] ++ findhull (furthest, p2) lower
  where
    furthest = snd $ maximum $ map (\x -> (nonNormalizedDistance l x, x)) xs
    upper    = filter (pointIsLeftOfLine (p1, furthest)) xs
    lower    = filter (pointIsLeftOfLine (furthest, p2)) xs

pointIsLeftOfLine :: Line -> Point -> Bool
pointIsLeftOfLine ((x1, y1), (x2, y2)) (x, y) = nx * x + ny * y > c
  where
    nx = y1 - y2
    ny = x2 - x1
    c  = nx * x1 + ny * y1

nonNormalizedDistance :: Line -> Point -> Int
nonNormalizedDistance ((x1, y1), (x2, y2)) (x, y) = nx * x + ny * y - c
  where
    nx = y1 - y2
    ny = x2 - x1
    c  = nx * x1 + ny * y1

