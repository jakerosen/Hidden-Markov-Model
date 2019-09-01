module HiddenMarkov where

import Data.Word
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Numeric.LinearAlgebra (Vector, Matrix, (!), (#>), tr, (?), (|>))
import qualified Numeric.LinearAlgebra as HMat
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as VStore
import Data.Function ((&))

runHMM
  :: Int
  -> Int
  -> Int
  -> ByteString
  -> ByteString
  -> Map Word8 Int
  -> Vector Double
  -> Matrix Double
  -> Matrix Double
  -> [(Vector Double, Matrix Double, Matrix Double, Double, Double)]
runHMM t n m obs obsSymbols obsIDs π a b = states
-- runHMM t n m obs obsSymbols obsIDs π a b = backward π a b
  -- -> (Vector Double, Matrix Double, Matrix Double)
-- runHMM t n m obs obsSymbols obsIDs π a b =
  -- head $ dropWhile checkProb (drop 100 states)

  where
    nextState
      :: (Vector Double, Matrix Double, Matrix Double, Double, Double)
      -> (Vector Double, Matrix Double, Matrix Double, Double, Double)
    nextState (π, a, b, oldLogP, _) = (π', a', b', oldLogP, logP)
      where
        αPreScale :: Matrix Double
        αPreScale = forward π a b

        α :: Matrix Double
        α = HMat.toColumns αPreScale
          & map (*scale)
          & HMat.fromColumns

        β :: Matrix Double
        β = HMat.toColumns (backward π a b)
          & map (*scale)
          & HMat.fromColumns

        γ :: Matrix Double
        γ = HMat.scale (1 / probO) (α * β)

        diγ :: Matrix Double
        diγ = HMat.fromRows $ zipWith3 f
          [0..]
          (HMat.toRows α)
          (tail (HMat.toRows β))
          where
            f :: Int
              -> Vector Double
              -> Vector Double
              -> Vector Double
            f i α β = HMat.flatten $ HMat.scale (1/probO)
              (a * (α `HMat.outer` (β * (b ! (obsToID i)))))

        (π', a', b') = reestimate γ diγ

        cs :: Vector Double
        cs = sum (HMat.toColumns αPreScale)

        scale :: Vector Double
        scale = VStore.postscanl' (*) (1) cs

        logP :: Double
        logP = negate $ VStore.foldl' (\x y -> x + (log y)) 0 scale

        probO :: Double
        probO = HMat.sumElements (αPreScale ! (t - 1))

    states :: [(Vector Double, Matrix Double, Matrix Double, Double, Double)]
    states = iterate nextState (π, a, b, 0, 0)

    forward
      :: Vector Double     -- π
      -> Matrix Double     -- A
      -> Matrix Double     -- B
      -> Matrix Double     -- α
    forward π a b = HMat.fromRows alphaRows
      where
        alphaRows :: [Vector Double]
        alphaRows = Vector.toList (Vector.constructN t f)
          where
            f :: Vector.Vector (Vector Double) -> Vector Double
            f v = if t == 0
              then π * (b ! (obsToID 0))  -- π * b0 -> α0
              else αIntermediate * (b ! (obsToID t))
                   -- The vector produced from below * b_t is α_t
              where
                t :: Int
                t = Vector.length v

                αPrev :: Vector Double
                αPrev = v Vector.! (t - 1)           -- ex. [ α(0) α(1) ]

                αIntermediate :: Vector Double
                αIntermediate = (tr a) #> αPrev
                -- illustration of what above is doing:
                -- A = [ .7 .3 ]     α_t-1 = [ α(0) ]
                --     [ .4 .6 ]             [ α(1) ]
                -- A' (transpose)
                --   = [ .7 .4 ]
                --     [ .3 .6 ]
                --
                -- [A'] [α_t-1]
                --   = [ .7 .4 ] [ α(0) ] = [ .7α(0) + .4α(1) ]
                --     [ .3 .6 ] [ α(1) ]   [ .3α(0) + .6α(1) ]
                --
                -- The resulting vector is equivalent to the sum over j
                -- for each i in the forward algorithm.  I received no
                -- outside help to work this out.

    backward
      :: Vector Double     -- π
      -> Matrix Double     -- A
      -> Matrix Double     -- B
      -> Matrix Double     -- β
    backward π a b = HMat.fromRows betaRows
      where
        betaRows :: [Vector Double]
        betaRows = reverse $ Vector.toList (Vector.constructN t f)
                -- I need to reverse this because I built it backwards.
          where
            f :: Vector.Vector (Vector Double) -> Vector Double
            f v = if j == 0
              then n |> repeat 1                     -- β_t-1
              else a #> βIntermediate                -- β
              where
                i :: Int
                i = t - j

                j :: Int
                j = Vector.length v

                βnext :: Vector Double
                βnext = v Vector.! (j - 1)

                βIntermediate :: Vector Double
                βIntermediate = βnext * (b ! (obsToID i))

    reestimate
      :: Matrix Double     -- γ
      -> Matrix Double     -- diγ
      -> (Vector Double, Matrix Double, Matrix Double)
    reestimate γ diγ = (π, a, b)
      where
        π :: Vector Double
        π = γ ! 0

        a :: Matrix Double
        a = num / denom
          where
            num :: Matrix Double
            num = HMat.reshape n (sum (HMat.toRows diγ))

            denom :: Matrix Double
            denom = HMat.fromRows $ replicate n (denomNoHead)

        b :: Matrix Double
        b = HMat.fromRows num
          where
            num :: [Vector Double]
            num = map f [0..m - 1]
              where
                f :: Int -> Vector Double
                f i = (sum . map snd) $
                  filter (\(j, _) -> obsToID j == i) iγRows

                iγRows :: [(Int, Vector Double)]
                iγRows = zip [0..] γRows

        γRows :: [Vector Double]
        γRows = HMat.toRows γ

        denomHead :: Vector Double
        denomHead = denomNoHead + (γ ! 0)

        denomNoHead :: Vector Double
        denomNoHead = sum (tail $ γRows)

    calcProb
      :: Matrix Double     -- α
      -> Double
    calcProb α = HMat.sumElements $ α ! (t - 1)

    checkProb
      :: (Vector Double, Matrix Double, Matrix Double, Double, Double)
      -> Bool
    checkProb (_, _, _, oldLogP, logP) = abs (logP - oldLogP) > undefined

    obsToID :: Int -> Int
    obsToID i = obsIDs Map.! (obs `ByteString.index` i)



-- TODO: This function is NOT done.  This version is for testing only.
initState :: Int -> (Vector Double, Matrix Double, Matrix Double)
initState n = (a, b, c)
  where
    a = HMat.fromList [0.6, 0.4]

    b = HMat.fromLists
      [ [0.7, 0.3]
      , [0.4, 0.6] ]

    c = tr $ HMat.fromLists
      [ [0.1, 0.4, 0.5]
      , [0.7, 0.2, 0.1] ]

-- initState :: Int -> (Vector Double, Matrix Double, Matrix Double)
-- initState n = undefined

initStateN2Default :: (Vector Double, Matrix Double, Matrix Double)
initStateN2Default = (π, a, b)
  where
    π :: Vector Double
    π = HMat.fromList [0.51316, 0.48684]

    a :: Matrix Double
    a = HMat.fromLists
      [ [ 0.47468, 0.52532 ]
      , [ 0.51656, 0.48344 ] ]

    b :: Matrix Double
    b = HMat.fromLists
      [ [ 0.03735, 0.03909 ]
      , [ 0.03408, 0.03537 ]
      , [ 0.03455, 0.03537 ]
      , [ 0.03828, 0.03909 ]
      , [ 0.03782, 0.03583 ]
      , [ 0.03922, 0.03630 ]
      , [ 0.03688, 0.04048 ]
      , [ 0.03408, 0.03537 ]
      , [ 0.03875, 0.03816 ]
      , [ 0.04062, 0.03909 ]
      , [ 0.03735, 0.03490 ]
      , [ 0.03968, 0.03723 ]
      , [ 0.03548, 0.03537 ]
      , [ 0.03735, 0.03909 ]
      , [ 0.04062, 0.03397 ]
      , [ 0.03595, 0.03397 ]
      , [ 0.03641, 0.03816 ]
      , [ 0.03408, 0.03676 ]
      , [ 0.04062, 0.04048 ]
      , [ 0.03548, 0.03443 ]
      , [ 0.03922, 0.03537 ]
      , [ 0.04062, 0.03955 ]
      , [ 0.03455, 0.03816 ]
      , [ 0.03595, 0.03723 ]
      , [ 0.03408, 0.03769 ]
      , [ 0.03408, 0.03955 ]
      , [ 0.03688, 0.03397 ] ]
