module Main where

import System.Environment
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import HiddenMarkov
import Numeric.LinearAlgebra (Vector, Matrix)
import Control.Monad (forever)

main :: IO ()
main = do
  args <- getArgs
  -- TODO: need to filter out unwanted characters
  -- TODO: and lowercase
  -- obs :: ByteString <- ByteString.concat <$> traverse ByteString.readFile args
  let
    -- obsSymbolsT :: Text
    -- obsSymbolsT = "abcdefghijklmnopqrstuvwxyz "

    -- obsSymbols :: ByteString
    -- obsSymbols = Text.encodeUtf8 obsSymbolsT

    -- obsIDs :: Map Word8 Int
    -- obsIDs = Map.fromList (zip (ByteString.unpack obsSymbols) [0..])
    obs :: ByteString
    obs = Text.encodeUtf8 "SMSL"

    obsSymbolsT :: Text
    obsSymbolsT = "SML"

    obsSymbols :: ByteString
    obsSymbols = Text.encodeUtf8 obsSymbolsT

    obsIDs :: Map Word8 Int
    obsIDs = Map.fromList (zip (ByteString.unpack obsSymbols) [0..])

    -- t :: Int
    -- t = ByteString.length obs

    -- n :: Int
    -- n = 2

    -- m :: Int
    -- m = 27
    t :: Int
    t = 4

    n :: Int
    n = 2

    m :: Int
    m = 3

    -- (π, a, b) = HiddenMarkov.initStateN2Default
    (π, a, b) = HiddenMarkov.initState 2

    -- α :: Matrix Double
    -- α = HiddenMarkov.runHMM t n m obs obsSymbols obsIDs π a b

    -- (π', a', b') = HiddenMarkov.runHMM t n m obs obsSymbols obsIDs π a b


  loop (HiddenMarkov.runHMM t n m obs obsSymbols obsIDs π a b)

loop :: [(Vector Double, Matrix Double, Matrix Double, Double, Double)] -> IO ()
loop [] = pure ()
loop (t:ts) = forever $ do
  let
    (π, a, b, oldLogP, logP) = t
  print π
  print a
  print b
  -- print t
  -- print (uncurry runHMM t)
  _ <- getLine
  loop ts
