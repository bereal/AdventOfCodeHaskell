{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2019.Day12 where

import Common (Input, InputParser (ParsecParser), solveDay)
import Control.DeepSeq (NFData)
import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, sepBy1, signed, string)
import Data.Map ((!?))
import qualified Data.Map as M
import Data.Text (Text)
import Data.Tuple.Select (sel1, sel2, sel3)
import Data.Vector ((!), (//))
import qualified Data.Vector as V
import GHC.Generics (Generic)

type Vector3D = (Int, Int, Int)

data Moon = Moon {position :: Vector3D, velocity :: Vector3D} deriving (Show, Generic, NFData)

num :: Parser Int
num = signed decimal

vector =
  (,,)
    <$> (string "<x=" *> num)
    <*> (string ", y=" *> num)
    <*> (string ", z=" *> num <* char '>')

parser = ParsecParser $ V.fromList <$> sepBy1 (flip Moon (0, 0, 0) <$> vector) endOfLine

(~+~) :: Vector3D -> Vector3D -> Vector3D
(~+~) (x, y, z) (x', y', z') = (x + x', y + y', z + z')

dv x x'
  | x < x' = 1
  | x > x' = -1
  | otherwise = 0

(<->) :: Moon -> Moon -> (Moon, Moon)
(<->) (Moon p@(x, y, z) v) (Moon p'@(x', y', z') v') =
  let dvx = dv x x'
      dvy = dv y y'
      dvz = dv z z'
   in (Moon p (v ~+~ (dvx, dvy, dvz)), Moon p' (v' ~+~ (- dvx, - dvy, - dvz)))

applyVelocity :: Moon -> Moon
applyVelocity (Moon p v) = Moon (p ~+~ v) v

energy :: Vector3D -> Int
energy (x, y, z) = abs x + abs y + abs z

moonEnergy (Moon p v) = energy p * energy v

pairs a b
  | a < b = [(a, c) | c <- [a + 1 .. b]] ++ pairs (a + 1) b
  | otherwise = []

step :: V.Vector Moon -> V.Vector Moon
step moons = applyVelocity <$> foldl updPair moons (pairs 0 $ V.length moons - 1)
  where
    updPair ms (a, b) =
      let (m1, m2) = (ms ! a) <-> (ms ! b) in ms // [(a, m1), (b, m2)]

sel :: (Vector3D -> b) -> V.Vector Moon -> V.Vector (b, b)
sel f = fmap (\(Moon c v) -> (f c, f v))

selectors = map sel [sel1, sel2, sel3]

data LoopFinder f s = Found Int | InProgress (M.Map s Int) (f Moon -> s)

loopLen (Found x) = Just x
loopLen _ = Nothing

updateFinder _ _ f@(Found _) = f
updateFinder step moons (InProgress m f) =
  let state = f moons
   in case m !? state of
        Nothing -> InProgress (M.insert state step m) f
        Just s -> Found (step - s)

updateHistory step moons = map (updateFinder step moons)

emptyHistory = map (InProgress M.empty) selectors

solve2 :: V.Vector Moon -> Int
solve2 moons = find' 1 initialState moons
  where
    initialState = updateHistory 0 moons emptyHistory
    find' s history moons =
      let moons' = step moons
          history' = updateHistory s moons' history
       in case mapM loopLen history' of
            Just r -> foldl1 lcm r
            Nothing -> find' (s + 1) history' moons'

totalEnergy :: [Moon] -> Int
totalEnergy = sum . map moonEnergy

solve1 ms = let ms' = iterate step ms !! 1000 in sum $ fmap moonEnergy ms'

solve = solveDay parser solve1 solve2
