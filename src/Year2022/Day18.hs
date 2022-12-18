module Year2022.Day18 (solve) where
import Data.Attoparsec.Text (decimal, char)
import qualified Data.MultiSet as MS
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Common (lineParser, solveDay)

type Coord = (Int, Int, Int)
parser = lineParser $ (,,) <$> (decimal <* char ',') <*> (decimal <* char ',') <*> decimal

sides :: Coord -> [(Coord, Coord)]
sides c@(x, y, z) = [
    ((x-1, y, z), c), (c, (x+1, y, z)),
    ((x, y-1, z), c), (c, (x, y+1, z)),
    ((x, y, z-1), c), (c, (x, y, z+1))]

data Axis = X | Y | Z

hull (a, b) (c, d) = (min a c, max b d)

axisHull :: Axis -> [Coord] -> [Coord]
axisHull axis = concatMap (unpack axis) . HM.toList . HM.fromListWith hull . map (pack axis) where
    pack Z (x, y, z) = ((x, y), (z, z))
    pack Y (x, y, z) = ((x, z), (y, y))
    pack X (x, y, z) = ((y, z), (x, x))
    unpack Z ((x, y), (z1, z2)) = (x, y, ) <$> [z1..z2]
    unpack Y ((x, z), (y1, y2)) = (x, , z) <$> [y1..y2]
    unpack X ((y, z), (x1, x2)) = (, y, z) <$> [x1..x2]

solve1 = length . filter ((==1) . snd). MS.toOccurList . MS.fromList . concatMap sides

solve2 :: [Coord] -> Int
solve2 cs = let h = foldl1 HS.intersection $ map (HS.fromList . (`axisHull` cs)) [X, Y, Z]
             in solve1 $ HS.toList $ HS.union h (HS.fromList cs)

solve = solveDay parser solve1 solve2