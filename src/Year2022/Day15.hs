module Year2022.Day15 where
import Data.Attoparsec.Text (string, decimal, signed)
import Common (lineParser, solveDay)
import Data.List (sort)

coord = (,) <$> (string "x=" *> signed decimal <* string ", y=") <*> signed decimal

parser = lineParser $ mkSensor <$> (string "Sensor at " *> coord) <*> (": closest beacon is at " *> coord)

type Coord = (Int, Int)
type Interval = (Int, Int)
type Sensor = (Coord, Coord, Int)  -- position, beacon, radius

mkSensor :: Coord -> Coord -> Sensor
mkSensor s@(a, b) bc@(c, d) = (s, bc, abs (a-c) + abs (b-d))

-- find cells with given y that belong to the sensor area
-- (with or without beacon)
row :: Bool -> Int -> Sensor -> Interval
row includeBeacon row ((x,y), (bx, by), radius) =
    let adjust (a, b) | includeBeacon || by /= row = (a, b)
                      | a == bx = (a+1, b)
                      | otherwise = (a, b-1)
        dy = abs (y - row)
        w = radius - dy
    in adjust (x - w, x + w)

notEmpty (a, b) = a <= b
width (a, b) = b - a + 1
hull (a, b) (c, d) = (min a c, max b d)

merge = merge' . sort . filter notEmpty where
    merge' (a@(_,a2):t@(b@(b1,_):r)) | b1 > a2 + 1 = a : merge t
                                     | otherwise =  merge' (hull a b : r)
    merge' last = last

solve1 = sum . map width . merge . map (row False 2000000)

solve2 cs = let (x, y) = head $ concatMap searchRow range in y * boundary + x
    where boundary = 4000000
          range = [boundary,boundary-1..]  -- in my case it was closer to the end, YMMV
          searchRow i = (i,) <$> findBeacon (merge $ map (row True i) cs)
          -- won't work if the beacon is on the edge, but it was not in my case
          findBeacon [(_, b), _] = [b+1]
          findBeacon _ = []

solve = solveDay parser solve1 solve2