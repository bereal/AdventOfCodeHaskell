{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Year2022.Day16 where
import Data.Char (ord)
import Data.Attoparsec.Text (string, many1, letter, decimal, sepBy1, Parser)
import qualified Data.IntMap as IM
import Common (lineParser, skipPart, solveDay)
import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as M
import Data.Bits (shift, Bits ((.&.), (.|.)))
import Data.HashMap.Strict ((!))
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Hashable (Hashable(hashWithSalt))
import Data.List.Extra (maximumOn)
import Data.List (sortOn)


name = many1 letter

nameList = name `sepBy1` string ", "

valve :: Parser (Node String)
valve = Node <$> (string "Valve " *> name)
    <*> (string " has flow rate=" *> decimal)
    <*> ((string "; tunnels lead to valves " <|> string "; tunnel leads to valve ") *> nameList)

parser = lineParser valve

data Node a = Node { nid :: a, flow :: Int, links :: [a] } deriving (Show, NFData, Generic)

rename :: (a -> b) -> Node a -> Node b
rename f n@Node{..} = n { nid=f nid, links=f <$> links }

bits = iterate (`shift` 1) 1

mapNodes :: [Node String] -> [Node Int]
mapNodes vs = let m = M.fromList $ (nid <$> vs) `zip` bits
                in map (rename (m!)) vs

data DPState = DPState { open :: Int, me :: Int, elephant :: Int, timeLeft :: Int } deriving (Eq, Show)

instance Hashable DPState where
    hashWithSalt s d = hashWithSalt s (open d, me d, elephant d, timeLeft d)

type DPMemo = M.HashMap DPState Int
type Map = M.HashMap Int (Node Int)

subs :: Map -> DPState -> [(Int, DPState)]
subs m st@DPState{..} = let
        Node{..} = m ! me
        moveSub = map (\l -> (0, st {timeLeft=timeLeft-1, me=l})) links
        openSub = [(flow, st { timeLeft=timeLeft-1, open=open .|. me }) | (open .&. me) == 0]
    in openSub ++ moveSub

runDP :: Map -> DPMemo -> DPState -> (DPMemo, Int)
runDP _ mem st@DPState{timeLeft=0} = (mem, 0)
runDP m mem st@DPState{..} | open == ((1 `shift` M.size m) - 1) = (mem, 0)
                           | otherwise = case M.lookup st mem of
                                Just r -> (mem, r)
                                _ -> runDP' m mem st

runDP' m mem st@DPState{..} =
    let runSub (curMem, best) (fl, st'@DPState{..}) =
            let (mem', res) = runDP m curMem st'
            in (mem', max best $ fl * timeLeft + res)
        (mem', best) = foldl runSub (mem, 0) $ subs m st
    in (M.insert st best mem', best)

initMask :: [Node Int] -> Int
initMask = foldl1 (.|.) . map (\Node{..} -> if flow == 0 then nid else 1)

solve1 nodes = let ns = mapNodes $ sortOn nid nodes
                   m = M.fromList $ [(nid n, n) | n <- ns]
                   mask = initMask ns
                in snd $ runDP m M.empty $ DPState mask 1 1 30

solve = solveDay parser solve1 skipPart