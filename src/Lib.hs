{-# LANGUAGE TupleSections #-}
module Lib
    ( someFunc,
      isHorizontal,
      merge,
      freecells,
      moves,
      singlemove,
      move,
      solved,
      bfsolve,
      goalmoves,
      covers,
      blocker,
      freeingmoves,
      premoves,
      expands,
      newplans,
      psearch,
      psolve ) where

import Data.List ((\\))

{-- junction
     1   2   3   4   5   6
     8   9  10  11  12  13
    15  16  17  18  19  20
    22  23  24  25  26  27
    29  30  31  32  33  34
    36  37  38  39  40  41

    x x x x x-x
    | | | |
    x x x x x x  
    |       | |
    x . x-x x x
              |
    . . x-x-x x
               
    . . x . x-x
        |
    x-x x . x-x
--}

someFunc :: IO ()
someFunc = print "done"

type Cell      = Int
type Grid      = [(Cell, Cell)]
type Vehicle   = Int
type Move      = (Vehicle, Cell)
type Path      = ([Move], Grid)          -- sequence of moves together with the resulting state
type Frontier  = [Path]                  -- list of paths waiting to be explored further
type Plan      = [Move]

isHorizontal :: (Cell, Cell) -> Bool
isHorizontal (r, f) = r > f - 7

merge :: [Cell] -> [Cell] -> [Cell]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x < y     = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys

freecells :: Grid -> [Cell]
freecells grid = allcells \\ occupied
    where
        allcells = [c | c <- [1..41], c `mod` 7 /= 0]
        occupied = foldr (merge . fillcells) [] grid
        fillcells p@(r, f) = if isHorizontal p then [r .. f] else [r, r + 7 .. f]

-- returns all possible moves of a given grid
moves :: Grid -> [Move]
moves grid = [(v, c) | (v, i) <- zip [0..] grid, c <- adjs i, c `elem` fs]
     where
        fs = freecells grid
        adjs p@(r, f) = if isHorizontal p then [f + 1, r - 1] else [f + 7, r - 7]

-- detects wether cell is part of a vehicle
covers :: Cell -> (Cell, Cell) -> Bool
covers c (r, f) = r <= c && c <= f && (r > f - 7 || (c - r) `mod` 7 == 0)

singlemove :: (Cell, Cell) -> Cell -> (Cell, Cell)
singlemove p@(r, f) c
    | covers c p          = p
    | horizontal && c > f = (r + 1, f + 1)
    | horizontal          = (r - 1, f - 1)
    | c < r               = (r - 7, f - 7)
    | otherwise           = (r + 7, f + 7)
    where horizontal = isHorizontal p

move :: Grid -> Move -> Grid
move g (v, c) = case splitAt v g of
    (g1, [])   -> g1
    (g1, [i])  -> g1 ++ [singlemove i c]
    (g1, i:g2) -> g1 ++ singlemove i c : g2

-- a grid solved if the front of vehicle 0 is at exit cell
solved :: Grid -> Bool
solved grid = snd (head grid) == 20

{-- breadth-first strategy --}
bfsolve :: Grid -> Maybe Plan
bfsolve grid = bfsearch' [] [] [([], grid)]
    where
        bfsearch' :: [Grid] -> Frontier -> Frontier -> Maybe [Move]
        bfsearch' _  [] [] =  Nothing
        bfsearch' qs rs [] =  bfsearch' qs [] rs
        bfsearch' qs rs (p@(ms, q) : ps)
            | solved q    = Just ms
            | q `elem` qs = bfsearch' qs rs ps
            | otherwise   = bfsearch' (q:qs) (succs p ++ rs) ps
            where
                succs (mvs, g) = [(mvs ++ [m], move g m) | m <- moves g]

{-- planning strategy --}

-- gives the list of moves required to step the special vehicle to the exit
goalmoves :: Grid -> Plan
goalmoves g = [(0, c) | c <- [snd (head g) + 1 .. 20]]

-- detects the vehicle occupiing the given cell
blocker :: Grid -> Cell -> Maybe (Vehicle, (Cell, Cell))
blocker grid = search (zip [0..] grid)
    where
        search []           _ = Nothing
        search ((v, i):vis) c = if covers c i then Just (v, i) else search vis c

-- provides sets of list of cells serving as a plan to free a cell occupied by a vehicle
freeingmoves :: Cell -> (Cell, Cell) -> [[Cell]]
freeingmoves c p@(r, f)
    | not $ covers c p = []
    | isHorizontal p   = [[f + 1, f +  2 .. f + c - r + 1] |      f + c - r + 1 < h +  7] ++
                         [[r - 1, r -  2 .. r + c - f - 1] | h <  r + c - f - 1         ]
    | otherwise        = [[r - 7, r - 14 .. r + c - f - 7] | v <= r + c - f - 7         ] ++
                         [[f + 7, f + 14 .. f + c - r + 7] |      f + c - r + 7 < v + 36]
    where (h, v) = (f - f `mod` 7, r `mod` 7)

-- provides plans to make a move legal
premoves :: Grid -> Move -> [Plan]
premoves g (_, c) = case blocker g c of
    Just (v, i) -> fmap (fmap (v,)) (freeingmoves c i)
    Nothing     -> []

-- expands a far distance move into a sequence of moves
expands :: Grid -> Move -> [Move]
expands g (v, c) = fmap (v,) cells
    where
        p@(r, f) = g!!v
        cells | isHorizontal p = if c > f then [f + 1, f +  2 .. c] else [r - 1, r -  2 .. c]
              | otherwise      = if c > f then [f + 7, f + 14 .. c] else [r - 7, r - 14 .. c]

{-- m: first move in the current plan
    if m is legal, make it
--}
newplans :: Grid -> Plan -> [Plan]
newplans _ []     = []
newplans g (m:ms) = if n `elem` legalmoves 
                    then [ns] 
                    else concat [
                       newplans g (pns ++ ns) | 
                       pns <- premoves g n, 
                       all (`notElem` ns) pns
                    ]
    where 
        ns = expands g m ++ ms
        n = head ns
        legalmoves = moves g

psearch :: [Grid] -> [([Move], Grid, Plan)] -> [([Move], Grid, Plan)] -> Maybe [Move]
psearch _  [] [] = Nothing
psearch qs rs [] = psearch qs [] rs
psearch qs rs (p@(ms, q, _) : ps)
    | solved q    = Just ms
    | q `elem` qs = psearch qs rs ps
    | otherwise   = psearch (q:qs) (bsuccs p ++ rs) (asuccs p ++ ps)
    where
        asuccs (mvs, g, pln) = [(mvs ++ [mv], move g mv, pln')  | mv:pln' <- newplans g pln]
        bsuccs (mvs, g, _  ) = [(mvs ++ [mv], g', goalmoves g') | mv <- moves g, let g' = move g mv]

psolve :: Grid -> Maybe Plan
psolve g = psearch [] [] [([], g, goalmoves g)]