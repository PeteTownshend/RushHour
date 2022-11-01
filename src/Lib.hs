{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib
    ( someFunc
    ) where

import Data.List ((\\))

someFunc :: IO ()
someFunc = mapM_ print psolution1

type Cell     = Int
type Grid     = [(Cell, Cell)]
type Vehicle  = Int
type Move     = (Vehicle, Cell)
type Path     = ([Move], Grid)          -- sequence of moves together with the resulting state
type Frontier = [Path]                  -- list of paths waiting to be explored further
type Plan     = [Move]
type APath = ([Move], Grid, Plan)       -- augmented path of moves already made from some starting state
type AFrontier = [APath]

freecells :: Grid -> [Cell]
freecells grid = allcells \\ occupied
    where
        allcells = [c | c <- [1..41], c `mod` 7 /= 0]
        occupied = foldr (merge . fillcells) [] grid
        merge xs [] = xs
        merge [] ys = ys
        merge (x:xs) (y:ys)
            | x < y     = x:merge xs (y:ys)
            | otherwise = y:merge (x:xs) ys
        fillcells (r, f) = if r > f - 7 then [r .. f] else [r, r + 7 .. f]

moves :: Grid -> [Move]
moves grid = [(v, c) | (v, i) <- zip [0..] grid, c <- adjs i, c `elem` freecells grid]
     where
        adjs (r, f) = if r > f - 7 then [f + 1, r - 1] else [f + 7, r - 7]

move :: Grid -> Move -> Grid
move grid (vehicle, cell) = grid1 ++ adjust i cell : grid2
    where
        (grid1, i:grid2) = splitAt vehicle grid
        adjust (r, f) c
            | r > f - 7 = if c > 7 then (r + 1, c) else (c, f - 1)
            | otherwise = if c < r then (c, f - 7) else (r + 7, c)

solved :: Grid -> Bool
solved grid = snd (head grid) == 20 -- a grid solved if the front of vehicle 0 is at exit cell

-- breadth-first strategy
bfsolve :: Grid -> Maybe [Move]
bfsolve grid = bfsearch' [] [] [([], grid)]

bfsearch' :: [Grid] -> Frontier -> Frontier -> Maybe [Move]
bfsearch' _  [] [] =  Nothing
bfsearch' qs rs [] =  bfsearch' qs [] rs
bfsearch' qs rs (p@(ms, q) : ps)
    |   solved q    = Just ms
    |   q `elem` qs = bfsearch' qs rs ps
    |   otherwise   = bfsearch' (q:qs) (succs p ++ rs) ps
    where
        succs (mvs, g) = [(mvs ++ [m], move g m) | m <- moves g]

goalmoves :: Grid -> Plan
goalmoves g = [(0, c) | c <- [snd (head g) + 1..20]]

blocker :: Grid -> Cell -> (Vehicle, (Cell, Cell))
blocker grid = search (zip [0..] grid)
    where
        covers c (r, f) = r <= c && c <= f && (r > f - 7 || (c - r) `mod` 7 == 0)
        search ((v, i) : vis) c = if covers c i then (v, i) else search vis c

freeingmoves :: Cell -> (Vehicle, (Cell, Cell)) -> [[Move]]
freeingmoves c (v, (r, f))
    | r > f - 7 = [[(v, j) | j <- [f + 1 .. c + n]] | c + n < k + 7] ++
                  [[(v, j) | j <- [r - 1, r - 2  .. c - n]] | c - n > k]
    | otherwise = [[(v, j) | j <- [r - 7, r - 14 .. c - m]] | c - m > 0] ++
                  [[(v, j) | j <- [f + 7, f + 14 .. c + m]] | c + m < 42]
    where (k, m, n) = (f - f `mod` 7, f - r + 7, f - r + 1)

premoves :: Grid -> Move -> [[Move]]
premoves g (_, c) = freeingmoves c (blocker g c)

expand :: Grid -> Move -> [Move]
expand g (v, c)
    | r > f - 7 = if c > f 
                  then [(v, p) | p <- [f + 1 .. c]]
                  else [(v, p) | p <- [r - 1, r - 2 .. c]]
    | otherwise = if c > f  
                  then [(v, p) | p <- [f + 7, f + 14 .. c]]
                  else [(v, p) | p <- [r - 7, r - 14 .. c]]
    where (r, f) = g !! v

newplans :: Grid -> Plan -> [Plan]
newplans _ []     = []
newplans g (m:ms) = mkplans (expand g m ++ ms)
    where
        mkplans ns | n `elem` gns = [ns]
                   | otherwise    = concat [mkplans (pns ++ ns) | pns <- premoves g n, all (`notElem` ns) pns]
            where 
                n = head ns
                gns = moves g

psearch :: [Grid] -> AFrontier -> AFrontier -> Maybe [Move]
psearch _  [] [] = Nothing
psearch qs rs [] = psearch qs [] rs
psearch qs rs (p@(ms, q, plan) : ps)
    | solved q      = Just (reverse ms)
    | q `elem` qs   = psearch qs rs ps
    | otherwise     = psearch (q : qs) (bsuccs p ++ rs) (asuccs p ++ ps)
    where
        asuccs :: APath -> [APath]
        asuccs (mvs, q, pl) = [(mvs ++ [mv], move q mv, pl') | mv : pl' <- newplans q pl]
        bsuccs :: APath -> [APath]
        bsuccs (mvs, q, _) = [(mvs ++ [mv], q', goalmoves q') | mv <- moves q, let q' = move q mv]

psolve :: Grid -> Maybe [Move]
psolve g = psearch [] [] [([], g, goalmoves g)]

grid1 :: Grid
grid1 = [(17, 18), (1, 15), (2, 9), (3, 10), (4, 11), (5, 6), (12, 19), (13, 27), (24, 26), (31, 38), (33, 34), (36, 37), (40, 41)]

bfsolution1 :: Maybe [Move]
bfsolution1 = bfsolve grid1

psolution1 :: Maybe [Move]
psolution1 = psolve grid1