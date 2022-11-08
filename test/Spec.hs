import Test.Hspec ( hspec, describe, it, shouldBe )
import Lib
    ( bfsolve,
      blocker,
      covers,
      freecells,
      freeingmoves,
      goalmoves,
      isHorizontal,
      merge,
      move,
      moves,
      singlemove,
      solved, 
      premoves,
      expands,
      psolve )

main :: IO ()
main = hspec $ do

    let grid = [(17, 18), (1, 15), (2, 9), (3, 10), (4, 11), (5, 6), (12, 19), (13, 27), (24, 26), (31, 38), (33, 34), (36, 37), (40, 41)]

    describe "isHorizontal" $ do

        it "should detect whether the vehicle is in horizontal orientation" $ do
            isHorizontal (24, 26) `shouldBe` True
            isHorizontal (1, 25)  `shouldBe` False

    describe "merge" $ do

        it "merge the overlapping" $ do
            merge [1..5] [] `shouldBe` [1..5]
            merge [] [6..10] `shouldBe` [6..10]
            merge [1..5] [6..10] `shouldBe` [1..10]
            merge [1..8] [3..10] `shouldBe` [1, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 10]

    describe "freecells" $ do

        it "shoud provide the non occupied cells" $ do
            freecells grid `shouldBe` [16, 22, 23, 29, 30, 32, 39]

    describe "moves" $ do

        it "provides all possible moves" $ do
            moves grid `shouldBe` [(0, 16), (1, 22), (2, 16), (8, 23), (10, 32), (12, 39)]

    describe "singlemove" $ do

        it "should move a car horizontally into the direction of cell by one step" $ do
            singlemove (31, 32) 33 `shouldBe` (32, 33)
            singlemove (31, 32) 34 `shouldBe` (32, 33)
            singlemove (31, 32) 30 `shouldBe` (30, 31)
            singlemove (31, 32) 29 `shouldBe` (30, 31)
            singlemove (31, 32) 31 `shouldBe` (31, 32)
            singlemove (31, 32) 32 `shouldBe` (31, 32)

        it "should move a truck horizontally into the direction of cell by one step" $ do
            singlemove (31, 33) 34 `shouldBe` (32, 34)
            singlemove (31, 33) 30 `shouldBe` (30, 32)
            singlemove (31, 33) 29 `shouldBe` (30, 32)
            singlemove (31, 33) 31 `shouldBe` (31, 33)
            singlemove (31, 33) 32 `shouldBe` (31, 33)
            singlemove (31, 33) 33 `shouldBe` (31, 33)

        it "should move a car verically into the direction of cell by one step" $ do
            singlemove (18, 25) 32 `shouldBe` (25, 32)
            singlemove (18, 25) 39 `shouldBe` (25, 32)
            singlemove (18, 25) 11 `shouldBe` (11, 18)
            singlemove (18, 25)  4 `shouldBe` (11, 18)
            singlemove (18, 25) 18 `shouldBe` (18, 25)
            singlemove (18, 25) 25 `shouldBe` (18, 25)

        it "should move a truck verically into the direction of cell by one step" $ do
            singlemove (18, 32) 39 `shouldBe` (25, 39)
            singlemove (18, 32) 11 `shouldBe` (11, 25)
            singlemove (18, 32)  4 `shouldBe` (11, 25)
            singlemove (18, 32) 18 `shouldBe` (18, 32)
            singlemove (18, 32) 25 `shouldBe` (18, 32)
            singlemove (18, 32) 32 `shouldBe` (18, 32)

    describe "move" $ do

        it "should change a grid accordingly to move (0, 16)" $ do
            let m = (0, 16)
            let v = fst m
            let moved = move grid m
            take v moved `shouldBe` take v grid
            moved!!v `shouldBe` (16, 17)
            drop (v + 1) moved `shouldBe` drop (v + 1) grid

        it "should change a grid accordingly to move (1, 22)" $ do
            let m = (1, 22)
            let v = fst m
            let moved = move grid m
            take v moved `shouldBe` take v grid
            moved!!v `shouldBe` (8, 22)
            drop (v + 1) moved `shouldBe` drop (v + 1) grid

        it "should change a grid accordingly to move (2, 16)" $ do
            let m = (2, 16)
            let v = fst m
            let moved = move grid m
            take v moved `shouldBe` take v grid
            moved!!v `shouldBe` (9, 16)
            drop (v + 1) moved `shouldBe` drop (v + 1) grid

        it "should change a grid accordingly to move (8, 23)" $ do
            let m = (8, 23)
            let v = fst m
            let moved = move grid m
            take v moved `shouldBe` take v grid
            moved!!v `shouldBe` (23, 25)
            drop (v + 1) moved `shouldBe` drop (v + 1) grid

        it "should change a grid accordingly to move (10, 32)" $ do
            let m = (10, 32)
            let v = fst m
            let moved = move grid m
            take v moved `shouldBe` take v grid
            moved!!v `shouldBe` (32, 33)
            drop (v + 1) moved `shouldBe` drop (v + 1) grid

        it "should change a grid accordingly to move (12, 39)" $ do
            let m = (12, 39)
            let v = fst m
            let moved = move grid m
            take v moved `shouldBe` take v grid
            moved!!v `shouldBe` (39, 40)
            drop (v + 1) moved `shouldBe` drop (v + 1) grid

        it "to the same cell should be ignored (10, 33)" $ do
            let m = (10, 33)
            let moved = move grid m
            moved `shouldBe` grid

    describe "solved" $ do

        it "should detect whether a grid is solved" $ do
            solved grid `shouldBe` False
            solved [(19, 20), (1, 15), (2, 9), (3, 10), (4, 11), (5, 6), (24, 26), (31, 38), (33, 34), (36, 37), (40, 41)] `shouldBe` True

    describe "breadth-first" $ do

        it "should be able to find a solution" $ do

            let mmvs = bfsolve grid
            fmap length mmvs `shouldBe` Just 34

            let msolution = do mvs <- mmvs
                               let grid' = foldl move grid mvs
                               return (solved grid')
            msolution `shouldBe` Just True

    describe "goalmoves" $ do

        it "should provide all the moves to necessary to get special vehicle to the exit" $ do
            goalmoves grid `shouldBe` [(0, 19), (0, 20)]

    describe "covers" $ do

        it "should detect whether a cell is within a horizontal vehicle" $ do
            let truck = (24, 26)
            covers 1  truck `shouldBe` False
            covers 23 truck `shouldBe` False
            covers 24 truck `shouldBe` True
            covers 25 truck `shouldBe` True
            covers 26 truck `shouldBe` True
            covers 27 truck `shouldBe` False
            covers 30 truck `shouldBe` False
            covers 32 truck `shouldBe` False

        it "should detect whether a cell is within a vertical car" $ do
            let car = (12, 19)
            covers 4  car `shouldBe` False
            covers 5  car `shouldBe` False
            covers 12 car `shouldBe` True
            covers 19 car `shouldBe` True
            covers 18 car `shouldBe` False
            covers 23 car `shouldBe` False
            covers 26 car `shouldBe` False

        it "should detect whether a cell is within a vertical truck" $ do
            let truck = (12, 26)
            covers 4  truck `shouldBe` False
            covers 5  truck `shouldBe` False
            covers 12 truck `shouldBe` True
            covers 19 truck `shouldBe` True
            covers 26 truck `shouldBe` True
            covers 27 truck `shouldBe` False
            covers 33 truck `shouldBe` False
            covers 37 truck `shouldBe` False

    describe "blocker" $ do

        let vehicle = blocker grid

        it "should find covering vehicles" $ do
            vehicle 18 `shouldBe` Just (0, (17, 18))
            vehicle 17 `shouldBe` Just (0, (17, 18))
            vehicle 1  `shouldBe` Just (1, (1, 15))
            vehicle 8  `shouldBe` Just (1, (1, 15))
            vehicle 15 `shouldBe` Just (1, (1, 15))

        it "should find nothing for free cells" $ do
            vehicle 16 `shouldBe` Nothing
            vehicle 22 `shouldBe` Nothing
            vehicle 23 `shouldBe` Nothing

        it "should deal with an empty grid" $ do
            blocker [] 18 `shouldBe` Nothing

        it "should deal with a sparse grid" $ do
            blocker [(17, 18)] 18 `shouldBe` Just (0, (17, 18))
            blocker [(17, 18)] 16 `shouldBe` Nothing

    describe "freeingmoves" $ do

        it "should provide moves freeing a cell occupied by a horizontal car" $ do

            let car = (31, 32)

            freeingmoves 30 car `shouldBe` []
            freeingmoves 34 car `shouldBe` []

            covers 31 car `shouldBe` True
            let free31 = freeingmoves 31 car
            length free31 `shouldBe` 2
            let free31_1 = head free31
            let free31_2 = head $ tail free31
            foldl singlemove car free31_1 `shouldBe` (32, 33)
            foldl singlemove car free31_2 `shouldBe` (29, 30)
            
            covers 32 car `shouldBe` True
            let free32 = freeingmoves 32 car
            length free32 `shouldBe` 2
            let free32_1 = head free32
            let free32_2 = head $ tail free32
            foldl singlemove car free32_1 `shouldBe` (33, 34)
            foldl singlemove car free32_2 `shouldBe` (30, 31)

        it "should provide moves freeing a cell occupied by a horizontal truck" $ do

            let truck = (31, 33)

            freeingmoves 30 truck `shouldBe` []
            freeingmoves 34 truck `shouldBe` []

            covers 31 truck `shouldBe` True
            let free31 = freeingmoves 31 truck
            length free31 `shouldBe` 1
            let free31_1 = head free31
            foldl singlemove truck free31_1 `shouldBe` (32, 34)
            
            covers 32 truck `shouldBe` True
            let free32 = freeingmoves 32 truck
            length free32 `shouldBe` 1
            let free32_1 = head free32
            foldl singlemove truck free32_1 `shouldBe` (29, 31)

            covers 33 truck `shouldBe` True
            let free33 = freeingmoves 33 truck
            length free33 `shouldBe` 1
            let free33_1 = head free33
            foldl singlemove truck free33_1 `shouldBe` (30, 32)

        it "should provide moves freeing a cell occupied by a vertical car" $ do

            let car = (12, 19)

            freeingmoves  5 car `shouldBe` []
            freeingmoves 26 car `shouldBe` []

            covers 12 car `shouldBe` True
            let free12 = freeingmoves 12 car
            length free12 `shouldBe` 1
            let free12_1 = head free12
            foldl singlemove car free12_1 `shouldBe` (19, 26)

            covers 19 car `shouldBe` True
            let free19 = freeingmoves 19 car
            length free19 `shouldBe` 2
            let free19_1 = head free19
            let free19_2 = head $ tail free19
            foldl singlemove car free19_1 `shouldBe` (5, 12)
            foldl singlemove car free19_2 `shouldBe` (26, 33)

        it "should provide moves freeing a cell occupied by a vertical truck" $ do

            let truck = (12, 26)

            freeingmoves  5 truck `shouldBe` []
            freeingmoves 33 truck `shouldBe` []

            covers 12 truck `shouldBe` True
            let free12 = freeingmoves 12 truck
            length free12 `shouldBe` 1
            let free12_1 = head free12
            foldl singlemove truck free12_1 `shouldBe` (19, 33)

            covers 19 truck `shouldBe` True
            let free19 = freeingmoves 19 truck
            length free19 `shouldBe` 1
            let free19_1 = head free19
            foldl singlemove truck free19_1 `shouldBe` (26, 40)

            covers 26 truck `shouldBe` True
            let free26 = freeingmoves 26 truck
            length free26 `shouldBe` 1
            let free26_1 = head free26
            foldl singlemove truck free26_1 `shouldBe` (5, 19)

    describe "premoves" $ do

        let pmvs = premoves grid

        it "should ..." $ do
            pmvs (0, 19) `shouldBe` [[(6, 5)], [(6, 26),(6, 33)]]
            pmvs (0, 20) `shouldBe` [[(7, 34), (7, 41)]]
    
    describe "expands" $ do

        let exps = expands grid

        it "should stick to valid moves" $ do
            let validmoves = moves grid
            (validmoves >>= exps) `shouldBe` validmoves

        {-- it "should translate the invalid to a sequence of valid moves" $ do
            exps (9, 24) `shouldBe` [] --}

    describe "psolve" $ do

        it "should be able to find a solution" $ do

            let mmvs = psolve grid
            fmap length mmvs `shouldBe` Just 38

            let msolution = do mvs <- mmvs
                               let grid' = foldl move grid mvs
                               return (solved grid')
            msolution `shouldBe` Just True