import Sokoban

import Data.Map
import Prelude hiding (lookup, Left, Right)
import Test.Hspec

main = hspec $ do
  describe "a sokoban game" $ do
    describe "representation" $ do
      it "can be represented by a string" $ do
        let g1 = decode $ unlines ["#####"
                                  ,"#.$@*"]
            g2 = decode $ unlines ["   "
                                  ,"  +"]
        workerPos g1        `shouldBe` Pos 3 1
        lookup (Pos 0 1) (content g1) `shouldBe` Just [Wall]
        lookup (Pos 1 1) (content g1) `shouldBe` Just [Target]
        lookup (Pos 2 1) (content g1) `shouldBe` Just [Crate]
        lookup (Pos 4 1) (content g1) `shouldBe` Just [Crate, Target]
        workerPos g2        `shouldBe` Pos 2 1
    describe "operations" $ do
      it "The worker can move in any Direction" $ do
        let g3 =decode $ unlines ["   "
                                 ," @ "
                                 ,"   "]
        workerPos (move g3 Left)  `shouldBe` Pos 0 1
        workerPos (move g3 Right) `shouldBe` Pos 2 1
        workerPos (move g3 Up)    `shouldBe` Pos 1 0
        workerPos (move g3 Down)  `shouldBe` Pos 1 2
      it "When a worker moves into a crate, he pushes it too" $ do
        let g4 = move (decode " $@") Left
        workerPos g4                  `shouldBe` Pos 1 0
        lookup (Pos 1 0) (content g4) `shouldBe` Nothing
        lookup (Pos 0 0) (content g4) `shouldBe` Just [Crate]
        let g5 = (move (decode " *@") Left)
        workerPos g5                  `shouldBe` Pos 1 0
        lookup (Pos 1 0) (content g5) `shouldBe` Just [Target]
        content g5 ! Pos 0 0 `shouldBe` [Crate]
        let g6 = (move (decode ".$@") Left)
        workerPos g6                  `shouldBe` Pos 1 0
        lookup (Pos 1 0) (content g6) `shouldBe` Nothing
        lookup (Pos 0 0) (content g6) `shouldBe` Just [Crate, Target]
      it "A worker cannot move into a wall" $ do
        let g6 = decode "#@"
        workerPos (move g6 Left) `shouldBe` Pos 1 0
      it "A crate cannot be moved into a wall or another crate" $ do
        let g7 = decode "#$@"
        workerPos (move g7 Left) `shouldBe` Pos 2 0
        let g8 = decode " $$@"
        workerPos (move g8 Left) `shouldBe` Pos 3 0
    describe "winning conditions" $ do
      describe "a game is over when all targets have a crate" $ do
        it "finished game example" $ do
          let g9  = decode "****@"
          over g9  `shouldBe` True
        it "unfinished game example" $ do
          let g10 = decode "**.$@"
          over g10 `shouldBe` False 

