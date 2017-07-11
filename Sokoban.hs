module Sokoban where 

import Data.Map
import Prelude hiding (lookup, filter, Left, Right)

data Position = Pos Int Int
  deriving (Show, Eq, Ord)

data Tile = Wall
           | Crate
           | Target
  deriving (Show, Eq)

data Direction = Up | Down | Left | Right

data Sokoban = Sokoban {
  workerPos :: Position,
  content   :: Map Position [Tile]
  }
  deriving (Show, Eq)

newPos (Pos x y) direction =
  case direction of
  Up    -> Pos  x    (y-1)
  Down  -> Pos  x    (y+1)
  Left  -> Pos (x-1)  y
  Right -> Pos (x+1)  y

moveWithCrate game direction =
  case lookup nextCratePos (content game) of
  Just [Wall]    -> game
  Just (Crate:_) -> game
  _              -> game { workerPos = cratePos
                         , content   = moveCrate}
  where cratePos = (newPos (workerPos game) direction)
        nextCratePos = (newPos cratePos direction)
        moveCrate = addCrate nextCratePos (
                    removeCrate cratePos
                          (content game))

move game direction =
  case lookup nextPos (content game) of
  Just [Wall]    -> game
  Just (Crate:_) -> moveWithCrate game direction
  _              -> game { workerPos = nextPos}
  where nextPos = (newPos (workerPos game) direction)

addCrate pos content =
  case lookup pos content of
  Nothing -> insert pos [Crate] content
  Just x  -> insert pos (Crate:x) content

removeCrate pos content =
  case lookup pos content of
  Just ([Crate]) -> delete pos content
  Just (Crate:x) -> insert pos x content

over game = noTargetLeft
  where noTargetLeft = filter ((==) [Target]) (content game) == empty

decode a = lineRead 0 (lines a) Sokoban {workerPos= (Pos 0 0)
                                          ,content  = empty}

lineRead x [] game = game
lineRead x (line:rest) game = lineRead (x+1) rest (symbolRead 0 x line game)

symbolRead x y [] game = game
symbolRead x y (e:es) (game@Sokoban{content=map}) =
  case e of
  '#' -> symbolRead (x+1) y es game {content = (insert (Pos x y) [Wall] map)}
  '.' -> symbolRead (x+1) y es game {content = (insert (Pos x y) [Target] map)}
  '$' -> symbolRead (x+1) y es game {content = (insert (Pos x y) [Crate] map)}
  '@' -> symbolRead (x+1) y es game {workerPos = Pos x y}
  '+' -> symbolRead (x+1) y es game {workerPos = (Pos x y), content = (insert (Pos x y) [Target] map)}
  '*' -> symbolRead (x+1) y es game {content = (insert (Pos x y) [Crate, Target] map)}
  ' ' -> symbolRead (x+1) y es game
  _   -> error (e:": symbol not valid")
