import Prelude hiding (Either(..))
import Data.List (sort, delete)
import Control.Monad (forM_)
import System.IO (stdin, stdout, hSetEcho, hSetBuffering, BufferMode(..))

data Input = Up
            | Down
            | Left
            | Right
            | Restart
            deriving (Show, Eq, Ord)

type Coord = (Int, Int)

data World = World  {walls
                    ,crates
                    ,storages   :: [Coord]
                    ,player     :: Coord
                    ,widthMax   :: Coord
                    ,steps      :: Int
                    } deriving (Show)

emptyWorld = World  {walls      = []
                    ,crates     = []
                    ,storages   = []
                    ,player     = (0,0)
                    ,widthMax   = (0,0)
                    ,steps      = 0
                    }


updateCurrentLoc :: Coord -> Input -> Coord
updateCurrentLoc (x,y) input =
    case input of
        Up      -> (x, y-1)
        Down    -> (x, y+1)
        Left    -> (x-1, y)
        Right   -> (x+1, y)

{-
This game only implements a single level and it is
this one hard coded here. Not optimal. Perhaps functionality
for reading level designs from outside sources will be 
implemented later on when continued outside the exercise scope.
-}
level = unlines
    ["##########"
    ,"#  # ## .#"
    ,"# o#    .#"
    ,"#     #  #"
    ,"##### # ##"
    ,"#    o  ##"
    ,"#   ## @##"
    ,"##########"]

{-
Function for loading the level. 
-}
loadLevel :: String -> World
loadLevel str = foldl consume (emptyWorld{widthMax = maximumNumOf}) elems
    where   lns             = lines str
            coords          = [[(x,y) | x <- [0..]] | y <- [0..]]
            elems           = concat $ zipWith zip coords lns
            maximumNumOf    = fst . last $ elems
            consume world (c, element) =
                case element of
                    '@' -> world{player     = c}
                    'o' -> world{crates     = c:crates world}
                    '#' -> world{walls      = c:walls world}
                    '.' -> world{storages    = c:storages world}
                    ' ' -> world
                    otherwise -> error (show element ++ " is an unknown element")

{-
Function for loading the world in its current state.
-}
loadWorld :: World -> IO()
loadWorld w = putStrLn . unlines . map (map displayCharRules) $ coords
    where (maxx, maxy)          = widthMax w
          coords                = [[(x,y) | x <- [0..maxx]] | y <- [0..maxy]]
          isPlayer w c  = player w == c
          displayCharRules c    =
              case () of () | isCrate w c && isStorages w c     -> '*'
                            | isPlayer w c && isStorages w c    -> '+'
                            | isWall w c                        -> '#'
                            | isPlayer w c                      -> '@'
                            | isCrate w c                       -> 'o'
                            | isStorages w c                    -> '.'
                            | otherwise                         -> ' '

{-
Function to change the world state.
Performs the necessary checks to determine if the input move
is legal / possible.
The actual legality check could perhaps be implemented in a separate
function of its own but in this scale the code seems to work so let's
not risk breaking it for now.
-}
modifyWorld :: World -> Input -> World
modifyWorld world input =
    case () of ()   | isWall    world newPos -> world
                    | isCrate   world newPos -> 
                        if not (isCrate world newPos') && not (isWall world newPos')
                            then moveCrate world' newPos newPos'
                            else world
                    | otherwise        -> world'
    where   oldPos  = player world
            newPos  = updateCurrentLoc oldPos input
            newPos' = updateCurrentLoc newPos input
            world'  = world{player = newPos, steps = steps world + 1}
            moveCrate w old new = w{crates = new:delete old (crates world)}
            bunchOfcrates = crates world

getInput :: IO Input
getInput = do
    char <- getChar
    case char of
        'w' -> return Up
        'a' -> return Left
        's' -> return Down
        'd' -> return Right
        'r' -> return Restart -- not implemented yet
        otherwise -> getInput

isStorages :: World -> Coord -> Bool
isStorages world coord = elem coord $ storages world

isWall :: World -> Coord -> Bool
isWall world coord = elem coord $ walls world

isCrate :: World -> Coord -> Bool
isCrate world coord = elem coord $ crates world

isFinished :: World -> Bool
isFinished world = sort (crates world) == sort (storages world)

main :: IO()
-- Main simply runs the gameloop 
main = do
    {-
    Apparently the NoBuffering has a bug on Windows platform;
    The key strokes require an enter stroke when run on Windows,
    but works as intended on Linux platform. Workaround not implemented
    because it would brake the NoBuffering functionality on Linux platforms.
    -}
    hSetEcho stdin True
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    gameLoop $ loadLevel level

gameLoop world = do
    loadWorld world
    input <- getInput
    let world' = modifyWorld world input
    if isFinished world'
        then loadWorld world' >> print "SUCCESS! All crates moved to storages."
        else gameLoop world'