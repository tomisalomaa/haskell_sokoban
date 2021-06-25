import Prelude hiding (Either(..))
import Data.List (sort, delete)
import Control.Monad (forM_)
import System.IO (stdin, stdout, hSetEcho, hSetBuffering, BufferMode(..))

data Input = Up
            | Down
            | Left
            | Right
            deriving (Show, Eq, Ord)

type Coord = (Int, Int)

data World = World  {walls
                    ,crates
                    ,storage    :: [Coord]
                    ,worker     :: Coord
                    ,wmax       :: Coord
                    ,steps      :: Int
                    } deriving (Show)

emptyWorld = World  {walls      = []
                    ,crates     = []
                    ,storage    = []
                    ,worker     = (0,0)
                    ,wmax       = (0,0)
                    ,steps      = 0
                    }

add :: Coord -> Input -> Coord
add (x,y) input =
    case input of
        Up      -> (x,y-1)
        Down    -> (x,y+1)
        Left    -> (x-1,y)
        Right   -> (x+1,y)

level = unlines
    ["#####"
    ,"#.o@#"
    ,"#####"]

loadLevel :: String -> World
loadLevel str = foldl consume (emptyWorld{wmax = maxi}) elems
    where   lns     = lines str
            coords  = [[(x,y) | x <- [0..]] | y <- [0..]]
            elems   = concat $ zipWith zip coords lns
            maxi    = fst . last $ elems
            consume world (c, element) =
                case element of
                    '@' -> world{worker     = c}
                    'o' -> world{crates     = c:crates world}
                    '#' -> world{walls      = c:walls world}
                    '.' -> world{storage    = c:storage world}
                    ' ' -> world
                    otherwise -> error (show element ++ " unknown")

displayWorld :: World -> IO()
displayWorld w = putStrLn . unlines . map (map func) $ coords
    where (maxx, maxy)      = wmax w
          coords            = [[(x,y) | x <- [0..maxx]] | y <- [0..maxy]]
          isWorker w c      = worker w == c
          func c            =
              case () of () | isCrate w c && isStorage w c  -> '*'
                            | isWorker w c && isStorage w c -> '+'
                            | isWall w c                    -> '#'
                            | isWorker w c                  -> '@'
                            | isCrate w c                   -> 'o'
                            | isStorage w c                 -> '.'
                            | otherwise                     -> ' '

modifyWorld :: World -> Input -> World
modifyWorld world input =
    case () of ()   | isWall    world newPos -> world
                    | isCrate   world newPos -> 
                        if not (isCrate world newPos') && not (isWall world newPos')
                            then moveCrate world' newPos newPos'
                            else world
                    | otherwise        -> world'
    where   oldPos  = worker world
            newPos  = add oldPos input
            newPos' = add newPos input
            world' = world{worker = newPos, steps = steps world + 1}
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
        otherwise -> getInput

isStorage :: World -> Coord -> Bool
isStorage world coord = elem coord (storage world)

isWall :: World -> Coord -> Bool
isWall world coord = elem coord (walls world)

isCrate :: World -> Coord -> Bool
isCrate world coord = elem coord (crates world)

isFinished :: World -> Bool
isFinished world = sort (crates world) == sort (storage world)

main :: IO()
main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    gameLoop $ loadLevel level

gameLoop world = do
    displayWorld world
    input <- getInput
    let world' = modifyWorld world input
    if isFinished world'
        then displayWorld world' >> print "well done!"
        else gameLoop world'