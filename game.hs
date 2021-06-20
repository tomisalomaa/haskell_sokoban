import Prelude hiding (Either(..))
import Data.List (sort)

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
                    ,wmax        :: Coord
                    ,steps      :: Int
                    } deriving (Show)

emptyWorld = World  {walls      = []
                    ,crates     = []
                    ,storage    = []
                    ,worker     = (0,0)
                    ,wmax        = (0,0)
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
    ,"#*@ #"
    ,"#####"]

loadLevel :: String -> World
loadLevel str = foldl consume (emptyWorld{wmax = maxi}) elems
    where   lns     = lines str
            elems   = concat $ zipWith zip coords lns
            coords  = [[(x,y) | x <- [0..]] | y <- [0..]]
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
displayWorld = print

modifyWorld :: World -> Input -> World
modifyWorld world input = world

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
isStorage = undefined

isWall :: World -> Coord -> Bool
isWall world coord = elem coord (walls world)

isCrate :: World -> Coord -> Bool
isCrate world coord = elem coord (crates world)

isValid :: World -> Input -> Bool
isValid world input =
    case () of ()   | isWall    world newPos -> False
                    | isCrate   world newPos -> 
                        not (isCrate world newPos') && 
                        not (isWall world newPos')
                    | otherwise        -> True
    where   oldPos  = worker world
            newPos  = add oldPos input
            newPos' = add newPos input

isFinished :: World -> Bool
isFinished world = sort (crates world) == sort (storage world)

main :: IO()
main = gameLoop $ loadLevel level

gameLoop world = do
    input <- getInput
    let world' = if isValid world input
                    then modifyWorld world input
                    else world
    displayWorld world'
    if isFinished world'
        then displayWorld world' >> print "well done!"
        else gameLoop world'