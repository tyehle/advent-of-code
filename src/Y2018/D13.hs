module Y2018.D13 where


import Control.Monad
import Data.Functor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Parsec
import Text.Parsec.Char
import ParseUtil
import Data.List (find, group, sort, sortOn)

type Loc = (Int, Int)
data Turn = TurnLeft | TurnRight | Straight deriving (Show, Eq, Ord)
data Direction = North | South | East | West deriving (Show, Eq, Ord)

data Track = Vert | Horiz | TurnBack | TurnFwd | Junction deriving (Show)
data Cart = Cart Loc Direction Turn deriving (Show, Eq, Ord)


getCartLoc :: Cart -> Loc
getCartLoc (Cart loc _ _) = loc

run :: String -> IO ()
run fileName = do
  input <- readFile fileName
  let (tracks, carts) = unsafeParse (parseBlocks (Map.empty, [])) fileName input
  let stepper = stepTic (tracks Map.!)
  print (Right carts :: Either Loc [Cart])
  print $ findCrash stepper carts


findCrash :: ([Cart] -> Either Loc [Cart]) -> [Cart] -> Loc
findCrash stepper carts = case stepper carts of
                            (Left loc) -> loc
                            (Right newCarts) -> findCrash stepper newCarts


stepTic :: (Loc -> Track) -> [Cart] -> Either Loc [Cart]
stepTic getTrack inCarts = go [] (sortOn (\(Cart (x, y) _ _) -> (y, x)) inCarts)
  where
    go :: [Cart] -> [Cart] -> Either Loc [Cart]
    go done [] = return . reverse $ done
    go done (cart:carts) = maybe (go (newCart:done) carts) Left maybeCollision
      where
        newCart = move getTrack cart
        maybeCollision = collisionLoc $ done ++ newCart:carts


move :: (Loc -> Track) -> Cart -> Cart
move getTrack c@(Cart loc d t) = setCartLoc nextLoc (nextDirection c (getTrack nextLoc))

  where
    nextLoc :: Loc
    nextLoc = newLoc loc d

    setCartLoc :: Loc -> Cart -> Cart
    setCartLoc loc (Cart _ d t) = Cart loc d t


    newLoc :: Loc -> Direction -> Loc
    newLoc (x, y) North = (x, y-1)
    newLoc (x, y) South = (x, y+1)
    newLoc (x, y) East = (x+1, y)
    newLoc (x, y) West = (x-1, y)


nextDirection :: Cart -> Track -> Cart
nextDirection (Cart loc d t) Vert = Cart loc d t

nextDirection (Cart loc d t) Horiz = Cart loc d t

nextDirection (Cart loc North t) TurnBack = Cart loc West t
nextDirection (Cart loc South t) TurnBack = Cart loc East t
nextDirection (Cart loc East t) TurnBack = Cart loc South t
nextDirection (Cart loc West t) TurnBack = Cart loc North t

nextDirection (Cart loc North t) TurnFwd = Cart loc East t
nextDirection (Cart loc South t) TurnFwd = Cart loc West t
nextDirection (Cart loc East t) TurnFwd = Cart loc North t
nextDirection (Cart loc West t) TurnFwd = Cart loc South t

-- left, straight, right, left, straight, ...
nextDirection (Cart loc d Straight) Junction = Cart loc d TurnRight
nextDirection (Cart loc d TurnLeft) Junction = Cart loc (rotLeft d) Straight
  where rotLeft North = West
        rotLeft South = East
        rotLeft East = North
        rotLeft West = South
nextDirection (Cart loc d TurnRight) Junction = Cart loc (rotRight d) TurnLeft
  where rotRight North = East
        rotRight South = West
        rotRight East = South
        rotRight West = North


collisionLoc :: [Cart] -> Maybe Loc
collisionLoc carts = head <$> find ((/= 1) . length) grouped
  where
      grouped = group . sort . map getCartLoc $ carts


parseBlocks :: (Map Loc Track, [Cart]) -> Parsec String () (Map Loc Track, [Cart])
parseBlocks initial =  eof $> initial
                   <|> (updateSegment initial >>= parseBlocks)



updateSegment :: (Map Loc Track, [Cart]) -> Parsec String () (Map Loc Track, [Cart])
updateSegment (trackMap, carts) = do
  (mcart, mtrack) <- parseSegment
  currLoc <- getLoc
  return (addTrack mtrack currLoc, addCart mcart)

  where
    addCart :: Maybe Cart -> [Cart]
    addCart Nothing = carts
    addCart (Just cart) = cart : carts

    addTrack :: Maybe Track -> Loc -> Map Loc Track
    addTrack Nothing  _ = trackMap
    addTrack (Just track) currLoc = Map.insert currLoc track trackMap


getLoc :: Parsec String () Loc
getLoc = do
  sourcePos <- getPosition
  return (sourceColumn sourcePos - 2, sourceLine sourcePos - 1)

parseSegment :: Parsec String () (Maybe Cart, Maybe Track)
parseSegment =  char '|'  $> (Nothing, Just Vert)
            <|> char '-'  $> (Nothing, Just Horiz)
            <|> char '/'  $> (Nothing, Just TurnFwd)
            <|> char '\\' $> (Nothing, Just TurnBack)
            <|> char '+'  $> (Nothing, Just Junction)
            <|> char '^'  *> buildResult North Vert
            <|> char 'v'  *> buildResult South Vert
            <|> char '>'  *> buildResult East Horiz
            <|> char '<'  *> buildResult West Horiz
            <|> space     $> (Nothing, Nothing)
  where
    buildResult :: Direction -> Track -> Parsec String () (Maybe Cart, Maybe Track)
    buildResult dir track = do
      loc <- getLoc
      return (Just $ Cart loc dir TurnLeft, Just track)
