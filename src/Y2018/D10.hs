module Y2018.D10 where

import Control.Monad
import Text.Parsec
import Text.Parsec.Char

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo


type Vec = (Int, Int)

--            position velocity
data Dot = Dot Vec Vec deriving (Show)


run :: String -> IO ()
run fileName = do
  dots <- map parseDot . lines <$> readFile fileName
  let allDots = iterate step dots
      numPlots = 200
  let (coolDots, offset) = dropBoring allDots
  zipWithM_ plotDots ["plots/" ++ show n ++ ".png" | n <- [offset..offset+numPlots-1]] coolDots


dropBoring :: [[Dot]] -> ([[Dot]], Int)
dropBoring = go 0
  where
    isClose :: Dot -> Bool
    isClose (Dot (px, py) _) = abs px < 400 && abs py < 400
    go :: Int -> [[Dot]] -> ([[Dot]], Int)
    go idx dots@(d:ds)
      | all isClose d = (dots, idx)
      | otherwise = go (idx + 1) ds

step :: [Dot] -> [Dot]
step = fmap babyStep
  where
    babyStep (Dot (px, py) (vx, vy)) = Dot (px + vx, py + vy) (vx, vy)


plotDots :: String -> [Dot] -> IO ()
plotDots fileName dots = toFile (FileOptions (1200,200) PNG) fileName $ do
  -- plot (points "" coords)
  plot $ liftEC $ do
    color <- takeColor
    shape <- takeShape
    plot_points_values .= (pos <$> dots)
    plot_points_title .= ""
    plot_points_style . point_color .= color
    plot_points_style . point_shape .= shape
    plot_points_style . point_radius .= 10
  return ()
  where
    pos (Dot (px, py) _) = (px, -py)


parseDot :: String -> Dot
parseDot = either (error . show) id . parse dot ""
  where
    dot :: Parsec String () Dot
    dot = do
      string "position=<"
      spaces
      px <- number
      char ','
      spaces
      py <- number
      string "> velocity=<"
      spaces
      vx <- number
      char ','
      spaces
      vy <- number
      char '>'
      eof
      return $ Dot (px, py) (vx, vy)
    number :: Parsec String () Int
    number = read <$> many1 (oneOf $ '-':['0'..'9'])
