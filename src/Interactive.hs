{-# LANGUAGE ScopedTypeVariables #-}

module Interactive
    ( runInteraction
    ) where


import Data.Maybe (fromMaybe)
import System.IO


runInteraction :: forall a. Eq a => (a -> String) -> (a -> a) -> (a -> a) -> a -> IO ()
runInteraction render shuffle step initial = do
  bufferMode <- hGetBuffering stdin
  hSetBuffering stdin NoBuffering
  interact $ \input -> initialRender ++ go [initial] [initial] input
  hSetBuffering stdin bufferMode
  where
    initialRender :: String
    initialRender = render initial

    reset :: String
    reset = "\ESC[" ++ show (length (lines initialRender)) ++ "F\ESC[J"

    clearLine :: String
    clearLine = "\ESC[2K\ESC[G"

    trimMemory :: [a] -> [a]
    trimMemory mem@(a:b:xs)
      | a == b = trimMemory $ b:xs
      | otherwise = take 5 mem
    trimMemory xs = take 5 xs

    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (x:_) = Just x

    go :: [a] -> [a] -> String -> String
    -- eof
    go _ _ [] = clearLine

    -- quit
    go _ _ ('q':_) = clearLine

    -- step
    go _ (prevState:prevStates) ('j':xs) = reset ++ render nextState ++ go [nextState] nextStates xs
      where
        nextState = step prevState
        nextStates = trimMemory $ nextState : prevState : prevStates

    -- step back
    go prevShuffles [s] ('k':xs) = reset ++ render s ++ go prevShuffles [s] xs
    go _ (_:toRender:states) ('k':xs) = reset ++ render toRender ++ go [toRender] (toRender:states) xs

    -- shuffle
    go (prevShuffle:prevShuffles) prevStates ('l':xs) = reset ++ render nextShuffle ++ go nextShuffles prevStates xs
      where
        nextShuffle = shuffle prevShuffle
        nextShuffles = trimMemory $ nextShuffle : prevShuffle : prevShuffles

    -- shuffle back
    go [s] states ('h':xs) = reset ++ render s ++ go [s] states xs
    go (_:toRender:shuffles) states ('h':xs) = reset ++ render toRender ++ go (toRender:shuffles) states xs

    -- end
    go _ prevStates ('G':xs) = reset ++ render lastState ++ go [lastState] states xs
      where
        states :: [a]
        lastState :: a
        states@(lastState:_) = findLastStates prevStates
        findLastStates :: [a] -> [a]
        findLastStates (s:states)
          | nextState == s = s:states
          | otherwise = findLastStates . trimMemory $ nextState:s:states
          where
            nextState = step s

    -- beginning
    go prevShuffles prevStates ('g':xs) = reset ++ render initial ++ go [initial] [initial] xs

    -- leap
    go _ (state:states) ('\n':xs) = "\ESC[F" ++ reset ++ render nextState ++ go [nextState] nextStates xs
      where
        nextStates = trimMemory . (++ states) . reverse . take 6 . iterate step $ state
        nextState = head nextStates

    -- ignore all other characters
    go shuffles@(s:_) states (_:xs) = reset ++ render s ++ go shuffles states xs
