module Interpretation (interpret)
where


data TurtleState = S {
      x, y, theta, heading :: Double
    } deriving Show


d2r    :: Double -> Double
d2r d   = (d/180 * pi)


initialState       :: Double -> [TurtleState]
initialState theta  = [ S 0 0 (d2r theta) (d2r 90) ]


forward                     :: [TurtleState] -> IO [TurtleState]
forward ((S x y t h):ss)     = do putStrLn "F"
                                  return ((S x' y' t h) : ss)
    where x' = x + (cos h)
          y' = y + (sin h)

rotatePos                   :: [TurtleState] -> IO [TurtleState]
rotatePos ((S x y t h):ss)   = return ((S x y t (h + t)) : ss)

rotateNeg                   :: [TurtleState] -> IO [TurtleState]
rotateNeg ((S x y t h):ss)   = return ((S x y t (h - t)) : ss)

push                        :: [TurtleState] -> IO [TurtleState]
push ss'@((S x y t h):ss)    = return ((S x y t h) : ss')

pop                         :: [TurtleState] -> IO [TurtleState]
pop (s:ss)                   = return ss


actOn     :: Char -> IO [TurtleState] -> IO [TurtleState]
actOn c s  = do states <- s
                let s' = case c of
                           'F' -> forward   states
                           '+' -> rotatePos states
                           '-' -> rotateNeg states
                           '[' -> push      states
                           ']' -> pop       states
                s'


interpret         :: String -> Double -> IO [TurtleState]
interpret s theta  = interpret' s (return $ initialState theta)
    where interpret' [] state     = state
          interpret' (x:xs) state = return (actOn x state)
                                    >>= interpret' xs
