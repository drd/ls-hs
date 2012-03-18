> module Main
> where

> import qualified Graphics.UI.GLFW as GLFW
> import Graphics.Rendering.OpenGL.Raw
> import Graphics.Rendering.GLU.Raw ( gluPerspective )
> import Data.Bits ( (.|.) )
> import System.Exit ( exitWith, ExitCode(..) )
> import Control.Monad ( forever )


LGrammars
=========

Types of LGrammars
------------------

*Deterministic, Context-Free Production:*

p: Letter -> Word

*1L Production:*

p: (Letter, Context, Direction) -> Word

*2L Production:*

p: (Maybe Left, Letter, Maybe Right) -> Word

*Stochastic Production:*

p: (DeterministicProduction, Probability) -> Word

Production Data types
---------------------

A Production is a pair of (predecessor, successor), where the
predecessor may be deterministic, stochastic, or context-sensitive.



> type Word        = String

> data Context     = LeftOf  Word
>                  | RightOf Word

> data Predecessor = OL  Char
>                  | IL  { pred        :: Char,
>                          context     :: Context
>                        }
>                  | IIL { left, right :: Word,
>                          pred        :: Char
>                        }

> data Production = Pr { pre, suc  :: String }
>                   deriving Show

> data LGrammar   = Lg { axiom       :: String,
>                        productions :: [Production] }
>                   deriving Show

Generating iterations of LGrammars
----------------------------------

For each production, start with an empty string accumulator and
attempt to match each production against the beginning of the
axiom or previous production.

if none of the productions match, then cons the first character
of the axiom to the accumulator, and then repeat this process
with the tail of the axiom

If one of the productions match, cons the successor of the production
to the accumulator, and repeat the process with the remainder of the
axiom.

> generate                :: LGrammar -> Int -> String
> generate  lg n           = produce (axiom lg) n
>     where produce s 0    = s
>           produce s n    = produce (next s) (n-1)
>           next []        = ""
>           next (x:xs)    = (suc' x) ++ (next xs)
>           -- next only works on 1-character variables
>           -- to generalize productions, suc' will have
>           -- to return a pair of strings (predecessor, tail)
>           -- and then next = (fst suc') ++ (next (snd suc'))
>           suc' x = suc (applicableProduction [x] (productions lg))
>           applicableProduction s []     = Pr s s
>           applicableProduction s (p:ps)
>               | (pre p) == s = p
>               | otherwise    = applicableProduction s ps


Turtle Graphics
===============

The Turtle is represented as a list of TurtleState's, called a TurtleStack,
each of which contains the current x,y position, a default theta for modification
of the heading, its current direction.

> data TurtleState = S {
>       x, y, theta, heading :: GLfloat
>     } deriving Show

> type TurtleStack = [TurtleState]


A simple helper function converts degrees to radians:

> d2r    :: GLfloat -> GLfloat
> d2r d   = (d/180 * pi)


initialState is used to generate the original stack containing a
turtle at the origin, headed north.

> initialState       :: GLfloat -> TurtleStack
> initialState theta  = [ S 0 0 (d2r theta) (d2r 90) ]


Turtle movement / manipulation functions
----------------------------------------

These functions actually implement the movement of the turtle through
space, as directed by the commands generated in the L-Grammars.

> w = 20
> l = 0.5

> forward                     :: TurtleStack -> IO TurtleStack
> forward ((S x y t h):ss)     = do
>
>   glBegin gl_QUADS
>   glVertex3f (x  + ddx) (y  - ddy) 0
>   glVertex3f (x  - ddx) (y  + ddy) 0
>   glVertex3f (x' - ddx) (y' + ddy) 0
>   glVertex3f (x' + ddx) (y' - ddy) 0
>   glEnd
>
>   return ((S x' y' t h) : ss)
>     where x'  = x + dx
>           y'  = y + dy
>           dx  = l * cos h
>           dy  = l * sin h
>           ddx = -dy / w  -- ddx and ddy are currently used to
>           ddy = -dx / w  -- add width to the lines

> rotatePos                   :: TurtleStack -> IO TurtleStack
> rotatePos ((S x y t h):ss)   = return ((S x y t (h + t)) : ss)

> rotateNeg                   :: TurtleStack -> IO TurtleStack
> rotateNeg ((S x y t h):ss)   = return ((S x y t (h - t)) : ss)

> push                        :: TurtleStack -> IO TurtleStack
> push ss'@((S x y t h):ss)    = return ((S x y t h) : ss')

> pop                         :: TurtleStack -> IO TurtleStack
> pop (s:ss)                   = return ss


actOn is the dispatcher for incoming turtle commands. To add new
commands, implement the behavior in a function from TurtleStack to
IO TurtleStack, and then add the appropriate command here in the
case statement. It works by transforming the current TurtleStack using
the helper functions above, each of which may modify the top of the
TurtleStack, in addition to performing any drawing actions.

> actOn     :: Char -> IO TurtleStack -> IO TurtleStack
> actOn c s  = do states <- s
>                 let states' = case c of
>                            'F' -> forward   states
>                            '+' -> rotatePos states
>                            '-' -> rotateNeg states
>                            '[' -> push      states
>                            ']' -> pop       states
>                 states'


interpret takes a String generated from an LGrammar, as well as
some initial configuration parameters (currently only the default
theta by which to modify the turtle's heading) and using actOn
to modify the initialState accordingly.

> interpret         :: String -> GLfloat -> IO [TurtleState]
> interpret s theta  = interpret' s (return $ initialState theta)
>     where interpret' [] state     = state
>           interpret' (x:xs) state = return (actOn x state)
>                                     >>= interpret' xs


OpenGL Functions
================

These were borrowed rather wholesale from a Haskell port of the very
useful Neon-Helium OpenGL tutorials of yore (where yore = 1999 or so.)
They have been modified and extended slightly to fit my purposes here.


> setupGraphics              :: Int -> Int -> IO ()
> setupGraphics width height  = do
>   True <- GLFW.initialize
>   -- select type of display mode:
>   -- Double buffer
>   -- RGBA color
>   -- Alpha components supported
>   -- Depth buffer
>   let dspOpts = GLFW.defaultDisplayOptions
>                 -- get a 800 x 600 window
>                 { GLFW.displayOptions_width  = width
>                 , GLFW.displayOptions_height = height
>                 -- Set depth buffering and RGBA colors
>                 , GLFW.displayOptions_numRedBits   = 8
>                 , GLFW.displayOptions_numGreenBits = 8
>                 , GLFW.displayOptions_numBlueBits  = 8
>                 , GLFW.displayOptions_numAlphaBits = 8
>                 , GLFW.displayOptions_numDepthBits = 1
>                 -- , GLFW.displayOptions_displayMode = GLFW.Fullscreen
>                 }
>   -- open a window
>   True <- GLFW.openWindow dspOpts
>   -- window starts at upper left corner of the screen
>   GLFW.setWindowPosition 0 0
>   GLFW.setWindowTitle "ls-hs"
>   -- register the function to do all our OpenGL drawing
>   GLFW.setWindowRefreshCallback drawScene
>   -- GLFW.setWindowRefreshCallback (drawScene rt rq)
>   -- register the funciton called when our window is resized
>   GLFW.setWindowSizeCallback resizeScene
>   -- register the function called when the keyboard is pressed.
>   GLFW.setKeyCallback keyPressed
>   GLFW.setWindowCloseCallback shutdown
>   -- initialize our window.
>   initGL
>   forever $ do
>     drawScene
>     GLFW.swapBuffers


> setRenderer   :: (IO ()) -> IO ()
> setRenderer f  = do
>   let r = wrapRenderer f
>   GLFW.setWindowRefreshCallback r
>   forever $ do
>     r
>     GLFW.swapBuffers


> wrapRenderer   :: (IO ()) -> (IO ())
> wrapRenderer r  = do
>   glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
>                          .|. gl_DEPTH_BUFFER_BIT
>   glLoadIdentity
>   r
>   glFlush


> drawScene :: IO ()
> drawScene = do
>   glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
>                          .|. gl_DEPTH_BUFFER_BIT
>   glLoadIdentity
>   glTranslatef (20) (20) (-80)
>   interpret (generate (Lg "F-F-F-F"
>                        [Pr "F" "FF-F-F-F-F-F+F"]) 3) 90
>   glFlush


> initGL :: IO ()
> initGL = do
>   glShadeModel gl_SMOOTH -- enables smooth color shading
>   glClearColor 0 0 0 0 -- Clear the background color to black
>   glClearDepth 1 -- enables clearing of the depth buffer
>   glEnable gl_DEPTH_TEST
>   glDepthFunc gl_LEQUAL  -- type of depth test
>   glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST


> resizeScene :: GLFW.WindowSizeCallback
> resizeScene w     0      = resizeScene w 1 -- prevent divide by zero
> resizeScene width height = do
>   glViewport 0 0 (fromIntegral width) (fromIntegral height)
>   glMatrixMode gl_PROJECTION
>   glLoadIdentity
>   gluPerspective 45 (fromIntegral width/fromIntegral height) 0.1 100
>   glMatrixMode gl_MODELVIEW
>   glLoadIdentity
>   glFlush


> shutdown :: GLFW.WindowCloseCallback
> shutdown = do
>   GLFW.closeWindow
>   GLFW.terminate
>   _ <- exitWith ExitSuccess
>   return True


> keyPressed                  :: GLFW.KeyCallback
> keyPressed GLFW.KeyEsc True  = shutdown >> return ()
> keyPressed _           _     = return ()

> main :: IO ()
> main = do
>   setupGraphics 800 600