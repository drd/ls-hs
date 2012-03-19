> module Main
> where

> import qualified Graphics.UI.GLFW as GLFW
> import Graphics.Rendering.OpenGL.Raw
> import Graphics.Rendering.GLU.Raw ( gluPerspective )
> import Data.Bits ( (.|.) )
> import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
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
each of which contains the current x,y,z position, and
of the heading, its current direction.

> type Scalar = GLfloat
> type Point = (Scalar, Scalar, Scalar)
> type Color = Point
> type Heading = (Point, Point, Point)

> data Pen = Pen {
>       theta  :: Scalar,
>       width  :: Scalar,
>       color  :: Int,
>       colors :: [Color]
>       } deriving Show

> data TurtleState = S {
>       position :: Point,
>       heading  :: Heading,
>       pen      :: Pen
>     } deriving Show

> type TurtleStack = [TurtleState]


A simple helper function converts degrees to radians:

> d2r    :: Scalar -> Scalar
> d2r d   = (d/180 * pi)


initialState is used to generate the original stack containing a
turtle at the origin, headed north.

> initialState      :: Scalar -> Scalar -> TurtleStack
> initialState th w  = [ S ( 0, 0, 0 )
>                          (( 0, 1, 0 ),
>                           ( 1, 0, 0 ),
>                           ( 0, 0, 1 ))
>                          (Pen (d2r th) w 0 [(0.53, 0.50, 0.40),
>                                             (0.15, 0.80, 0.25),
>                                             (0.05, 0.60, 0.05),
>                                             (0.20, 0.70, 0.15),
>                                             (0.10, 0.90, 0.20)]) ]


Turtle movement / manipulation functions
----------------------------------------

These functions actually implement the movement of the turtle through
space, as directed by the commands generated in the L-Grammars.

> delta :: Scalar
> delta = 7

> matMult :: Heading -> Heading -> Heading
> ((a1, b1, c1),
>  (d1, e1, f1),
>  (g1, h1, i1)) `matMult`
>  ((a2, b2, c2),
>   (d2, e2, f2),
>   (g2, h2, i2)) =
>     (((a1*a2 + b1*d2 + c1*g2), (a1*b2 + b1*e2 + c1*h2), (a1*c2 + b1*f2 + c1*i2)),
>      ((d1*a2 + e1*d2 + f1*g2), (d1*b2 + e1*e2 + f1*h2), (d1*c2 + e1*f2 + f1*i2)),
>      ((g1*a2 + h1*d2 + i1*g2), (g1*b2 + h1*e2 + i1*h2), (g1*c2 + h1*f2 + i1*i2)))

> rU       :: Scalar -> Heading
> rU alpha  = ((  cos alpha, sin alpha, 0),
>              ( -sin alpha, cos alpha, 0),
>              (          0,         0, 1)) :: Heading


> rL       :: Scalar -> Heading
> rL alpha  = (( cos alpha, 0, -sin alpha),
>              ( 0,         1,          0),
>              ( sin alpha, 0,  cos alpha)) :: Heading


> rH       :: Scalar -> Heading
> rH alpha  = (( 1,         0,          0),
>              ( 0, cos alpha, -sin alpha),
>              ( 0, sin alpha,  cos alpha)) :: Heading

> forward                              :: Bool -> TurtleStack -> IO TurtleStack
> forward line ((S (x, y, z) ((hx, hy, hz), l, u) p):ss) = do
>   if line
>     then
>       glBegin gl_QUADS >>
>       glVertex3f (x  + ddx) (y  - ddy) z  >>
>       glVertex3f (x  - ddx) (y  + ddy) z  >>
>       glVertex3f (x' - ddx) (y' + ddy) z' >>
>       glVertex3f (x' + ddx) (y' - ddy) z' >>
>       glEnd
>     else
>       glVertex3f x' y' z'
>   return ((S (x', y', z') ((hx, hy, hz), l, u) p) : ss)
>     where x'  = x + hx * delta
>           y'  = y + hy * delta
>           z'  = z + hz * delta
>           ddx = -hy / w      -- ddx and ddy are currently used to give the lines
>           ddy = -hx / w      -- some width, until I figure out gluCylinder
>           w   = width p

> rotateX                              :: Scalar -> TurtleStack -> IO TurtleStack
> rotateX theta ((S pos head p):ss)     = return $ (S pos ((rU theta) `matMult` head) p):ss

> rotateY                              :: Scalar -> TurtleStack -> IO TurtleStack
> rotateY theta ((S pos head p):ss)     = return $ (S pos ((rL theta) `matMult` head) p):ss

> rotateZ                              :: Scalar -> TurtleStack -> IO TurtleStack
> rotateZ theta ((S pos head p):ss)     = return $ (S pos ((rH theta) `matMult` head) p):ss

> turnAround                           :: TurtleStack -> IO TurtleStack
> turnAround ((S pos head p):ss)        = return $ (S pos (rU (d2r 180) `matMult` head) p):ss

> push                                 :: TurtleStack -> IO TurtleStack
> push ss'@((S pos head p):ss)          = return $ (S pos head p) : ss'

> pop                                  :: TurtleStack -> IO TurtleStack
> pop (s:ss)                            = return ss

> decrWidth                            :: TurtleStack -> IO TurtleStack
> decrWidth ((S pos head (Pen t w c cs)):ss)
>                                       = return $ (S pos head (Pen t (w*0.95) c cs)):ss

> setColor           :: Color -> IO ()
> setColor (r, g, b)  = do
>   glColor3f r g b

> nextColor ((S pos head (Pen t w c cs)):ss) = do
>   let c' = (c + 1) `rem` (length cs)
>   setColor (cs !! c')
>   return $ (S pos head (Pen t (w*0.95) c' cs)):ss

> polyBegin                            :: TurtleStack -> IO TurtleStack
> polyBegin ss                          = do
>   glBegin gl_POLYGON >> return ss

> polyEnd                              :: TurtleStack -> IO TurtleStack
> polyEnd ss                            = do
>   glEnd >> return ss


actOn is the dispatcher for incoming turtle commands. To add new
commands, implement the behavior in a function from TurtleStack to
IO TurtleStack, and then add the appropriate command here in the
case statement. It works by transforming the current TurtleStack using
the helper functions above, each of which may modify the top of the
TurtleStack, in addition to performing any drawing actions.

> actOn     :: Char -> IO TurtleStack -> IO TurtleStack
> actOn c s  = do states <- s
>                 let states' = case c of
>                            'F'  -> forward True  states
>                            'f'  -> forward False states
>                            '+'  -> rotateX (theta' states) states
>                            '-'  -> rotateX (-theta' states) states
>                            '&'  -> rotateY (theta' states) states
>                            '^'  -> rotateY (-theta' states) states
>                            '\\' -> rotateZ (theta' states) states
>                            '/'  -> rotateZ (theta' states) states
>                            '|'  -> turnAround states
>                            '['  -> push      states
>                            ']'  -> pop       states
>                            '!'  -> decrWidth states
>                            '{'  -> polyBegin states
>                            '}'  -> polyEnd   states
>                            '\'' -> nextColor states
>                            _    -> return    states
>                 states'
>              where theta' ss = (theta . pen . head) ss


interpret takes a String generated from an LGrammar, as well as
some initial configuration parameters (currently only the default
theta by which to modify the turtle's heading) and using actOn
to modify the initialState accordingly.

> interpret         :: String -> GLfloat -> IO [TurtleState]
> interpret s th     = interpret' s (return $ initialState th 3)
>     where interpret' [] state     = state
>           interpret' (x:xs) state = return (actOn x state)
>                                     >>= interpret' xs


OpenGL Functions
================

These were borrowed rather wholesale from a Haskell port of the very
useful Neon-Helium OpenGL tutorials of yore (where yore = 1999 or so.)
They have been modified and extended slightly to fit my purposes here.


> setupGraphics              :: Int -> Int -> IO ()
> setupGraphics w h = do
>   r <- newIORef 0
>   True <- GLFW.initialize
>   -- select type of display mode:
>   -- Double buffer
>   -- RGBA color
>   -- Alpha components supported
>   -- Depth buffer
>   let dspOpts = GLFW.defaultDisplayOptions
>                 { GLFW.displayOptions_width  = w
>                 , GLFW.displayOptions_height = h
>                 -- Set depth buffering and RGBA colors
>                 , GLFW.displayOptions_numRedBits   = 8
>                 , GLFW.displayOptions_numGreenBits = 8
>                 , GLFW.displayOptions_numBlueBits  = 8
>                 , GLFW.displayOptions_numAlphaBits = 8
>                 , GLFW.displayOptions_numDepthBits = 8
>                 , GLFW.displayOptions_numFsaaSamples = Just 8
>                 -- , GLFW.displayOptions_displayMode = GLFW.Fullscreen
>                 }
>   -- open a window
>   True <- GLFW.openWindow dspOpts
>   -- window starts at upper left corner of the screen
>   GLFW.setWindowPosition 0 0
>   GLFW.setWindowTitle "ls-hs"
>   -- register the function to do all our OpenGL drawing
>   GLFW.setWindowRefreshCallback (drawScene r)
>   -- GLFW.setWindowRefreshCallback (drawScene rt rq)
>   -- register the funciton called when our window is resized
>   GLFW.setWindowSizeCallback resizeScene
>   -- register the function called when the keyboard is pressed.
>   GLFW.setKeyCallback keyPressed
>   GLFW.setWindowCloseCallback shutdown
>   -- initialize our window.
>   initGL
>   forever $ do
>     drawScene r
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


> bush :: LGrammar
> bush  = (Lg "A"
>          [Pr "A" "[&FL!A]/////'[&FL!A]///////'[&FL!A]",
>           Pr "F" "S ///// F",
>           Pr "S" "F L",
>           Pr "L" "['''^^{-f+f+f-|-f+f+f}]"])

> drawScene :: IORef Scalar -> IO ()
> drawScene rtheta = do
>   glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
>                          .|. gl_DEPTH_BUFFER_BIT
>   glLoadIdentity
>   th <- readIORef rtheta
>   glTranslatef (0) (-140) (-150)
>   glRotatef th 0 1 0
>   _ <- interpret (generate bush 7) 22.5
>   writeIORef rtheta $! th + 0.5
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
> resizeScene w h = do
>   glViewport 0 0 (fromIntegral w) (fromIntegral h)
>   glMatrixMode gl_PROJECTION
>   glLoadIdentity
>   gluPerspective 120 (fromIntegral w / fromIntegral h) 0.01 200
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
>   setupGraphics 1000 750