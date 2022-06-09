-- module which manages the state of the game
module Animation.State where

-- With this standard library of haskell  we have strict state monads, 
-- and we import the get which Fetch the current value of the state within the monad.
-- also we import the put s  which sets the state within the monad to s.

import Control.Monad.Trans.State.Strict (get, put)

-- With this standar library of haskell we have ReaderT monad transformer, 
-- which adds a static environment to a given monad.
-- and we import ask which Fetch the value of the environment.

import Control.Monad.Trans.Reader (ask)

-- this is standad class of monad transformers.
-- A monad transformer makes a new monad out of an existing monad, such that computations 
-- of the old monad may be embedded in the new one. 
-- and we import lift which lift a computation from the argument monad to the constructed monad.

import Control.Monad.Trans.Class (lift)

{-
This is standad IO library
We import:
1. Hready

Computation hReady hdl indicates whether at least one item is available for input from handle hdl.

TO HAVE IN MIND: THIS OPERATION COULD FAIL

isEOFError if the end of file has been reached.

2. Handle

Haskell defines operations to read and write characters from and to files, 
represented by values of type Handle. 
Each value of this type is a handle: a record used by the Haskell run-time system to manage I/O 
with file system objects. A handle has at least the following properties:

    | whether it manages input or output or both;
    | whether it is open, closed or semi-closed;
    | whether the object is seekable;
    | whether buffering is disabled, or enabled on a line or block basis;
    | a buffer (whose length may be zero).

Most handles will also have a current I/O position indicating where the next input or 
output operation will occur. A handle is readable if it manages only input or both input 
and output; likewise, it is writable if it manages only output or both input and output. 
A handle is open when first allocated. Once it is closed it can no longer be used for either 
input or output, though an implementation cannot re-use its storage while references remain to it. 
Handles are in the Show and Eq classes. The string produced by showing a handle is system dependent; 
it should include enough information to identify the handle for debugging. A handle is equal according
to == only to itself; no attempt is made to compare the internal state of different handles for 
equality.

TO HAVE IN MIND:
A Handle will be automatically closed when the garbage collector detects that it has become 
unreferenced by the program, however we should not rely on this because it is unpredictable
3. Stdin

Three handles are allocated during program initialisation, and are initially open.
Here we have stdin, but exist also stdout and stderr

4. hSetEcho

Set the echoing status of a handle connected to a terminal.


5. hSetBuffering
Computation hSetBuffering hdl mode sets the mode of buffering for handle hdl on 
subsequent reads and writes.

If the buffer mode is changed from BlockBuffering or LineBuffering to NoBuffering, then

if hdl is writable, the buffer is flushed as for hFlush;
if hdl is not writable, the contents of the buffer is discarded.

TO HAVE IN MIND: THIS COULD FAIL WITH
isPermissionError if the handle has already been used for reading or writing and the implementation 
does not allow the buffering mode to be changed.

6. BufferMode(NoBuffering)

We set the buffer mode to NoBuffering
buffering is disabled if possible.

It also exists
LineBuffering : line-buffering should be enabled if possible.

BlockBuffering (Maybe Int)	block-buffering should be enabled if possible. The size of the 
buffer is n items if the argument is Just n and is otherwise implementation-dependent.
-}
import System.IO (hReady, Handle(..), stdin, hSetEcho, hSetBuffering, BufferMode(NoBuffering))

-- We import the environment
import Animation.Env (Env(..))

-- We import the internal types and datatypes of the game
import Animation.Type ( Animation
                      , Brick(..)
                      , GameStatus(..)
                      , UserInput(..)
                      )

-- We define the direction 
data Direction
    = Positive
    | Negative
    | Neutral

--  Converts Int to direction
directionFromInt :: Int -> Direction
directionFromInt 0 = Neutral
directionFromInt 1 = Positive
directionFromInt 2 = Negative
directionFromInt _ = error "Error"

-- Converts the direction to the multiplier if needed
directionToMultiplier :: Direction -> Int
directionToMultiplier Positive =  1
directionToMultiplier Negative = -1
directionToMultiplier Neutral  =  0

-- We define the datatype St 
data St =
    St
        { position     :: (Int, Int)
        , direction    :: (Direction, Direction)
        , bXPosition   :: Int
        , bricks       :: [Brick]
        , points       :: Int
        , status       :: GameStatus
        }

-- | Allocation of the list of reduced positions in the game
-- | A reduced position is a 'x' value divided by the brick length
-- | Positions in this function are a list of 'x positions'. This means that
-- | given width = 50 then positions 49, 50, 51, 52,... correspond to points 
-- | (49,0), (50,0), (1,1), (2,1),...

bricksInPlace :: Int -> [Int] -> Int -> Int -> [Brick]
bricksInPlace width positions life bricklength = map (\x -> Brick (findPosition (bricklength*x) width 0) life) positions
           where findPosition x width level = if x < width then (x,level) else findPosition (x - width) width (level + 1)

-- We define the default st if we dont instanciate it other way
defaultSt :: St
defaultSt = St (0, 0) (Neutral, Neutral) 0 [] 0 Stopped         

-- | Management of the input of the user in a handy way.

-- Get the key from the keyboard (helper function of getUserInput)
getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

-- Gets the user input using the buffering from IO Library
getUserInput :: IO (Maybe UserInput)
getUserInput = go Nothing
        where go a = do
                hSetBuffering stdin NoBuffering 
                hSetEcho stdin False
                ready <- hReady stdin
                if not ready then return Nothing
                else do
                      hSetBuffering stdin NoBuffering
                      string <- getKey
                      let condition = (==) (head string) in
                             if condition 'a' || condition 'A'                  then return (Just MoveLeft)
                        else if condition 'd' || condition 'D'                  then return (Just MoveRight)
                        else if condition 'p' || condition 'P'                  then return (Just Pause)
                        else if condition 'q' || condition 'Q'                  then return (Just Stop)
                        else if condition 's' || condition 'S'                  then return (Just Start)
                        else if condition 'r' || condition 'R' || condition ' ' then return (Just Restart)
                        else return Nothing

-- Get next action
next :: Animation Env St ()
next = do
    env    <- ask
    input  <- lift $ lift getUserInput
    prevSt <- lift get
    lift (put (nextInternal env input prevSt))

-- Helper function of next
nextInternal :: Env -> Maybe UserInput -> St -> St
nextInternal (Env (width, height) velocity baselength bricklength _ _ _ ) 
             userInput 
             prevSt@(St (prevX, prevY) (prevXDir, prevYDir) prevBXPos prevBricks prevPoints prevStatus)
             =

 -- If we add some other UserInput of GameStatus we should make changes here   
 -- | Management of next state according to GameStatus and UserInput
   
    case prevStatus of
        Paused        -> case userInput of
                           Just Start -> prevSt {status = Playing}
                           Just Stop  -> prevSt {status = Stopped}
                           _          -> prevSt
        Stopped       -> case userInput of
                           Just Restart -> prevSt {status = Restarted}
                           _            -> prevSt
        LevelComplete -> case userInput of
                           Just Restart -> prevSt {status = Restarted}
                           _            -> prevSt
        Playing       -> if prevBricks /= [] then
                            case userInput of
                               Just Stop     -> prevSt {status = Stopped}
                               Just Pause    -> prevSt {status = Paused }
                               Just MoveLeft -> 
                                 St 
                                   { position   = (newX, newY)
                                   , direction  = (newXDir, newYDir)
                                   , bXPosition = newBXPosition (-2)
                                   , bricks     = newBricks
                                   , points     = newPoints
                                   , status     = newStatus
                                   }
                               Just MoveRight -> 
                                 St 
                                   { position   = (newX, newY)
                                   , direction  = (newXDir, newYDir)
                                   , bXPosition = newBXPosition (2)
                                   , bricks     = newBricks
                                   , points     = newPoints
                                   , status     = newStatus
                                   }
                               _              -> 
                                 St 
                                   { position   = (newX, newY)
                                   , direction  = (newXDir, newYDir)
                                   , bXPosition = prevBXPos
                                   , bricks     = newBricks
                                   , points     = newPoints
                                   , status     = newStatus
                                   }
                         else prevSt {status = LevelComplete }

    where

 -- | New_Unbounded tells us which would be the position of the ball if there is no bound (Very handy)
   
    newXUnbounded          = prevX + directionToMultiplier prevXDir * velocity
    newYUnbounded          = prevY + directionToMultiplier prevYDir * velocity

 -- | Detection of collision with the base
    
    baseCollision          = prevBXPos <= newX && prevBXPos + baselength >= newX
                                               && newYUnbounded >= height - 2
 
 -- | Auxiliary functions to consider the length of a brick, not just their position
 -- | completePositions returns a list of occupied positions given a list of Bricks
    
    addPositions (u,v) brl = zip [u .. (u + brl - 1)] $ take brl $ repeat v
    completePositions      = foldl (\x y -> x ++ addPositions (brickPosition y) bricklength) []
    
 -- | Identification of the coordinate that will be impacted according to ball direction for three 
 -- | different cases: Collision with top or botton (brickCollisionY), collision with one side (brickCollisionX)   
 -- | or collision with a corner (cornerCollision)
    
    targetX                = ( newXUnbounded + directionToMultiplier prevXDir, newY)
    targetY                = ( newX, newYUnbounded + directionToMultiplier prevYDir)
    cornerTarget           = ( newXUnbounded + directionToMultiplier prevXDir
                             , newYUnbounded + directionToMultiplier prevYDir )
    brickCollisionY        = elem targetY $ completePositions prevBricks
    brickCollisionX        = elem targetX $ completePositions prevBricks
    cornerCollision        = not brickCollisionX && not brickCollisionY 
                           && elem cornerTarget (completePositions prevBricks)
    
 -- | Identification of the block that will be hit.
    
    targetBlockY           = identify targetY      prevBricks
    targetBlockX           = identify targetX      prevBricks
    targetBlockC           = identify cornerTarget prevBricks

 -- | Filters the only brick that is hit by the ball given a target position and a list of Bricks.
    
    identify target        = head . filter (\u -> snd target == snd (brickPosition u) 
                                               && fst target -  fst (brickPosition u) < bricklength 
                                               && fst target -  fst (brickPosition u) >= 0          )
    
 -- | Update positions and directions for next state
    
    newX =
        case prevXDir of
            Neutral  ->     newXUnbounded
            Positive -> min newXUnbounded width
            Negative -> max newXUnbounded 0
    newY =
        case prevYDir of
            Neutral  ->     newYUnbounded
            Positive -> min newYUnbounded height
            Negative -> max newYUnbounded 0
    newXDir =
        case prevXDir of
            Neutral  -> Neutral
            Positive ->
                if newXUnbounded >= width  || brickCollisionX || cornerCollision
                then Negative
                else Positive
            Negative ->
                if newXUnbounded <= 0      || brickCollisionX || cornerCollision
                then Positive
                else Negative
    newYDir =
        case prevYDir of
            Neutral  -> Neutral
            Positive -> if brickCollisionY || cornerCollision || baseCollision
                            then Negative
                            else Positive
            Negative ->
                if newYUnbounded <= 0      || brickCollisionY || cornerCollision
                then Positive
                else Negative
    
 -- | Position control of the base limited by the width
    
    newBXPosition i = let newBxPos = prevBXPos + i
                       in if newBxPos + baselength > width
                          then prevBXPos
                          else if newBxPos <= 0
                               then 0
                               else newBxPos
    
 -- | Update status in case the player is unable to bounce back the ball
    
    newStatus = if newY /= height then Playing else Stopped
 
 -- | Update the score in case of any brick collision 
    
    newPoints = (+) prevPoints $ fromEnum $ brickCollisionY || brickCollisionX || cornerCollision
    
 -- | Update the bricks state according to collisions. Brick disappears if life = 0
    
    newBricks   -- | Case 1: Collision in Y axis AND X axis (Two bricks at the same time)
              
              | brickCollisionX && brickCollisionY 
                                = changeBricks targetBlockY $ changeBricks targetBlockX prevBricks
             
                -- | Case 2: Collision in Y axis
              
              | brickCollisionY = changeBricks targetBlockY prevBricks
             
                -- | Case 3: Collision in X axis
             
              | brickCollisionX = changeBricks targetBlockX prevBricks
             
                -- | Case 4: Collision with a corner
             
              | cornerCollision = changeBricks targetBlockC prevBricks
             
                -- | Case 5: No collision
             
              | otherwise = prevBricks
 
 -- | Update of the life of the bricks
    
    changeBricks x bricks = let brickTail  = filter ((/=) (brickPosition x) . brickPosition) bricks
                                brickHurt  = Brick (brickPosition x) (life x - 1)
                             in if life x > 0 then brickHurt : brickTail else brickTail