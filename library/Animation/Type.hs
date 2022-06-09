-- Module which manages internal types and datatypes of the game

module Animation.Type where

-- With this standar library of haskell we have ReaderT monad transformer, 
-- which adds a static environment to a given monad.
-- and we import ReaderT which is the reader monad transformer, 
-- which adds a read-only environment to the given monad.
-- TO HAVE IN MIND
-- 1. the return function ignores the environment
-- 2. >>= passes the inherited environment to both subcomputations.

import Control.Monad.Trans.Reader (ReaderT(..))

-- With this standard library of haskell  we have strict state monads, 
-- and we import the constructor StateT (state transformer monad)
-- TO HAVE IN MIND
-- 1. the return function leaves the state unchanged
-- 2. >>= uses the final state of the first computation as the initial state of the second one

-- and also the evalStateT which Evaluates a state computation with the given initial state  
-- and return the final value,discarding the final state.

import Control.Monad.Trans.State.Strict (StateT(..), evalStateT)

-- We declare the type Animation which takes the action in the environment
type Animation env st a = ReaderT env (StateT st IO) a

-- We declare the datatype Object with 2 constructors 
-- 1. Ball 
-- 2. Base 
data Object = Ball Int
            | Base Int Int

-- We declare the datatype Object with 1 constructor
-- 1. Brick which have the position of the brick with a pair of ints (x and y in 2D)
-- and life which it the life of the bricks 
-- (the times we need to touch with our ball the brick in question)
-- derive eq to know if a brick should be the owner of a pixel considering its position and length
-- better to understand this looking and render

data Brick =
  Brick
    { brickPosition :: (Int, Int)
    , life          :: Int
    }
  deriving Eq

--  We declare the datatype GameStatus, which it we say in which status of the game we are 
-- derive show to monstrate it on terminal 
data GameStatus = Paused
                | Playing
                | Stopped
                | LevelComplete
                | Restarted
                deriving (Show)

--  We declare the datatype UserInput, to associate the key of the keyboard we have push with userInput
data UserInput = MoveLeft
               | MoveRight
               | Pause
               | Stop
               | Start
               | Restart

-- The function which runs the animation
-- takes the environment, the state, and the action (type animation) and return an IO a
-- IO Have side effects
runAnimation :: env -> st -> Animation env st a -> IO a
runAnimation env st action = evalStateT (runReaderT action env) st