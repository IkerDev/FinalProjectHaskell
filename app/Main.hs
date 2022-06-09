-- To use the game on VS in windows
-- 1. Install VS
-- 2. Instal GHC VIA GHCUP https://www.haskell.org/ghcup/
-- 3. Install extension HASKELL
-- 4. Open Folder with the project
-- 4. Open terminal
-- 4. Cabal build
-- 5. Cabal RUN

-- Module which controls all the app
module Main where

-- We import the different datatypes and types of the game
import Animation
    ( Animation
    , Direction(..)
    , Env(..)
    , St(..)
    , defaultEnv
    , defaultSt
    , next
    , render
    , runAnimation
    , directionFromInt
    , bricksInPlace
    )

-- A common interface to a collection of useful concurrency abstractions.
-- we import threadDelay which Suspends the current thread for a given number of microseconds (GHC only).
-- TO HAVE IN MIND:
-- There is no guarantee that the thread will be rescheduled promptly when the delay has expired, 
--but the thread will never continue to run earlier than specified.
import Control.Concurrent (threadDelay)


--This module provides type classes and instances for the following concepts:
-- Pure pseudo-random number generators (RandomGen, stdGen, splitMix)
-- we import randomRio A variant of randomRM that uses the global 
-- pseudo-random number generator globalStdGen 

import System.Random (randomRIO)

-- This module provides the List Standar of Haskell
-- We import nubby behaves just like nub(*), except it uses a 
-- user-supplied equality predicate instead of the overloaded == function.
--(*) The nub function removes duplicate elements from a list. In particular, 
-- it keeps only the first occurrence of each element. 

import Data.List (nubBy)

-- With this standard library of haskell  we have strict state monads, 
-- and we import the get which Fetch the current value of the state within the monad.
-- also we import the put s  which sets the state within the monad to s.

import Control.Monad.Trans.State.Strict (put, get)

-- With this standar library of haskell we have ReaderT monad transformer, 
-- which adds a static environment to a given monad.
-- and we import ask which Fetch the value of the environment.

import Control.Monad.Trans.Reader (ask)

-- this is standad the class of monad transformers.
-- A monad transformer makes a new monad out of an existing monad, such that computations 
-- of the old monad may be embedded in the new one. 
-- and we import lift which lift a computation from the argument monad to the constructed monad.

import Control.Monad.Trans.Class (lift)

-- We import the GameStatus

import Animation.Type (GameStatus(..))


-- We instanciate an initial state without it we would instanciate default st (helper function of main animation)
putInitialState :: Animation Env St ()
putInitialState = do
    (Env (width, height) _ baselength bricklength _ _ lifes) <- ask
    posX <- lift $ lift $ randomRIO (div width  3, (*) 2 $ div width  3)
    posY <- lift $ lift $ randomRIO (div height 3, (*) 2 $ div height 3)
    dirX <- lift $ lift $ fmap directionFromInt $ randomRIO (1, 2)
    dirY <- lift $ lift $ fmap directionFromInt $ randomRIO (1, 2)
  
 -- | Creation of a random number of blocks limited by a desired maximum number.
  
    randNumBlocks  <- let maxDesiredBlocks = div (width * (height - 4)) (bricklength * 4) in lift $ lift $ randomRIO (0, maxDesiredBlocks)
  
 -- | Creation of a list of DIFFERENT positions. The range of available positions has to be divided by the bricklength so
 -- | we can introduce the bricklength space afterwards in order to get bricks not to overlap
  
    randRedDistBlocks <- (fmap (nubBy (==)) $ sequence $ replicate randNumBlocks $ randomRIO (1, div (width * (height - 4)) bricklength :: Int))
        
    lift $ put $ St (posX, posY) (dirX, dirY) (div (width - baselength) 2) (bricksInPlace width randRedDistBlocks lifes bricklength) 0 Paused
  
  
 -- | Management of the animation. Restarted state interrupts it. (helper function of mainAnimation)
animate :: Animation Env St ()
animate = do
    render
    event <- lift get
    case (status event) of
        Restarted -> putInitialState
        _         -> next
    lift $ lift $ threadDelay 500000
    animate

-- Helper function of main
mainAnimation :: Animation Env St ()
mainAnimation = do
    putInitialState
    animate

-- Generate the game
main :: IO ()
main = do
    runAnimation defaultEnv defaultSt mainAnimation
  
  

