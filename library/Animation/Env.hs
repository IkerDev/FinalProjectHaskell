-- Module which manages the environment of the game

module Animation.Env where

-- With this import we get the user imput to know how to render the environment in function of it

import Animation.Type (UserInput(..))    

-- Environment declaration

data Env =
    Env
        { size               :: (Int, Int)
        , velocity           :: Int
        , baselength         :: Int
        , bricklength        :: Int
        , numOfBricks        :: Int
        , posOfBricks        :: [Int]
        , lifes              :: Int
        }

-- Default environment if we not instanciate it other way
defaultEnv :: Env
defaultEnv =
    Env { size               = (50, 20)
        , velocity           = 1
        , baselength         = 10
        , bricklength        = 3        
        , numOfBricks        = 0
        , posOfBricks        = []
        , lifes              = 1
        }