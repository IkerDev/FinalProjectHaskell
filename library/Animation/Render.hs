-- Module which manages the render of the game

module Animation.Render where

-- With this standard library of haskell  we have strict state monads, 
-- and we import the get which Fetch the current value of the state within the monad.

import Control.Monad.Trans.State.Strict (get)

-- With this standar library of haskell we have ReaderT monad transformer, 
-- which adds a static environment to a given monad.
-- and we import ask which Fetch the value of the environment.

import Control.Monad.Trans.Reader (ask)

-- this is standad the class of monad transformers.
-- A monad transformer makes a new monad out of an existing monad, such that computations 
-- of the old monad may be embedded in the new one. 
-- and we import lift which lift a computation from the argument monad to the constructed monad.

import Control.Monad.Trans.Class (lift)

-- We import the environment
import Animation.Env (Env(..))
-- We import the state
import Animation.State (St(..))
-- We import the internal types and datatypes of the game
import Animation.Type (Animation,Object(..),Brick(..), GameStatus(..))

--Render the game using the helpers functions
render :: Animation Env St ()
render = do
    val <- renderVal
    lift (lift (putStrLn val))

--Helper function of render
renderVal :: Animation Env St String
renderVal = do
    env <- ask
    st <- lift get
    return (renderInternal env st)

--Helper function of renderVal
renderInternal :: Env -> St -> String
renderInternal env st = makeBox (size        env) 
                                (baselength  env) 
                                (bXPosition  st ) 
                                (position    st ) 
                                (bricklength env) 
                                (bricks      st ) 
                                (status      st )
                                (points      st )


-- If we would need to render some other things we should change here

-- | Definition of how each line is going to be rendered according to what is there. Options are:
-- | 1) Nothing / 2) Just bricks / 3) Just the ball / 4) Just the base / 5) The ball and bricks /
-- | 6) The ball and the base / (The base and bricks is not an option)

-- Helper Function of make box
makeLine :: Char 
         -> Char 
         -> Int 
         -> Maybe Object 
         -> Maybe Object 
         -> [Brick] 
         -> Int
         -> String
makeLine endChar innerChar i mb mba bricks bricklength =
    let positions = [0 .. i]
        renderPixel x =
            case mb of
                Nothing -> case mba of 
                               Nothing           -> printBlock x
                               Just (Base bl ba) -> if x `elem` [ba..(ba+bl)] then ':' else innerChar
                                 
                Just (Ball b) -> case mba of 
                               Nothing           -> if x == b then 'O' else printBlock x
                               Just (Base bl ba) -> if x == b then 'O' 
                                                    else if x `elem` [ba..(ba+bl)] then ':' 
                                                    else innerChar
                                       
     in [endChar] ++ map renderPixel positions ++ [endChar]
     
     -- | Finding if a brick should be the owner of a pixel considering its position and length
     -- | If True, it paints it according to the life of the brick
     
     where brickXPositions = map (fst . brickPosition) bricks
           printBlock x    = if x `elem` foldl (\u v  -> u ++ [v..(v+bricklength-1)]) [] brickXPositions
                             then if (life $ pixelOwner x) > 0 then '=' else '-'
                             else innerChar
           pixelOwner x    = head $ filter (\u -> x - fst (brickPosition u) < bricklength
                                               && x - fst (brickPosition u) >= 0 ) bricks

-- Render all the game
makeBox :: (Int, Int) 
        -> Int 
        -> Int 
        -> (Int, Int) 
        -> Int 
        -> [Brick] 
        -> GameStatus
        -> Int 
        -> String
makeBox (numCols, numRows) baseL baseX (ballX, ballY) bricklength bricks status points =
    unlines 
        (["            BRICK BREAKER VIDEOGAME"] ++ [" "] 
        ++ case status of 
              LevelComplete -> [celebratrionCartoon]
              _             -> [   makeLine '-' '-' numCols Nothing Nothing [] bricklength ]
                              ++   mappedPositions          
                              ++ [ makeLine '-' '-' numCols Nothing Nothing [] bricklength ]
                              ++ ["Status: " ++ show status
                                 ++ if ballY /= numRows then   " | Score: " ++ show points 
                                    else  " | ***** GAME OVER ***** | Your Score is " ++ show points 
                                 ]
                              ++ -- Render menu according to status
                                 [ case status of
                                     Stopped       -> "Press (R) to Restart"
                                     Paused        -> "Press (S) to Play | Controls: (A) Left / (D) Right"
                                     Playing       -> "(P) Pause / (Q) Stop / (A) Left / (D) Right"
                                     _             -> ""
                                 ]
                           -- | Uncomment these lines for debugging purposes 
--                            ++ [  "BaseX: " ++ show (baseX + div baseL 2) 
--                               ++ " | Ball: (" ++ show ballX ++ "," ++ show ballY ++ ")"
--                               ++ " | BallOverBase: "   ++ show (ballX >= baseX && (ballX <= (baseX + baseL)))
--                               ]
        )
    where   positions = [0 .. numRows]
            mappedPositions = map lineMaker positions

         -- | Painting lines depending on the position of the ball and position of the base given a Y position
            
            lineMaker y =
              let brickYPositions = filter ((==) y . snd . brickPosition) bricks
               in if y == ballY
                    then if y == numRows - 1
                         then makeLine '|' ' ' numCols (Just (Ball ballX)) (Just (Base baseL baseX)) brickYPositions bricklength
                         else makeLine '|' ' ' numCols (Just (Ball ballX)) Nothing                   brickYPositions bricklength
                    else if y == numRows - 1
                         then makeLine '|' ' ' numCols Nothing             (Just (Base baseL baseX)) brickYPositions bricklength
                         else makeLine '|' ' ' numCols Nothing             Nothing                   brickYPositions bricklength
            
            celebratrionCartoon = 
                                    "                        .-."
                                ++"\n                _.--¨¨¨¨.o/         .-.-._"
                                ++"\n             __'   .¨¨¨; {        _J ,__  `.       Level Complete"
                                ++"\n            ; o`.-.`._.'J;       ; /  `- /  ;"
                                ++"\n            `--i`¨. `¨ .';       `._ __.'   |     ¡CONGRATULATIONS!"
                                ++"\n                `  `¨¨¨   `         `;      :"
                                ++"\n                 `.¨-.     ;     ____/     /     Your Score: " ++ show points ++ " points"
                                ++"\n                   `-.`     `-.-'    `¨-..'"
                                ++"\n     ___              `;__.-'¨           `."
                                ++"\n  .-{_  `--._         /.-¨                 `-."
                                ++"\n /    ¨¨T    ¨¨---...'  _.-¨¨   ¨¨¨-.         `."
                                ++"\n;       /                 __.-¨¨.    `.         `,             _.."
                                ++"\n `     /            __.-¨¨       '.    `          `.,__      .'L' }"
                                ++"\n  `---¨`-.__    __.¨    .-.       j     `.         :   `.  .' ,' /"
                                ++"\n            ¨¨¨¨       /   `     :        `.       |     F' `   ;"
                                ++"\n                      ;     `-._,L_,-¨¨-.   `-,    ;     `   ; /"
                                ++"\n                       `.       |        `-._  `.__/_        `/"
                                ++"\n                         `     _;            `  _.'  `-.     /"
                                ++"\n                          `---¨ `.___,,      ;¨¨        `  .'"
                                ++"\n                                    _/       ;           `¨"
                                ++"\n      Bring me                   .-¨     _,-' "
                                ++"\n     more bricks!               {       ¨¨;            Next Level - Press SPACE"
                                ++"\n                                 ;-.____.'`."
                                ++"\n      I am not done yet!          `.  ` '.  :"
                                ++"\n                                    `  : : /"
                                ++"\n                                     `':/ `"
                                