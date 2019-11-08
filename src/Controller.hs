module Controller where 
    
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game 
import System.Random
import System.Exit
import Safe

import Model

{-
proposed game loop order: 
1) get input
2) update all movement (player, bullets, enemies)
3) check collision
4) remove dead objects
-}
{- Iteration handling -}
step :: Float -> GameState -> IO GameState
step secs gs@(GameState _ _ _ _ Running) = return $ pureGameLoop gs { getTime = getTime gs + secs}
step _ gs@(GameState _ _ _ _ Paused)     = return gs       
step _ gs@(GameState _ _ _ _ GameOver)   = return gs

{- Input handling -}
input :: Event -> GameState -> IO GameState
-- handle IO related key presses in the IO part
input e@(EventKey (Char c) Down _ _) gstate | c == quitKey = saveGame gstate >>= exitGame
                                            | c == loadKey = loadGame
                                            | otherwise    = return (inputKey e gstate)
-- handle pure input key presses in the pure part
input e gstate = return (inputKey e gstate)

-- Handle key inputs which result in pure computations
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gs@(GameState _ _ _ _ Paused) | c == unPauseKey = gs {getState = Running}
                                                                    | otherwise       = gs
inputKey (EventKey (Char c) Down _ _) gs@(GameState _ _ _ _ Running) | c == upKey     = movePlayer (makeVector N (playerSpeed gs)) gs
                                                                     | c == downKey   = movePlayer (makeVector S (playerSpeed gs)) gs
                                                                     | c == shootKey  = playerShoots gs
                                                                     | c == pauseKey  = gs {getState = Paused}
                                                                     | otherwise      = gs
  where
    playerSpeed :: GameState -> Speed
    playerSpeed = getSpeed . getPlayer . getWorld
inputKey _ gs = gs

{- Saving and loading the game -}
-- Save the gamestate to "save.txt"
saveGame :: GameState -> IO ()
saveGame gs = writeFile "save.txt" (show gs) -- Save the current gamestate to a file

-- Load the game safely, if the "safe.txt" file doesnt contain a nice gamestate, it will return the initialGameState instead
loadGame :: IO GameState
loadGame = do
    contents <- readFile "save.txt"
    let gs = readDef initialGameState contents
    return gs

{- Exiting the game -}
-- Exit the game safely
exitGame :: () -> IO GameState -- type definition to make it sequentiable with the savegame feature, such that a bind forces computation of both.
exitGame _ = exitSuccess 

{- Pure game loop -}
-- Run the pure part of the code
pureGameLoop :: GameState -> GameState
pureGameLoop = handleCleanup . handleCollision . handleMovement . handleSpawning . handleShooting

{- Movement handling -}
-- Moment handler for the gamestate
handleMovement :: GameState -> GameState
handleMovement (GameState s t world g rs) = GameState s t (worldUpdateMovement world) g rs

{- Enemy behaviour -}
-- When an enemy will spawn
handleSpawning :: GameState -> GameState
handleSpawning (GameState s t world g rs) | round t `mod` 11 == 0  = spawnEnemy (GameState s t world g rs) Standard
                                          | (s+1) `mod` 1001 == 0  = spawnEnemy (GameState (s+1) t world g rs) Boss
                                          | otherwise              = (GameState s t world g rs)
                                  
                                          

-- When an enemy will shoot
handleShooting :: GameState -> GameState
handleShooting (GameState s t world g rs) | round t `mod` 6 == 0  = enemyShoots (GameState s t world g rs)
                                          | otherwise             = (GameState s t world g rs)

-- Update all movement in the world
worldUpdateMovement :: World -> World
worldUpdateMovement (World player enemyList bulletList) = World player movedEnemies movedBullets
  where
    movedEnemies :: [Enemy]
    movedEnemies = moveEnemies (getLocation player) enemyList
    movedBullets :: [Bullet]
    movedBullets = map moveBullet bulletList

-- List movement handlers
-- Enemy list movement handler
moveEnemies :: Location -> [Enemy] -> [Enemy]
moveEnemies _ [] = []
moveEnemies l (x@(Enemy Standard _) : xs) = moveStandardEnemy x : moveEnemies l xs
moveEnemies l (x@(Enemy Boss _) : xs)     = moveBossEnemy l x : moveEnemies l xs

-- Individual object movement handlers
-- Player movement
movePlayer :: Vector -> GameState -> GameState
movePlayer v gs@(GameState t s (World player es bs) g st) | playerY < upperLimit && playerY > lowerLimit = GameState t s (World (move v player) es bs) g st
                                                          | playerY >= upperLimit = GameState t s (World (player {getPLocation = (playerX, upperLimit - 0.1)}) es bs) g st
                                                          | playerY <= lowerLimit = GameState t s (World (player {getPLocation = (playerX, lowerLimit + 0.1)}) es bs) g st
                                                          | otherwise                       = gs
   where
    playerY :: Float
    (playerX, playerY) = getLocation player
    upperLimit :: Float
    upperLimit = windowHeight / 2 - getSpeed player
    lowerLimit :: Float
    lowerLimit = - (windowHeight / 2 - getSpeed player)

-- Bullet movement, moves in its own direction at a constant pace
moveBullet :: Bullet -> Bullet
moveBullet b = move vector b
  where
    vector :: Vector
    vector = getVector b

-- Standard Enemy movement, moves left at a constant pace
moveStandardEnemy :: Enemy -> Enemy
moveStandardEnemy e = move vector e
  where
    vector :: Vector
    vector = makeVector W (getSpeed e)

-- Boss Enemy movement, follows the player
moveBossEnemy :: Location -> Enemy -> Enemy
moveBossEnemy l e = move (vector l (getLocation e)) e
  where
    vector :: Location -> Location -> Vector
    vector (_, yp) (_, yb) | yp < yb   = makeVector S $ getSpeed e
                           | yp > yb   = makeVector N $ getSpeed e
                           | otherwise = neutralVector

{-Spawn Enemies/Create Bullets-}
-- Spawn a single enemy of the given type
spawnEnemy :: GameState -> EnemyType -> GameState
spawnEnemy (GameState score time world gen rs) eType = GameState score time (newWorld world) newGen rs
  where
    newWorld :: World -> World
    newWorld (World p es bs) = World p (newEnemy eType : es) bs
    nextGen :: (Int, StdGen)
    nextGen = next gen
    newGen :: StdGen
    newGen = snd nextGen
    newEnemy :: EnemyType -> Enemy
    newEnemy Boss = makeBossEnemy healthBoss (windowWidth * 0.5 * 0.9, 0)
      where
        healthBoss :: Health
        healthBoss = 20 * (score `div` 100) + 3
    newEnemy Standard = makeStandardEnemy healthStandard randLocation
      where
        healthStandard :: Health
        healthStandard = 10 * (score `div` 100) + 1
        randLocation :: Location
        randLocation = (windowWidth * 0.5 * 0.95, 0)

--Enemy bullets
enemyShoots :: GameState -> GameState
enemyShoots (GameState score time world gen rs) = GameState score time (newWorld world) gen rs
  where
    newWorld :: World -> World
    newWorld (World p es bs) = World p es (addedBullets ++ bs)
    createBullet :: Enemy -> Bullet
    createBullet enemy = makeEnemyBullet (getLocation enemy) 1
    addedBullets :: [Bullet]
    addedBullets = map createBullet (getEnemyList world)

--Player shooting
playerShoots :: GameState -> GameState
playerShoots (GameState score time world gen rs) = GameState score time (newWorld world) gen rs
  where
    newWorld :: World -> World
    newWorld (World p es bs) = World p es (createBullet p : bs)
    createBullet :: Player -> Bullet
    createBullet p = makePlayerBullet (getLocation p) (playerDamage p)
    playerDamage :: Player -> Damage 
    playerDamage p = round (1 * getUpgrades p)

{- Collision handling -}
-- The gamestate collision handler
handleCollision :: GameState -> GameState
handleCollision (GameState s t world g rs) = GameState s t (worldUpdateCollision world) g rs

-- Update the world with all the collisions taken care of
worldUpdateCollision :: World -> World
worldUpdateCollision (World player enemyList bulletList) = World updatedPlayer updatedEnemies updatedBullets
  where
    updatedPlayer :: Player
    updatedPlayer = collidePlayer player enemyList bulletList
    updatedEnemies :: [Enemy]
    updatedEnemies = collideEnemies player enemyList bulletList
    updatedBullets :: [Bullet]
    updatedBullets = collideBullets player enemyList bulletList

-- Collide the player    
collidePlayer :: Player -> [Enemy] -> [Bullet] -> Player
collidePlayer pl es bs = checkPwithE (checkPwithB pl bs) es
  where
    checkPwithB :: Player -> [Bullet] -> Player
    checkPwithB p []                                     = p
    checkPwithB p (Bullet PlayerBullet _ _ _ _ _ : xs)   = checkPwithB p xs
    checkPwithB p (x:xs) | doesCollide p x               = checkPwithB (damage p (getDamage x)) xs
                         | otherwise                     = checkPwithB p xs
    checkPwithE :: Player -> [Enemy] -> Player
    checkPwithE p [] = p
    checkPwithE p (x:xs) | doesCollide p x = checkPwithE (damage p damageOnEnemyCollision) xs
                         | otherwise = checkPwithE p xs

-- Collide the enemies
collideEnemies :: Player -> [Enemy] -> [Bullet] -> [Enemy]
collideEnemies pl es bs = map (checkEwithP pl . checkEwithB bs) es
  where
    checkEwithP :: Player -> Enemy -> Enemy 
    checkEwithP p e | doesCollide e p = kill e
                    | otherwise        = e
    checkEwithB :: [Bullet] -> Enemy -> Enemy
    checkEwithB [] e                                    = e
    checkEwithB (Bullet EnemyBullet _ _ _ _ _ : xs) e = checkEwithB xs e
    checkEwithB (x : xs) e | doesCollide e x            = checkEwithB xs (damage e (getDamage x))
                           | otherwise                  = checkEwithB xs e

-- Collide the bullets
collideBullets :: Player -> [Enemy] -> [Bullet] -> [Bullet]
collideBullets pl es = map $ checkBwithP pl . checkBwithE es
  where
    checkBwithP :: Player -> Bullet -> Bullet
    checkBwithP p b@(Bullet EnemyBullet _ _ _ _ _) | doesCollide b p = kill b
                                                   | otherwise       = b 
    checkBwithP _ b = b
    checkBwithE :: [Enemy] -> Bullet -> Bullet
    checkBwithE [] b                                                         = b
    checkBwithE (x : xs) b@(Bullet PlayerBullet _ _ _ _ _) | doesCollide b x = kill b
                                                           | otherwise       = checkBwithE xs b
    checkBwithE (_ : xs) b@(Bullet EnemyBullet _ _ _ _ _)                    = checkBwithE xs b

-- check if two objects collide                        
doesCollide :: (Movable a, Movable b) => a -> b -> Bool
doesCollide o1 o2  | actualDistance > maxDistance = False
                   | otherwise = True
  where
    actualDistance :: Float
    actualDistance = distanceBetweenPoints (getLocation o1) (getLocation o2)
    maxDistance :: Float
    maxDistance = getRadius o1 + getRadius o2

{- Cleanup: Object removal -}
-- The cleanup handler for the gamestate
handleCleanup :: GameState -> GameState
handleCleanup (GameState initScore t initWorld g _) = GameState (updatedScore initWorld) t (worldUpdateCleanup initWorld) g $ updatedRS $ playerHealthState initWorld
  where
    updatedScore :: World -> Score
    updatedScore (World _ es _) = scoreCounter (initScore, es)
    playerHealthState :: World -> HealthState
    playerHealthState (World p _ _) = getHealthState p
    updatedRS :: HealthState -> RunningState
    updatedRS Alive = Running
    updatedRS Dead  = GameOver
        
-- Handle the cleanup in the world
worldUpdateCleanup :: World -> World
worldUpdateCleanup (World p es bs) = World p (listCleaner es) (listCleaner bs)

-- Remove all dead objects from a given list
listCleaner :: Damageable a => [a] -> [a]
listCleaner [] = []
listCleaner (x : xs) | getHealthState x == Alive = x : listCleaner xs
                     | otherwise                 = listCleaner xs

-- Count the score earned in the current state of the game and accumulate it
scoreCounter :: (Score, [Enemy]) -> Score
scoreCounter (n, [])                                                    = n
scoreCounter (n, x@(Enemy Standard _) : xs) | getHealthState x == Alive = scoreCounter (n, xs)
                                            | otherwise                 = scoreCounter (n + 25, xs)
scoreCounter (n, x@(Enemy Boss _) : xs) | getHealthState x == Alive     = scoreCounter (n, xs)
                                        | otherwise                     = scoreCounter (n + 100, xs)