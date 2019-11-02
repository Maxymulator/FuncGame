module Controller where 
    
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

import Model

{-
proposed game loop order: 
1) get input
2) update all movement (player, bullets, enemies)
3) check collision
4) remove dead objects
-}

{- Movement-}
-- Update all movement in the world
worldUpdateMovement :: World -> World
worldUpdateMovement (World player enemyList bulletList) = World movedPlayer movedEnemies movedBullets
  where
    movedPlayer :: Player
    movedPlayer = movePlayer player
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
movePlayer :: Player -> Player
movePlayer = undefined

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

{- Collision handling -}
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
collidePlayer p es bs = checkPwithE (checkPwithB p bs) es
  where
    checkPwithB :: Player -> [Bullet] -> Player
    checkPwithB p []                                                      = p
    checkPwithB p ((Bullet PlayerBullet _ _ _ _) : xs)                    = checkPwithB p xs
    checkPwithB p (x:xs)                               | collidesWith p x = checkPwithB (damage p (getDamage x)) xs
                                                       | otherwise        = checkPwithB p xs
    checkPwithE :: Player -> [Enemy] -> Player
    checkPwithE p [] = p
    checkPwithE p (x:xs) | collidesWith p x = checkPwithE (damage p damageOnEnemyCollision) xs
                         | otherwise = checkPwithE p xs

-- Collide the enemies
collideEnemies :: Player -> [Enemy] -> [Bullet] -> [Enemy]
collideEnemies p es bs = map ((checkEwithP p) . (checkEwithB bs)) es
  where
    checkEwithP :: Player -> Enemy -> Enemy 
    checkEwithP p e | collidesWith e p = kill e
                    | otherwise        = e
    checkEwithB :: [Bullet] -> Enemy -> Enemy
    checkEwithB [] e                                                   = e
    checkEwithB ((Bullet EnemyBullet _ _ _ _) : xs)                    = checkEwithB e xs
    checkEwithB (x : xs) e                          | collidesWith e x = checkEwithB (damage e (getDamage x)) xs
                                                    | otherwise        = checkEwithB e xs

-- Collide the bullets
collideBullets :: Player -> [Enemy] -> [Bullet] -> [Bullet]
collideBullets p es bs = map ((checkBwithP p) . (checkBwithE es)) bs
  where
    checkBwithP :: Player -> Bullet -> Bullet
    checkBwithP p b@(Bullet EnemyBullet _ _ _ _) | collidesWith b e = kill b
                                                 | otherwise        = b 
    checkBwithP _ b = b
    checkBwithE :: [Enemy] -> Bullet -> Bullet
    checkBwithE [] b                                                        = b
    checkBwithE (x : xs) b@(Bullet PlayerBullet _ _ _ _) | collidesWith b x = kill b
                                                         | otherwise        = checkBwithE xs b
    checkBwithE (x : xs) b                                                  = checkBwithE xs b


-- check if two objects collide                        
doesCollide :: Movable a => a -> a -> Bool
doesCollide o1 o2  | actualDistance > maxDistance = False
                   | otherwise = True
  where
    actualDistance :: Float
    actualDistance = distanceBetweenPoints (getLocation o1) (getLocation o2)
    maxDistance :: Float
    maxDistance = (getRadius o1) + (getRadius o2)

--damageHandler :: Damagable a => a -> Damage -> a