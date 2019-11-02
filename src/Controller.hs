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
    movedEnemies = moveEnemies (getPLocation player) enemyList
    movedBullets :: [Bullet]
    movedBullets = moveBullets bulletList

-- List movement handlers
-- Enemy list movement handler
moveEnemies :: Location -> [Enemy] -> [Enemy]
moveEnemies _ [] = []
moveEnemies l (x@(Enemy Standard _) : xs) = moveStandardEnemy x : moveEnemies l xs
moveEnemies l (x@(Enemy Boss _) : xs) = moveBossEnemy l x : moveEnemies l xs

-- Bullet list movement handler
moveBullets :: [Bullet] -> [Bullet]
moveBullets [] = []
moveBullets xs = map moveBullet xs

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
    vector = makeVector W (getESpeed (getStats e))

-- Boss Enemy movement, follows the player
moveBossEnemy :: Location -> Enemy -> Enemy
moveBossEnemy l e = move (vector l (getELocation (getStats e))) e
  where
    vector :: Location -> Location -> Vector
    vector (_, yp) (_, yb) | yp < yb   = makeVector S (getESpeed (getStats e))
                           | yp > yb   = makeVector N (getESpeed (getStats e))
                           | otherwise = neutralVector

{-Spawn Enemies-}
spawnEnemy :: GameState -> EnemyType -> GameState
spawnEnemy gameState enemy = (score, time, newWorld, newGen)
        where 
          (score, time, world, gen) = gameState
          newWorld                  = (getPlayer world, newEnemyList, getBulletList world)
          newEnemyList              = emenyNew : getEnemyList world
          enemyNew                  = makeAnEnemy enemy
          healthBoss                = 30 * (score / 100) + 3
          healthStandard            = 10 * (score / 100) + 1
          randLocation              = (175, fst(next gen))
          newGen                    = snd(next gen)
          makeAnEnemy :: EnemyType -> Enemy
          makeAnEnemy enemy | enemy == Boss   = makeBossEnemy     healthBoss     (175.0, 0)
                            | otherwise       = makeStandardEnemy healthStandard randLocation