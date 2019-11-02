{-# LANGUAGE TypeSynonymInstances #-}

module Model where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as GArithmetic

-- Health of an object
type Health = Int

-- Damage of an object
type Damage = Int

-- Location of an object
type Location = Point

-- Speed of an object
type Speed = Float

-- Shooting bound of an enemy (use as: enemy.Y + bound and enemy.Y - bound)
type ShootBound = Float

-- Damage multiplier of the player
type Upgrades = Float

-- The score of the player
type Score = Int

-- The amout of /seconds/ since the game began
type Time = Float

-- The radius of an object
type Radius = Float

-- the possible movement directions
data Direction = N | NE | E | SE | S | SW | W | NW

-- The player
data Player = Player { getPHealth :: Health
                     , getPLocation :: Location
                     , getPSpeed :: Speed
                     , getUpgrades :: Upgrades
                     , getPRadius :: Radius
}

-- The enemy
-- The data stored in an enemy
data EnemyStats = Stats { getEHealth :: Health
                        , getELocation :: Location
                        , getESpeed :: Speed
                        , getShootBound :: ShootBound
                        , getERadius :: Radius
}
-- The type of an enemy
data EnemyType = Standard | Boss
-- The constructor of the enemy
data Enemy = Enemy { getEnemyType :: EnemyType
                   , getStats :: EnemyStats}

-- The bullet
data BulletType = EnemyBullet | PlayerBullet
data Bullet = Bullet { getBulletType :: BulletType
                     , getBLocation :: Location
                     , getVector :: Vector
                     , getDamage :: Damage
                     , getRadius :: Radius
}

-- The physical world, aka the playing field
data World = World { getPlayer :: Player
                   , getEnemyList :: [Enemy]
                   , getBulletList :: [Bullet]
}

-- The gamestate storing all the data.
data GameState = GameState { getScore :: Score
                           , getTime :: Time
                           , getWorld :: World
}

-- A datastructure defining if an object is either living or dead
data HealthState = Alive | Dead

{- Type classes -}
class Movable a where
    move :: Vector -> a -> a
    getLocation :: a -> Location
    getSpeed :: a -> Speed
    getRadius :: a -> Radius

class Renderable a where
    getPicture :: a -> Picture


class Damageable a where
    damage :: a -> Damage -> a
    kill :: a -> a
    getHealth :: a -> Health
    getHealthState :: a -> HealthState

{- Instances -}
-- Movable instances    
instance Movable Player where
    move v (Player h l s u r) = Player h ((GArithmetic.+) v l) s u r
    getLocation (Player _ l _ _ _) = l
    getSpeed (Player _ _ s _ _) = s
    getRadius (Player _ _ _ _ r) = r

instance Movable EnemyStats where
    move v (Stats h l s sb r) = Stats h ((GArithmetic.+) v l) s sb r
    getLocation (Stats _ l _ _ _) = l
    getSpeed (Stats _ _ s _ _) = s
    getRadius (Stats _ _ _ _ r) = r

instance Movable Enemy where
    move v (Enemy t s) = Enemy t (move v s)
    getLocation (Enemy _ s) = getLocation s
    getSpeed (Enemy _ s) = getSpeed s
    getRadius (Enemy _ s) = getRadius s

instance Movable Bullet where
    move v (Bullet bt l vec d r) = Bullet bt ((GArithmetic.+) v l) vec d r
    getLocation (Bullet _ l _ _ _) = l
    getSpeed _ = 3
    getRadius (Bullet _ _ _ _ r) = r

-- Renderable instances
instance Renderable World where
    getPicture (World pl es bs) = pl1 <> es2 <> bs2
      where 
      pl1 :: Player -> Picture
      pl1 (Player _ l _ _) = Color white (Translate x y (Circle 10.0))
        where (x, y) = l
      es1 :: Enemy -> Picture
      es1 (Enemy t s) | t == Boss   = Color black (Translate x y (Circle 15.0))
                      | otherwise   = Color black (Translate x y (Circle 10.0))
            where (h,l,s,sh) = getStats s
                  (x,y)     = l
      es2 :: [Enemy] -> Picture
      es2 = mconcat . map es1 es
      bs1 :: Bullet -> Picture
      bs1 (Bullet b l _ _ _) | b == EnemyBullet  = Color black (Translate x y (Circle 1.0))
                             | otherwise         = Color white (Translate x y (Circle 1.0))
            where (x,y)     = l
      bs2 :: [Bullet] -> Picture
      bs2 = mconcat . map bs1 bs

instance Renderable Score where
    getPicture s = Translate (-150.0) 150.0 (Text show s)

instance Renderable Time where 
    getPicture t = Translate   150.0 150.0 (Text show t)

-- Damageable instance
instance Damageable Player where
    damage (Player h l s u r) dam = Player (h - dam) l s u r
    kill (Player _ l s u r)       = Player 0 l s u r
    getHealth (Player h _ _ _ _)  = h
    getHealthState p              | getHealth p <= 0 = Dead
                                  | otherwise         = Alive

instance Damageable EnemyStats where
    damage (Stats h l s sb r) dam = Stats (h - dam) l s sb r
    kill (Stats _ l s sb r)       = Stats 0 l s sb r
    getHealth (Stats h _ _ _ _)   = h
    getHealthState s              | getHealth s <= 0 = Dead
                                  | otherwise         = Alive

instance Damageable Enemy where
    damage (Enemy t s) dam     = Enemy t (damage s dam)
    kill (Enemy t s)           = Enemy t (kill s)
    getHealth (Enemy _ s)      = getHealth s
    getHealthState (Enemy _ s) = getHealthState s

{- initial values for types -}
-- initial player
initialPlayer :: Player
initialPlayer = Player h l s u r
  where
    h :: Health
    h = 10
    l :: Location
    l = (-150, 0)
    s :: Speed
    s = 1
    u :: Upgrades
    u = 0.0
    r :: Radius
    r = playerCircleRadius

-- initial world
initialWorld :: World
initialWorld = World initialPlayer [] []

-- initial gamestate
initialGameState :: GameState
initialGameState = GameState 0 0 initialWorld

-- neutral vector
neutralVector :: Vector
neutralVector = (0, 0)

{- make object functions -}
-- make a boss enemy at the specified location with specified health
makeBossEnemy :: Health -> Location -> Enemy
makeBossEnemy h l = Enemy t s
  where
    t :: EnemyType
    t = Boss
    s :: EnemyStats
    s = Stats h l 1 40 enemyBossCircleRadius
    
-- make a standard enemy at the specified location with specified health
makeStandardEnemy :: Health -> Location -> Enemy
makeStandardEnemy h l = Enemy t s
  where
    t :: EnemyType
    t = Standard
    s :: EnemyStats
    s = Stats h l 2 20 enemyStndCircleRadius

-- make a player bullet, moving right
makePlayerBullet :: Location -> Damage -> Bullet
makePlayerBullet l d = Bullet PlayerBullet l (makeVector E 3) d

-- make an enemy bullet, moving left
makeEnemyBullet :: Location -> Damage -> Bullet
makeEnemyBullet l d = Bullet EnemyBullet l (makeVector W 3) d

-- make a vector, given a direction and speed
makeVector :: Direction -> Speed -> Vector
makeVector S s  = (0, -s)
makeVector SE s = (s/2, (-s)/2)
makeVector E s  = (s, 0)
makeVector NE s = (s/2, s/2)
makeVector N s  = (0, s)
makeVector NW s = ((-s)/2,s/2)
makeVector W s  = (-s, 0)
makeVector SW s = ((-s)/2,(-s)/2)

{- Constants -}
-- The radius of the player circle
playerCircleRadius :: Float
playerCircleRadius = 10.0

-- THe radius of the standard enemy circle
enemyStndCircleRadius :: Float
enemyStndCircleRadius = 10.0

-- The radius of the standard enemy circle
enemyBossCircleRadius :: Float
enemyBossCircleRadius = 15.0

-- The radius of the bullet
bulletRadius :: Float
bulletRadius = 1.0

-- Damage when hitting a standard enemy
damageOnEnemyCollision :: Int
damageOnEnemyCollision = 1

{- Calculations -}
-- Calc distance between two points
distanceBetweenPoints :: Point -> Point -> Float
distanceBetweenPoints p1 p2 = magV vector
  where
    vector = (GArithmetic.-) p1 p2