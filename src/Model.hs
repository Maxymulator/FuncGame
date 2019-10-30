{-# LANGUAGE TypeSynonymInstances #-}

module Model where

import Graphics.Gloss
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

-- the possible movement directions
data Direction = N | NE | E | SE | S | SW | W | NW

-- The player
data Player = Player { getPHealth :: Health
                     , getPLocation :: Location
                     , getPSpeed :: Speed
                     , getUpgrades :: Upgrades
}

-- The enemy
-- The data stored in an enemy
data EnemyStats = Stats { getEHealth :: Health
                        , getELocation :: Location
                        , getESpeed :: Speed
                        , getShootBound :: ShootBound
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

class Renderable a where
    getPicture :: a -> Picture

class Damageable a where
    damage :: a -> Damage -> a
    getHealthState :: a -> HealthState

{- Instances -}
-- Movable instances    
--instance Movable Point where
--    move v p = v + p

instance Movable Player where
    move v (Player h l s u) = Player h ((GArithmetic.+) v l) s u

instance Movable EnemyStats where
    move v (Stats h l s sb) = Stats h ((GArithmetic.+) v l) s sb
    
instance Movable Enemy where
    move v (Enemy t s) = Enemy t (move v s)

instance Movable Bullet where
    move v (Bullet bt l vec d) = Bullet bt ((GArithmetic.+) v l) vec d

-- Renderable instances
instance Renderable World where
    getPicture = undefined

instance Renderable Score where
    getPicture = undefined

instance Renderable Time where 
    getPicture = undefined 

-- Damageable instance
instance Damageable Player where
    damage (Player h l s u) dam = Player (h - dam) l s u
    getHealthState p            | getPHealth p <= 0 = Dead
                                | otherwise         = Alive

instance Damageable EnemyStats where
    damage (Stats h l s sb) dam = Stats (h - dam) l s sb
    getHealthState s            | getEHealth s <= 0 = Dead
                                | otherwise         = Alive

instance Damageable Enemy where
    damage (Enemy t s) dam     = Enemy t (damage s dam)
    getHealthState (Enemy _ s) = getHealthState s

{- initial values for types -}
-- initial player
initialPlayer :: Player
initialPlayer = Player h l s u
  where
    h :: Health
    h = 10
    l :: Location
    l = (-150, 0)
    s :: Speed
    s = 1
    u :: Upgrades
    u = 0.0

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
    s = Stats h l 1 40
    
-- make a standard enemy at the specified location with specified health
makeStandardEnemy :: Health -> Location -> Enemy
makeStandardEnemy h l = Enemy t s
  where
    t :: EnemyType
    t = Standard
    s :: EnemyStats
    s = Stats h l 2 20

-- make a player bullet, moving right
makePlayerBullet :: Location -> Damage -> Bullet
makePlayerBullet l d = Bullet PlayerBullet l (makeVector E 3) d

-- make an enemy bullet, moving left
makeEnemyBullet :: Location -> Damage -> Bullet
makeEnemyBullet l d = Bullet EnemyBullet l (makeVector W 3) d

-- make a vector, given a direction and speed
makeVector :: Direction -> Speed -> Vector
makeVector N s  = (0, -s)
makeVector NE s = (s/2, (-s)/2)
makeVector E s  = (s, 0)
makeVector SE s = (s/2, s/2)
makeVector S s  = (0, s)
makeVector SW s = ((-s)/2,s/2)
makeVector W s  = (-s, 0)
makeVector NW s = ((-s)/2,(-s)/2)