module Model where

import Graphics.Gloss

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
data Direction = Left | Right | Up | Down

-- The player
data Player = Player { getHealth :: Health
                     , getLocation :: Location
                     , getSpeed :: Speed
                     , getUpgrades :: Upgrades
}

-- The enemy
-- The data stored in an enemy
data EnemyStats = Stats { getHealth :: Health
                        , getLocation :: Location
                        , getSpeed :: Speed
                        , getShootBound :: ShootBound
}
-- The type of an enemy
data EnemyType = Standard | Boss
-- The constructor of the enemy
data Enemy = Enemy { getType :: EnemyType
                   , getStats :: EnemyStats}

-- The bullet
data BulletType = EnemyBullet | PlayerBullet
data Bullet = Bullet { getBulletType :: BulletType
                     , getLocation :: Location
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
    getPicture :: Picture

class Damageable a where
    damage :: a -> Damage -> a
    getHealthState :: a -> HealthState

{- Instances -}
-- Movable instances    
instance Movable Point where
    move v p = v + p

instance Movable Player where
    move v (Player h l s u) = Player h (move v l) s u

instance Movable EnemyStats where
    move v (Stats h l s sb) = Stats h (move v l) s sb
    
instance Movable Enemy where
    move v (Enemy t s) = Enemy t (move v s)

instance Movable Bullet where
    move v (Bullet bt l vec d) = Bullet bt (move v l) vec d

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
    getHealthState p            | getHealth p <= 0 = Dead
                                | otherwise        = Alive

instance Damageable EnemyType where
    damage (Stats h l s sb) dam = Stats (h - dam) l s sb
    getHealthState s            | getHealth s <= 0 = Dead
                                | otherwise        = Alive

instance Damageable Enemy where
    damage (Enemy t s) dam     = Enemy t (damage s dam)
    getHealthState (Enemy t s) = getHealthState s

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

{- get object functions -}
-- get a boss enemy at the specified location with specified health
getBossEnemy :: Health -> Location -> Enemy
getBossEnemy h l = Enemy t s
  where
    t :: EnemyType
    t = Boss
    s :: EnemyStats
    s = Stats h l 1 40
    
-- get a standard enemy at the specified location with specified health
getStandardEnemy :: Health -> Location -> Enemy
getStandardEnemy h l = Enemy t s
  where
    t :: EnemyType
    t = Standard
    s :: EnemyStats
    s = Stats h l 2 20

-- get a player bullet, moving right
getPlayerBullet :: Location -> Damage -> Bullet
getPlayerBullet l d = Bullet PlayerBullet l (getVector Right 3) d

-- get an enemy bullet, moving left
getEnemyBullet :: Location -> Damage -> Bullet
getEnemyBullet l d = Bullet EnemyBullet l (getVector Left 3) d

-- get a vector, given a direction and speed
getVector :: Direction -> Speed -> Vector
getVector Left s  = (-s, 0)
getVector Right s = (s, 0)
getVector Up s    = (0, -s)
getVector Down s  = (0, s)