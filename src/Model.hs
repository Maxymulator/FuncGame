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

-- The direction the object is moving in
type Direction = Vector

-- The score of the player
type Score = Int

-- The amout of /seconds/ since the game began
type Time = Float

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
data Enemy = Enemy {getType :: EnemyType, getStats :: EnemyStats}

-- The bullet
data BulletType = EnemyBullet | PlayerBullet
data Bullet = Bullet { getBulletType :: BulletType
                     , getLocation :: Location
                     , getSpeed :: Speed
                     , getDirection :: Direction
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

data HealthState = Alive | Dead

--Type classes
class Movable a where
    move :: Vector -> a -> a 

class Renderable a where
    getPicture :: Picture


class Damageable a where
    damage :: a -> Damage -> a
    getHealthState :: a -> HealthState

-- Instances 

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
    move v (Bullet bt l s dir d) = Bullet bt (move v l) s dir d

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

-- basic enemy types