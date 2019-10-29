module Model where

 -- point defines location
type Point = (Float, Float)

f :: Point -> a
f (x, y) = x

-- vector defines point manipulation
type Vector = (Float, Float)

-- Health of an object
type Health = Int

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

instance Eq Point where
  (x1, y1) == (x2, y2) = x1 == x2 && y1 == y2
  (x1, y1) /= (x2, y2) = not (x1, y1) == (x2, y2)

-- The player
data Player = Player { getHealth :: Health
                     , getLocation :: Location
                     , getSpeed :: Speed
                     , getUpgrades :: Upgrades
}

-- The data stored in an enemy
data EnemyStats = Stats { getHealth :: Health
                        , getLocation :: Location
                        , getSpeed :: Speed
                        , getShootBound :: ShootBound
}
-- The enemy
data Enemy = Standard { getStats :: EnemyStats} | Boss { getStats :: EnemyStats}

-- The bullet
data BulletType = EnemyBullet | PlayerBullet
data Bullet = Bullet { getBulletType :: BulletType
                     , getLocation :: Location
                     , getSpeed :: Speed
                     , getDirection :: Direction
                     , getDamage :: Int
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
--Type classes
class Movable where
    move :: Vector -> a -> a 

class Renderable where
    undefined

class Killable where
    undefined

-- Instances 
instance Movable Point where
    move (Vector vx vy) (Point px py) = Point (px + vx) (py + vy)

instance Movable Player where
    move v (Player h l s u) = Player h (move v l) s u

instance Movable Enemy where
    move v (Enemy (Stats h l s sb)) = Enemy (Stats h (move v l) s sb)

instance Movable Bullet where
    move v (Bullet bt l s dir d) = Bullet bt (move v l) s dir d
-- basic enemy types

moveObject :: Movable a => a -> Vector -> a