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
type Upgrades = Int

-- The direction the object is moving in
type Direction = Vector

-- The score of the player
type Score = Int

-- The amout of /seconds/ since the game began
type Time = Int

Instance Eq Point where
  (x1, y1) == (x2, y2) = x1 == x2 && y1 == y2
  (x1, y1) /= (x2, y2) = not (x1, y1) == (x2, y2)

-- The player
data Player = Player {
      Health
    , Location
    , Upgrades
}

-- The data stored in an enemy
data EnemyStats = Stats {
      Health
    , Location
    , Speed
    , ShootBound
}
-- The enemy
data Enemy = Standard EnemyStats | Boss EnemyStats

-- The bullet
data BulletType = EnemyBullet | PlayerBullet
data Bullet = Bullet {
      BulletType
    , Location
    , Speed
    , Direction
    , Damage :: Int
}

-- The physical world, aka the playing field
data World = World {
      Player
    , [Enemy]
    , [Bullet]
}

-- The gamestate storing all the data.
data GameState = GameState {
      Score
    , Time
    , World
}