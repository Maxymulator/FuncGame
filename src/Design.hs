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

data Player = Player {
      Health
    , Location
    , Upgrades
}

data EnemyStats = Stats {
      Health
    , Location
    , Speed
    , ShootBound
}
data Enemy = Standard EnemyStats | Boss EnemyStats

data BulletType = EnemyBullet | PlayerBullet
data Bullet = Bullet {
      BulletType
    , Location
    , Speed
    , Direction
    , Damage :: Int
}

data World = World {
      Player
    , [Enemy]
    , [Bullet]
}

data GameState = GameState {
      Score
    , Time
    , World
}

-- Voorbeeld functies
getStndEnemy :: Score -> Enemy
getStndEnemy score = Standard (Stats h l s sb)
  where
    h = 10 * (score / 100) + 1
    l = Point 1.0 5.0 -- TODO: Make randomised
    s = 3.0
    sb = 5.0

getBossEnemy :: Score -> Enemy
getBossEnemy = Boss (Stats h l s sb)
  where
    h = 30 * (score/100) + 3
    l = Point 1.0 5.0
    s = 1.0
    sb = 10.0

