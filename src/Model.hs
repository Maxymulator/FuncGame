{-# LANGUAGE TypeSynonymInstances #-}

module Model where

import Graphics.Gloss
import System.Random
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
data Direction = N | NE | E | SE | S | SW | W | NW deriving (Eq)

-- The player
data Player = Player { getPHealth :: Health
                     , getPLocation :: Location
                     , getPSpeed :: Speed
                     , getUpgrades :: Upgrades
                     , getPRadius :: Radius
} deriving (Read, Show)

-- The enemy
-- The data stored in an enemy
data EnemyStats = Stats { getEHealth :: Health
                        , getELocation :: Location
                        , getESpeed :: Speed
                        , getShootBound :: ShootBound
                        , getERadius :: Radius
} deriving (Read, Show)
-- The type of an enemy
data EnemyType = Standard | Boss deriving (Eq, Read, Show)
-- The constructor of the enemy
data Enemy = Enemy { getEnemyType :: EnemyType
                   , getStats :: EnemyStats
} deriving (Read, Show)

-- The bullet
data BulletType = EnemyBullet | PlayerBullet deriving (Eq, Read, Show)
data Bullet = Bullet { getBulletType :: BulletType
                     , getBLocation :: Location
                     , getVector :: Vector
                     , getDamage :: Damage
                     , getBRadius :: Radius
                     , getBHealth :: Health
} deriving (Read, Show)

-- The physical world, aka the playing field
data World = World { getPlayer :: Player
                   , getEnemyList :: [Enemy]
                   , getBulletList :: [Bullet]
} deriving (Read, Show)

-- The runningState defines what is currently happening in the game, if its paused, being played, or game over
data RunningState = Running | Paused | GameOver deriving (Eq, Read, Show)
-- The gamestate storing all the data.
data GameState = GameState { getScore :: Score
                           , getTime :: Time
                           , getWorld :: World
                           , getGen :: StdGen
                           , getState :: RunningState
} deriving (Read, Show)

-- A datastructure defining if an object is either living or dead
data HealthState = Alive | Dead deriving (Eq, Read, Show)

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
    move v (Bullet bt l vec d r h) = Bullet bt ((GArithmetic.+) v l) vec d r h
    getLocation (Bullet _ l _ _ _ _) = l
    getSpeed _ = 3
    getRadius (Bullet _ _ _ _ r _) = r

-- Renderable instances
instance Renderable World where
    getPicture (World pl es bs) = getPicture pl <> renderEnemies es <> renderBullets bs
      where 
      renderEnemies :: [Enemy] -> Picture
      renderEnemies = mconcat . map getPicture
      renderBullets :: [Bullet] -> Picture
      renderBullets = mconcat . map getPicture

instance Renderable Player where
    getPicture p = Color white $ Translate x y $ circleSolid $ getRadius p
      where 
        x :: Float
        y :: Float
        (x, y) = getLocation p

instance Renderable Enemy where
    getPicture e = Color (getColor (getEnemyType e)) $ Translate x y $ circleSolid $ getRadius e
      where
        x :: Float
        y :: Float
        (x, y) = getLocation e
        getColor :: EnemyType -> Color
        getColor Standard = black
        getColor Boss     = red
            
instance Renderable Bullet where 
    getPicture b = Color (getColor (getBulletType b)) $ Translate x y $ circleSolid $ getRadius b
      where
        x :: Float
        y :: Float
        (x, y) = getLocation b
        getColor :: BulletType -> Color
        getColor EnemyBullet  = black
        getColor PlayerBullet = white

instance Renderable Score where
    getPicture s = Translate (-windowWidth * 0.5 * 0.8) (windowHeight * 0.5 * 0.7) $ Text $ show s

instance Renderable Time where 
    getPicture t = Translate (windowWidth * 0.5 * 0.65) (windowHeight * 0.5 * 0.7) $ Text $ show time
      where
        time :: Int
        time = round t

-- Damageable instance
instance Damageable Player where
    damage (Player h l s u r) dam       = Player (h - dam) l s u r
    kill (Player _ l s u r)             = Player 0 l s u r
    getHealth                           = getPHealth
    getHealthState p | getHealth p <= 0 = Dead
                     | otherwise        = Alive

instance Damageable EnemyStats where
    damage (Stats h l s sb r) dam       = Stats (h - dam) l s sb r
    kill (Stats _ l s sb r)             = Stats 0 l s sb r
    getHealth                           = getEHealth
    getHealthState s | getHealth s <= 0 = Dead
                     | otherwise        = Alive

instance Damageable Enemy where
    damage (Enemy t s) dam     = Enemy t (damage s dam)
    kill (Enemy t s)           = Enemy t (kill s)
    getHealth (Enemy _ s)      = getHealth s
    getHealthState (Enemy _ s) = getHealthState s

instance Damageable Bullet where
    damage (Bullet bt l vec d r h) dam = Bullet bt l vec d r (h - dam)
    kill (Bullet bt l vec d r _) = Bullet bt l vec d r 0
    getHealth                           = getBHealth
    getHealthState b | getHealth b <= 0 = Dead
                     | otherwise        = Alive

{- initial values for types -}
-- initial player
initialPlayer :: Player
initialPlayer = Player h l s u r
  where
    h :: Health
    h = 10
    l :: Location
    l = (-windowWidth * 0.5 * 0.9, 0)
    s :: Speed
    s = 10
    u :: Upgrades
    u = 0.0
    r :: Radius
    r = playerCircleRadius

-- initial world
initialWorld :: World
initialWorld = World initialPlayer [] []

initialGen :: StdGen
initialGen = mkStdGen 5

-- initial gamestate
initialGameState :: GameState
initialGameState = GameState 0 0 initialWorld initialGen Running

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
makePlayerBullet l d = Bullet PlayerBullet l (makeVector E 3) d bulletRadius 1

-- make an enemy bullet, moving left
makeEnemyBullet :: Location -> Damage -> Bullet
makeEnemyBullet l d = Bullet EnemyBullet l (makeVector W 3) d bulletRadius 1

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
playerCircleRadius = 30.0

-- THe radius of the standard enemy circle
enemyStndCircleRadius :: Float
enemyStndCircleRadius = 20.0

-- The radius of the standard enemy circle
enemyBossCircleRadius :: Float
enemyBossCircleRadius = 40.0

-- The radius of the bullet
bulletRadius :: Float
bulletRadius = 8.0

-- Damage when hitting a standard enemy
damageOnEnemyCollision :: Int
damageOnEnemyCollision = 1

-- The key to move up
upKey :: Char
upKey = 'w'

-- The key to move down
downKey :: Char
downKey = 's'

-- The key to shoot
shootKey :: Char
shootKey = 'c'

-- The key to pause the game
pauseKey :: Char
pauseKey = 'p'

-- The key to unpause the game
unPauseKey :: Char
unPauseKey = 'o'

-- The key to save and quit the game
quitKey :: Char
quitKey = 'q'

-- The key to load the previous save
loadKey :: Char
loadKey = 'l'

-- The size of the window
windowWidth :: Float
windowWidth = 1280.0

windowHeight :: Float
windowHeight = 720.0

{- Calculations -}
-- Calc distance between two points
distanceBetweenPoints :: Point -> Point -> Float
distanceBetweenPoints p1 p2 = magV vector
  where
    vector :: Vector
    vector = (GArithmetic.-) p1 p2