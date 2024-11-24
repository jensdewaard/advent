module Common.Vector2 (Vector2(..), vector2, plus, minus, scale, angle, angleI, normalize, rotate, manhattan) where
import GHC.Float (float2Int, int2Float)

data Vector2 a = Vector2 {
    x :: a,
    y :: a
} deriving (Eq, Show)

instance RealFloat a => Semigroup (Vector2 a) where
  (<>) :: Vector2 a -> Vector2 a -> Vector2 a
  (<>) = plus

instance RealFloat a => Monoid (Vector2 a) where
  mempty :: Vector2 a
  mempty = vector2 0 0

vector2 :: a -> a -> Vector2 a
vector2 = Vector2

plus :: RealFloat a => Vector2 a -> Vector2 a -> Vector2 a
plus (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (x1 + x2) (y1 + y2)

minus :: RealFloat a => Vector2 a -> Vector2 a -> Vector2 a
minus (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (x1 - x2) (y1 - y2)

scale :: RealFloat a => Int -> Vector2 a -> Vector2 a
scale n v = Vector2 (scaleFloat n (x v)) (scaleFloat n (y v))

manhattan :: Num a => Vector2 a -> Vector2 a -> a
manhattan v w = abs (x w - x v) + abs (y w - y v)

angle :: RealFloat a => Vector2 a -> a
angle (Vector2 vx vy) = atan2 vy vx

angleI :: Integral a => Vector2 a -> Float
angleI (Vector2 vx vy) = atan2 (int2Float $ fromIntegral vy) (int2Float $ fromIntegral vx)

vector2Polar :: RealFloat a => a -> a -> Vector2 a
vector2Polar rho theta = Vector2 (rho * cos theta) (rho * sin theta)

vector2Rho :: RealFloat a => Vector2 a -> a
vector2Rho (Vector2 vx vy) = sqrt (vx * vx + vy * vy)

vector2Theta :: RealFloat a => Vector2 a -> a
vector2Theta (Vector2 vx vy) = atan2 vy vx

rotate :: RealFloat a => a -> Vector2 a -> Vector2 a
rotate a v = vector2Polar (vector2Rho v) (vector2Theta v + a)

normalize :: (RealFloat a) => Vector2 a -> Vector2 Float
normalize (Vector2 0 0) = Vector2 0 0
normalize v = let 
    vx :: Int
    vx = fromInteger $ floor $ x v
    vy :: Int
    vy = fromInteger $ floor $ y v
    g = gcd vx vy
    in Vector2 (int2Float $ vx `div` g) (int2Float $ vy `div` g)
