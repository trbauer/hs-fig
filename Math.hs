module Math where

import Data.List
import Text.Printf


data Vec3 = Vec3 !Float !Float !Float
  deriving (Show,Eq,Ord,Read)
fmtVec3 :: Vec3 -> String
fmtVec3 v = "<" ++ intercalate "," (map (printf "% 4.3f") (vecToList v)) ++ ">"

infixl 6 .+
(.+) :: Vec v => v -> v -> v
(.+) = vecZip (+)
infixl 6 .-
(.-) :: Vec v => v -> v -> v
(.-) = vecZip (-)
infixl 7 .*
(.*) :: Vec v => Float -> v -> v
(.*) s = vecMap (s*)
infixl 7 ./
(./) :: Vec v => v  -> Float -> v
(./) v s = vecMap (/s) v
len2 :: Vec v => v -> Float
len2 = foldl' (\a x -> a + x*x) 0 . vecToList
len :: Vec v => v -> Float
len = sqrt . len2
dist2 :: Vec v => v -> v -> Float
dist2 v1 v2 = len2 (v2 .- v1)
dist :: Vec v => v -> v -> Float
dist v1 v2 = len (v2 .- v1)
cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 a1 a2 a3) (Vec3 b1 b2 b3) = Vec3 c1 c2 c3
  where c1 = a2*b3 - a3*b2
        c2 = a3*b1 - a1*b3
        c3 = a1*b2 - a2*b1
norm :: Vec v => v -> v
norm v = v ./ len v


class Vec v where
  vecMap :: (Float -> Float) -> v -> v
  vecZip :: (Float -> Float -> Float) -> v -> v -> v
  vecToList :: v -> [Float]
  vecFromList :: [Float] -> v

instance Vec Vec3 where
  vecMap op (Vec3 x1 y1 z1) = (Vec3 (op x1) (op y1) (op z1))
  vecZip op (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
    Vec3 (x1`op`x2) (y1`op`y2) (z1`op`z2)
  vecToList (Vec3 x1 x2 x3) = [x1,x2,x3]
  vecFromList [x1,x2,x3] = (Vec3 x1 x2 x3)


-- toF :: Integral i => i -> Float
-- toF = fromIntegral
class (Ord c, Num c) => Coord c where
  toF :: c -> Float
instance Coord Float where
  toF = id
instance Coord Int where
  toF = fromIntegral


data Mat4x4 =
  Mat4x4 {
    m00 :: !Float, m01 :: !Float, m02 :: !Float, m03 :: !Float
  , m10 :: !Float, m11 :: !Float, m12 :: !Float, m13 :: !Float
  , m20 :: !Float, m21 :: !Float, m22 :: !Float, m23 :: !Float
  , m30 :: !Float, m31 :: !Float, m32 :: !Float, m33 :: !Float
  } deriving (Eq,Show)

fromListMat4x4 :: [Float] -> Mat4x4
fromListMat4x4 fs =
    Mat4x4
      m00 m01 m02 m03
      m10 m11 m12 m13
      m20 m21 m22 m23
      m30 m31 m32 m33
  where
  [m00,m01,m02,m03,
   m10,m11,m12,m13,
   m20,m21,m22,m23,
   m30,m31,m32,m33] = fs

fmtMat4x4 :: Mat4x4 -> String
fmtMat4x4 = fmtMat4x4' "" "%5.2f"

fmtMat4x4' :: String -> String -> Mat4x4 -> String
fmtMat4x4' ind fmt m =
  concatMap fmtRow [
      [m00,m01,m02,m03],
      [m10,m11,m12,m13],
      [m20,m21,m22,m23],
      [m30,m31,m32,m33]]
  where fmtRow fcs = ind ++ intercalate "  " (map (printf fmt . ($m)) fcs) ++ "\n"


translateMat4x4 :: Vec3 -> Mat4x4
translateMat4x4 (Vec3 x y z) =
  Mat4x4
    0 0 0 x
    0 0 0 y
    0 0 0 z
    0 0 0 1

scaleMat4x4 :: Vec3 -> Mat4x4
scaleMat4x4 (Vec3 x y z) =
  Mat4x4
    x 0 0 0
    0 y 0 0
    0 0 z 0
    0 0 0 1

addMat4x4 :: Mat4x4 -> Mat4x4 -> Mat4x4
addMat4x4 mA mB =
    Mat4x4
      (f m00) (f m01) (f m02) (f m03)
      (f m10) (f m11) (f m12) (f m13)
      (f m20) (f m21) (f m22) (f m23)
      (f m30) (f m31) (f m32) (f m33)
  where f g = g mA + g mB

mulMat4x4 :: Mat4x4 -> Mat4x4 -> Mat4x4
mulMat4x4 mA mB =
    Mat4x4
      (dot row0 col0) (dot row0 col1) (dot row0 col2) (dot row0 col3)
      (dot row1 col0) (dot row1 col1) (dot row1 col2) (dot row1 col3)
      (dot row2 col0) (dot row2 col1) (dot row2 col2) (dot row2 col3)
      (dot row3 col0) (dot row3 col1) (dot row3 col2) (dot row3 col3)
  where dot :: [(Mat4x4 -> Float)] -> [(Mat4x4 -> Float)] -> Float
        dot fas fbs = sum $ zipWith (\fa fb -> fa mA * fb mB) fas fbs
        row0 = [m00,m01,m02,m03]
        row1 = [m10,m11,m12,m13]
        row2 = [m20,m21,m22,m23]
        row3 = [m30,m31,m32,m33]
        col0 = [m00,m10,m20,m30]
        col1 = [m01,m11,m21,m31]
        col2 = [m02,m12,m22,m32]
        col3 = [m03,m13,m23,m33]

