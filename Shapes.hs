module Shapes where

import Geom
import Math

import Graphics.UI.GLUT
import qualified Graphics.UI.GLUT as GLUT

import Control.Applicative
import Control.Monad
import Data.Array
import Data.Char
import Data.List
import Data.IORef
import System.Random
import Text.Printf
import qualified Data.Map.Strict as DM
import qualified Data.IntMap.Strict as DIM
import qualified Control.Monad.State.Strict as ST


skeleton_root :: Bone
-- skeleton_root = knee
skeleton_root = knee{bChildren = []}
-- skeleton_root = left_calf{bChildren = []}
-- skeleton_root = left_foot

skeleton_bone_names :: [String]
skeleton_bone_names = map bName (enumBones skeleton_root)

lookupBone :: String -> Bone
lookupBone bnm =
  case filter ((==bnm) . bName) (enumBones skeleton_root) of
    (b:_) -> b

updBone :: String -> (Bone -> Bone) -> Bone -> Bone
updBone b_nm f = upd
  where upd b
          | b_nm == bName b = f b
          | otherwise = b{bChildren = map (\(t,bc) -> (t,upd bc)) (bChildren b)}

enumBones :: Bone -> [Bone]
enumBones b = b:concatMap (enumBones . snd) (bChildren b)



renderHierarchy :: String -> Bone -> IO ()
renderHierarchy sel = renderBone (Vec3 0 0 0)
  where renderBone (Vec3 tx ty tz) b = preservingMatrix $ do
          translate (Vector3 tx ty tz)
          let Vec3 rx ry rz = bOrientation b
          rotate rz (Vector3 0 0 1)
          rotate ry (Vector3 0 1 0)
          rotate rx (Vector3 1 0 0)
          when (bName b == sel) $ do
              renderAxes
              color3f 1 0 0
          mImmediateRender True (bMesh b)
          when (bName b == sel) $
              color3f 1 1 1
          mapM_ (uncurry renderBone) (bChildren b)


renderAxes :: IO ()
renderAxes = renderAxesSized 1 0xAAAA

renderAxesSized :: GLfloat -> GLushort -> IO ()
renderAxesSized s stipple = body
  where body = preservingAttrib attrs $ do
          lighting $= Disabled
          renderPrimitive Lines $ do
            color3f 1 0 0
            vertex3f 0 0 0
            vertex3f s 0 0
            color3f 0 1 0
            vertex3f 0 0 0
            vertex3f 0 s 0
            color3f 0 0 1
            vertex3f 0 0 0
            vertex3f 0 0 s
          lineStipple $= Just (10,stipple)
          renderPrimitive Lines $ do
            color3f 1 0 0
            vertex3f 0 0 0
            vertex3f (-s) 0 0
            color3f 0 1 0
            vertex3f 0 0 0
            vertex3f 0 (-s) 0
            color3f 0 0 1
            vertex3f 0 0 0
            vertex3f 0 0 (-s)
          lineStipple $= Nothing
          return ()

        attrs =
          [
            CurrentAttributes -- color, normal, texture, raster (https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPushAttrib.xml)
          , EnableAttributes
          , LightingAttributes
          ]

data Bone =
  Bone {
    bName :: !String
  , bOrientation :: !Vec3 -- applied before rendering children
  , bMesh :: !Mesh
  , bChildren :: ![(Vec3,Bone)] -- translate applied before each child
  } deriving (Show,Eq)


fmtBone :: Bone -> String
fmtBone = fmtBone' ""
fmtBone' :: String -> Bone -> String
fmtBone' ind b  =
    ind ++ "Bone " ++ bName b ++ "\n" ++
    ind ++ "  bOrientation = " ++ fmtVec3 (bOrientation b) ++ "\n" ++
    ind ++ "  bMesh = " ++ mFormat (bMesh b) ++ "\n" ++
    -- concatMap (\strs -> "\n" ++ concatMap (\v-> ind ++ "  " ++ fmtVec3 v ++ "\n") strs) (mStrips (bMesh b)) ++ "\n" ++
    ind ++ "  bChildren =" ++ children
  where children
          | null (bChildren b) = " []\n"
          | otherwise = "\n" ++ concatMap fmtBone (bChildren b)
          where fmtBone (t,cb) = ind ++ "  " ++ fmtVec3 t ++ " -> " ++ bName cb ++ "\n"

knee :: Bone
knee =
  Bone {
    bName = "knee"
  , bOrientation = Vec3 0 0 0
  , bMesh = m_knee
  , bChildren =
      [
        (Vec3 0 (-0.15) 0,left_calf)
      ]
  }

left_calf :: Bone
left_calf =
  Bone {
    bName = "left calf"
  , bOrientation = Vec3 0 0 0
  , bMesh = m_leg
  , bChildren =
      [
        (Vec3 0 (-1.5) 0,left_foot)
      ]
  }

left_foot :: Bone
left_foot =
  Bone {
    bName = "left foot"
  , bOrientation = Vec3 0 0 0
  , bMesh = m_foot
  , bChildren = []
  }


-- remove redundant indices
mOptimize :: [Mesh] -> [Mesh]
mOptimize ms = map remapMesh ms
  where new_v_ixs :: DM.Map Vec3 Int
        new_v_ixs = remapIndices (concatMap (elems . mVertexArray) ms)

        new_n_ixs :: DM.Map Vec3 Int
        new_n_ixs = remapIndices (concatMap (elems . mNormalArray) ms)

        mkArr :: DM.Map Vec3 Int -> Array Int Vec3
        mkArr m = array (0,DM.size m) (map (\(v,ix) -> (ix,v)) (DM.toList m))

        remapMesh :: Mesh -> Mesh
        remapMesh m = 
              m {
                mPrimitives = map updPrim (mPrimitives m)
              , mVertexArray = mkArr new_v_ixs
              , mNormalArray = mkArr new_n_ixs
              } 
          where updPrim :: Prim -> Prim
                updPrim p = p {pVerts = map updVert (pVerts p)}
                  where updVert :: (Int,Int) -> (Int,Int)
                        updVert (n_ix,v_ix) = 
                            case (n `DM.lookup` new_n_ixs,v `DM.lookup` new_v_ixs) of
                              (Just n_new_ix, Just v_new_ix) -> (n_new_ix,v_new_ix)
                          where n = mNormalArray m ! n_ix
                                v = mVertexArray m ! v_ix

        remapIndices :: Ord a => [a] -> DM.Map a Int
        -- TODO: I could order them by:
        --   - submission order (first mesh first)
        --   - Euclean order (natural order)
        --   - frequency order (caching benefits?)
        remapIndices as = DM.fromList $ zip nub_as [0..]
          where nub_as = map fst $ DM.toList $ DM.fromList $ map (\a -> (a,())) as


data Mesh =
  Mesh {
    mName :: !String
  , mPrimitives :: ![Prim]
  , mNormalArray :: !(Array Int Vec3)
  , mVertexArray :: !(Array Int Vec3) -- DIM.IntMap Vec3
    -- TODO: material..etc...
  } deriving (Show,Eq)
data Prim =
  Prim {
    pMode :: !PrimitiveMode
  , pVerts :: ![(Int,Int)] -- normal and
  } deriving (Show,Eq)

-- mesh builder monad
type MB a = ST.State MBSt a
data MBSt =
  MBSt {
    mbstVs :: DM.Map Vec3 Int
  , mbstVsIx :: DIM.IntMap Vec3
  , mbstNs :: DM.Map Vec3 Int
  , mbstNsIx :: DIM.IntMap Vec3
  } 



mbBuild :: String -> MB [Prim] -> Mesh
mbBuild nm f = Mesh nm ps n_arr v_arr
  where (ps,n_arr,v_arr) = evalMB f


evalMB :: MB a -> (a,Array Int Vec3,Array Int Vec3)
evalMB p = ST.evalState f mb_st0
  where mb_st0 = MBSt DM.empty DIM.empty DM.empty DIM.empty
        f = do
          a <- p
          n_arr <- mbMakeIndexArray mbstNsIx
          v_arr <- mbMakeIndexArray mbstVsIx
          return (a,n_arr,v_arr)


mbVert :: Vec3 -> MB Int
mbVert v = do
  mb_st <- ST.get
  let (mv,mi) = (mbstVs mb_st,mbstVsIx mb_st)
  case v`DM.lookup`mv of
    Nothing -> do
      ST.put $
        mb_st {
          mbstVs = DM.insert v (DM.size mv) mv
        , mbstVsIx = DIM.insert (DM.size mv) v mi
        }
      return (DM.size mv)
    Just ix -> return ix


mbNorm :: Vec3 -> MB Int
mbNorm v0 = do
  let v = norm v0
  mb_st <- ST.get
  let (mv,mi) = (mbstNs mb_st,mbstNsIx mb_st)
  case v`DM.lookup`mv of
    Nothing -> do
      ST.put $
        mb_st {
          mbstNs = DM.insert v (DM.size mv) mv
        , mbstNsIx = DIM.insert (DM.size mv) v mi
        }
      return (DM.size mv)
    Just ix -> return ix


mbMakeIndexArray :: (MBSt -> DIM.IntMap a) -> MB (Array Int a)
mbMakeIndexArray f = do
  dim <- ST.gets f
  return $ listArray (0,DIM.size dim - 1) (DIM.elems dim)


mFromTriStrips :: String -> [[Vec3]] -> Mesh
mFromTriStrips nm = mbBuild nm . mapM addTriStrip
  where addTriStrip :: [Vec3] -> MB Prim
        addTriStrip vs0 = 
          case vs0 of
            (v0:v1:vss@(_:_)) -> do
              ixs <- addVerts 0 v0 v1 vss
              return $ Prim TriangleStrip ixs
            _ -> error "mFromTriStrips: need at least three vertices"
        
        mbNormFromVec :: Int -> Vec3 -> Vec3 -> Vec3 -> MB Int
        mbNormFromVec ix v0 v1 v2 = do
          let n
                | odd ix = (v0 .- v1) `cross` (v2 .- v1)
                | otherwise = (v2 .- v1) `cross` (v0 .- v1)
          mbNorm n

        addVerts :: Int -> Vec3 -> Vec3 -> [Vec3] -> MB [(Int,Int)]
        addVerts ix v0 v1 (v2:vs) = do
          n_ix <- mbNormFromVec ix v0 v1 v2
          v_ix0 <- mbVert v0
          if null vs then do -- last triangle
            v_ix1 <- mbVert v1
            v_ix2 <- mbVert v2
            return [(n_ix,v_ix0),(n_ix,v_ix1),(n_ix,v_ix2)]
            else do -- middle triangle, add v0
              ((n_ix,v_ix0):) <$> addVerts (ix+1) v1 v2 vs


mFormat :: Mesh -> String
mFormat m =
    "Mesh " ++ mName m ++ " (#" ++ show (length (mPrimitives m)) ++ ")\n" ++
    concatMap fmtPrim (mPrimitives m) ++
    ""
  where fmtPrim :: Prim -> String
        fmtPrim p =
            "  " ++ show (pMode p) ++ "\n" ++
            concatMap fmtVert (pVerts p)
          where fmtVert (ni,vi) = "    " ++ fmtVec3 (mVertexArray m ! vi) ++ norm_sfx ++ "\n"
                  where norm_sfx
                          | ni < 0 = " (no norm)"
                          | otherwise = "  (" ++ fmtVec3 (mNormalArray m ! ni) ++ ")"


mImmediateRender :: Bool -> Mesh -> IO ()
mImmediateRender dbg m = mapM_ renderPrim (mPrimitives m) >> runDbg
  where renderPrim :: Prim -> IO ()
        renderPrim p = do
          renderPrimitive (pMode p) $ do
            -- putStrLn $ "renderPrimitive "++ show (pMode p)
            mapM_ renderVert (pVerts p)

        renderVert :: (Int,Int) -> IO ()
        renderVert (ni,vi) = do
          when (ni >= 0) $
            norm (ns ! ni)
          vert (vs ! vi)

        vert :: Vec3 -> IO ()
        vert (Vec3 x y z) = do
          -- putStrLn $ "  " ++ fmtVec3 (Vec3 x y z)
          vertex3f x y z
        norm :: Vec3 -> IO ()
        norm (Vec3 x y z) = normal3f x y z

        vs = mVertexArray m
        ns = mNormalArray m

        runDbg
          | not dbg = return ()
          | otherwise = 
            preservingAttrib [EnableAttributes,CurrentAttributes,LightingAttributes] $ do
              -- lineStipple $= Just (10,0xA0A0)
              lighting $= Disabled
              depthMask $= Enabled
              color3f 1.0 1.0 0.0
              renderPrimitive Lines $ do          
                renderNorms (mPrimitives m) $ \n v -> vert v >> vert (v .+ n)
              color3f 1.0 0.0 1.0
              pointSize $= 3
              renderPrimitive Points $ do          
                renderNorms (mPrimitives m) $ \n v  -> vert (v .+ n)

        renderNorms :: [Prim] -> (Vec3 -> Vec3 -> IO ()) -> IO ()
        renderNorms ps f = mapM_ (mapM_ rendIxs . pVerts) ps
          where rendIxs :: (Int,Int) -> IO ()
                rendIxs (n_ix,v_ix)
                  | n_ix < 0 = return ()
                  | otherwise = f n v
                  where v = vs ! v_ix
                        n = ns ! n_ix


m_leg :: Mesh
m_leg = m
  where m = mFromTriStrips "leg" [
              ring ( 0.0) 1.0 1.4
            , ring (-0.3) 1.4 1.4
            , ring (-0.6) 1.4 1.2
            , ring (-0.9) 1.2 1.0
            ]

        ring dy zf_t zf_b =
            map
              (\(Vec3 x y z) -> Vec3 x (y+dy) z)
              (complete r_side)
          where r_side =
                  [
                    bback_bot_r
                  , bback_top_r
                  , mback_bot_r
                  , mback_top_r
                  , bmid_bot_r
                  , bmid_top_r
                  , fmid_bot_r
                  , fmid_top_r
                  ]
                seg_dy = 0.3

                bback_bot_r = Vec3 (-0.05)  (-seg_dy) (-0.35*zf_b)
                bback_top_r = Vec3 (-0.05)  ( 0.00)   (-0.35*zf_t)
                mback_bot_r = Vec3 (-0.25)  (-seg_dy) (-0.25)
                mback_top_r = Vec3 (-0.25)  ( 0.00)   (-0.25)
                bmid_bot_r  = Vec3 (-0.30)  (-seg_dy) ( 0.00)
                bmid_top_r  = Vec3 (-0.30)  ( 0.00)   ( 0.00)
                fmid_bot_r  = Vec3 (-0.15)  (-seg_dy) ( 0.25)
                fmid_top_r  = Vec3 (-0.15)  ( 0.00)   ( 0.25)


m_foot :: Mesh
m_foot = mFromTriStrips "foot" [complete r_side, top, bot]
  where r_side =
          [
            bback_bot_r
          , bback_top_r
          , mback_bot_r
          , mback_top_r
          , bmid_bot_r
          , bmid_top_r
          , fmid_bot_r
          , fmid_top_r
          , front_bot_r
          , front_top_r
          ]

        top =
          [
                     front_top_r
          ,          fmid_top_r
          , reflectX front_top_r
          , reflectX fmid_top_r
          ]
        bot =
          [
            reflectX bback_bot_r
          ,          bback_bot_r
          , reflectX mback_bot_r
          ,          mback_bot_r
          , reflectX bmid_bot_r
          ,          bmid_bot_r
          , reflectX fmid_bot_r
          ,          fmid_bot_r
          , reflectX front_bot_r
          ,          front_bot_r
          ]

        bback_bot_r = Vec3 (-0.05)  (-0.30)  (-0.35)
        bback_top_r = Vec3 (-0.05)  ( 0.00)  (-0.35)
        mback_bot_r = Vec3 (-0.25)  (-0.30)  (-0.25)
        mback_top_r = Vec3 (-0.25)  ( 0.00)  (-0.25)
        bmid_bot_r  = Vec3 (-0.30)  (-0.30)  (0.00)
        bmid_top_r  = Vec3 (-0.30)  ( 0.00)  (0.00)
        fmid_bot_r  = Vec3 (-0.25)  (-0.30)  (0.25)
        fmid_top_r  = Vec3 (-0.25)  ( 0.00)  (0.25)
        --
        front_bot_r = Vec3 (-0.15) (-0.30)  (0.80)
        front_top_r = Vec3 (-0.15) (-0.15)  (0.80)


reflectX (Vec3 x y z) = Vec3 (-x) y z

-- reflect the X value the Z axis
-- this completes the geometry all the way around the
-- Z axis into a full ring
complete :: [Vec3] -> [Vec3]
complete vs = vs ++ adjust vs_r ++ take 2 vs
  where vs_r = reverse $ map reflectX vs
        adjust [] = []
        adjust (v0:v1:vs) = v1:v0:adjust vs

m_knee :: Mesh
m_knee = mFromTriStrips "knee" [
      ringsToTriStrip top_ring mid_ring
    , ringsToTriStrip mid_ring bot_ring    
    ]
  where bot_ring =
          completeHalfRing [
            Vec3 (-0.15)  (-0.15)  (-0.35)
          , Vec3 (-0.30)  (-0.15)  ( 0.00)
          , Vec3 (-0.15)  (-0.15)  ( 0.35)
          ]
        mid_ring = map (.+(Vec3 0.00 0.15 0.00)) bot_ring
        top_ring = map (.+(Vec3 0.00 0.15 0.00)) mid_ring

        -- scaleXZ k (Vec3 x y z) = (Vec3 (x*k) y (z*k))

-- completes a ring across the Z axis (reflect X)
completeHalfRing :: [Vec3] -> [Vec3]
completeHalfRing vs = vs ++ concatMap f (reverse vs)
  where f (Vec3 0 y z) = []
        f (Vec3 x y z) = [Vec3 (-x) y z]

ringsToTriStrip :: [Vec3] -> [Vec3] -> [Vec3]
ringsToTriStrip vs_hi vs_lo
  | length vs_hi /= length vs_lo = error "ringsToTriStrip: length size mismatch"
  | otherwise = joinMesh vs_hi vs_lo
  where joinMesh :: [Vec3] -> [Vec3] -> [Vec3]
        joinMesh (v_hi:v_his) (v_lo:v_los) =
          v_hi:v_lo:joinMesh v_his v_los -- CCW
          -- v_lo:v_hi:joinMesh v_los v_his -- CW
        joinMesh [] [] = take 1 vs_hi ++ take 1 vs_lo


fmtPrim :: [Vec3] -> IO ()
fmtPrim = mapM_ (putStrLn . fmtVec3)

-- lo = completeHalfRing [
--             Vec3 (-0.15)  (-0.15)  (-0.35)
--           , Vec3 (-0.15)  (-0.15)  ( 0.35)
--           ]
--  mi = map (.+(Vec3 0.00 0.15 0.00)) lo

-- x = fmtPrim (ringsToTriStrip bot_ring mid_ring)
--  where bot_ring = completeHalfRing [
--           Vec3 (-0.2) (0.0) (-0.2),
--           Vec3 (-0.2) (0.0) ( 0.2)]
--        mid_ring = map (.+(Vec3 0.00 0.15 0.00)) bot_ring
