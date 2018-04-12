import Geom
import Math
import Shapes

import Graphics.UI.GLUT
import qualified Graphics.UI.GLUT as GLUT

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.IORef
import System.Random
import System.Directory
import Text.Printf

data St =
  St {
    stRx :: !GLfloat
  , stRy :: !GLfloat
  , stSc :: !GLfloat
  , stLDown :: !Bool
  , stPos :: !(Maybe (GLfloat,GLfloat))
  , stSkeletonRoot :: !Bone
  , stSelected :: !String
  } deriving Show
st_init :: St
st_init = St 0 0 1 False Nothing skeleton_root ""


main :: IO ()
main = go

go :: IO ()
go = run

-- getModificationTime

-- run once on startup
init :: IO ()
init = do
  depthFunc $= Just Less
  depthMask $= Enabled
enableAntialiasing :: IO ()
enableAntialiasing = do
  lineSmooth $= Enabled
  blend      $= Enabled
  blendFunc  $= (SrcAlpha, OneMinusSrcAlpha)
  lineWidth  $= 1.5
  shadeModel $= Smooth

run :: IO ()
run = do
  (_progName, _args) <- getArgsAndInitialize
  initialWindowSize $= Size 1024 768 -- 1200 800
  actionOnWindowClose $= MainLoopReturns
  st_ref <- newIORef st_init
  _window <- createWindow "Figure"
  let reshape :: ReshapeCallback
      reshape size@(Size w h) = do
        putStrLn $ "reshape: " ++ show size
        viewport $= (Position 0 0, size)
        reset
  --
  reshapeCallback $= Just reshape
  --
  let display :: DisplayCallback
      display  = do
        --
        clear [ ColorBuffer, DepthBuffer ]
        --
        --
        -- draw 3 boxes for now
        color3f 1 1 1
        Size w h <- get screenSize
        --
        -- loadIdentity
        --
        renderAxesSized 2 0x1111
        --
        --
        -- let a = Vec3 0 0 0
        --    b = Vec3 0 1 0
        --    c = Vec3 1 0 0
        --    d = Vec3 1 1 0 -- (-0.3)
        --    e = Vec3 2 0 0
        --    f = Vec3 2 1 0
        --    g = Vec3 3 0 0
        --    h = Vec3 3 1 0
        --    a2 = Vec3 0 0 (-2)
        --    b2 = Vec3 0 1 (-2)
        --    c2 = Vec3 1 0 (-2)
        --    d2 = Vec3 1 1 (-2.3)
        -- renderPrimitive TriangleStrip $ do
        -- CW
        -- a >> b >> c >> d >> e >> f >> g >> h
        -- h >> g >> f >> e >> d >> c >> b >> a
        --
        -- CCW (the default)
        -- (THIS IS THE RIGHT WAY)
        -- b >> a >> d >> c >> f >> e >> h >> g
        let test_vs_front = 
              [Vec3 0 0 0, Vec3 0 1 0, Vec3 1 0 0] ++
              [Vec3 1 1 (-1)]
        let m = mFromTriStrips "test" [test_vs_front]
        mImmediateRender True m
        -- color3f 0 1 1
        -- mImmediateRender True (mFromTriStrips "test" [[b2,a2,d2]] )
        -- putStrLn $ mFormat m
        --
        -- renderFigures st_ref
        --
        flush
  displayCallback $= display
  --
  enableAntialiasing
  let -- type KeyboardMouseCallback = Key -> KeyState -> Modifiers -> Position -> IO ()
      keyboardMouse :: KeyboardMouseCallback
      keyboardMouse key  Up _ _ =
        case key of
          MouseButton LeftButton -> modifyIORef st_ref $ \st -> st{stLDown = False}
          _ -> return ()
      keyboardMouse key Down ms (Position mx my) = do
        let rot_amount = if shift ms == Down then 1.0 else 15 :: GLfloat
            sc_amount = if shift ms == Down then 0.01 else 0.1 :: GLfloat
        case key of
          Char '\ESC' -> leaveMainLoop
          Char 'l' -> do
            l <- get lighting
            lighting           $= if l == Enabled then Disabled else Enabled
            light    (Light 0) $= Enabled
            position (Light 0) $= Vertex4 3 3 3 0
            ambient  (Light 0) $= Color4 1 1 1 1
            diffuse  (Light 0) $= Color4 1 1 1 1
            specular (Light 0) $= Color4 1 1 1 1
            light    (Light 1) $= Enabled
            position (Light 1) $= Vertex4 (-3) (-3) (-3) 0
            ambient  (Light 1) $= Color4 1 1 1 1
            diffuse  (Light 1) $= Color4 1 1 1 1
            specular (Light 1) $= Color4 1 1 1 1
            -- http://devernay.free.fr/cours/opengl/materials.html
            -- red plastic 0.0  0.0 0.0 0.5 0.0 0.0 0.7 0.6 0.6 .25
            materialAmbient Front $= Color4 0.0 0.0 0.0 1.0
            materialDiffuse Front $= Color4 0.7 0.4 0.4 1.0
            materialSpecular Front $= Color4 1.0 0.8 0.8 1.0
            materialShininess Front $= 128.0*0.25
            get maxShininess >>= print

          Char 's' -> do
            m <- get shadeModel
            shadeModel $= if m == Flat then Smooth else Flat
          Char 'x' -> rotBone st_ref (Vec3 ( rot_amount) 0 0)
          Char 'X' -> rotBone st_ref (Vec3 (-rot_amount) 0 0)
          Char 'y' -> rotBone st_ref (Vec3 0 ( rot_amount) 0)
          Char 'Y' -> rotBone st_ref (Vec3 0 (-rot_amount) 0)
          Char 'z' -> rotBone st_ref (Vec3 0 0 ( rot_amount))
          Char 'Z' -> rotBone st_ref (Vec3 0 0 (-rot_amount))
          Char 'r' -> reset >> writeIORef st_ref st_init
          Char 'p' -> do
            (m,_) <- get polygonMode
            let pm = if m == Fill then Line else Fill
            polygonMode $= (pm,Line)
          Char 'd' -> do
            readIORef st_ref >>= print
            dumpMatrices
            b <- stSkeletonRoot <$> readIORef st_ref
            putStrLn $
              concatMap (\b -> fmtBone b ++ "\n\n") (enumBones b)
            postRedisplay Nothing
          Char '\t'
            | shift ms == Down -> selectPrev st_ref
            | otherwise -> selectNext st_ref
          SpecialKey KeyLeft -> do
            -- modifyIORef st_ref $ \st -> st{stRy = stRy st + 0.5}
            rotate rot_amount (Vector3 0 1 0)
          SpecialKey KeyRight -> do
            -- modifyIORef st_ref $ \st -> st{stRy = stRy st - 0.5}
            rotate (-rot_amount) (Vector3 0 1 0)
          SpecialKey KeyUp -> do
            -- modifyIORef st_ref $ \st -> st{stRx = stRx st + 0.5}
            rotate rot_amount (Vector3 1 0 0)
          SpecialKey KeyDown -> do
            -- modifyIORef st_ref $ \st -> st{stRx = stRx st - 0.5}
            rotate (-rot_amount) (Vector3 1 0 0)
          MouseButton WheelDown -> do
              scale f f f where f = (1-sc_amount) :: GLfloat
          MouseButton WheelUp -> do
              scale f f f where f = (1+sc_amount) :: GLfloat
          MouseButton LeftButton -> do
            modifyIORef st_ref $ \st -> st{stLDown = True}
            return ()
          MouseButton RightButton -> do
            print (mx,my)
          _ -> putStrLn $ "unhandled key down: " ++ show key
        postRedisplay Nothing
  keyboardMouseCallback $= Just keyboardMouse
  let -- type MotionCallback -> Position -> IO ()
      motionCb :: MotionCallback
      motionCb (Position x y) = do
        st <- readIORef st_ref
        let mpos = stPos st
            (nx,ny) = (fromIntegral x,fromIntegral y :: GLfloat)
        case mpos of
          Nothing -> return ()
          Just (ox,oy) -> do
            let (dx,dy) = (nx-ox,ny-oy)
            if dx > dy then rotate (dx) (Vector3 0 1 0)
              else rotate (dy) (Vector3 1 0 0)
        writeIORef st_ref $ st{stPos = Just (nx,ny)}
  motionCallback $= Just motionCb
  mainLoop

rotBone :: IORef St -> Vec3 -> IO ()
rotBone st_ref v_r =
    modifyIORef st_ref $
      \st -> st{stSkeletonRoot = updBone (stSelected st) f (stSkeletonRoot st)}
  where f b = b{bOrientation = bOrientation b .+ v_r}


selectPrev :: IORef St -> IO ()
selectPrev st_ref = modifyIORef st_ref $ \st -> st{stSelected = f st}
  where f st
          | null (stSelected st) = last skeleton_bone_names
          | stSelected st == head skeleton_bone_names = last skeleton_bone_names
          | otherwise = srch skeleton_bone_names
          where srch (b1:b2:bs)
                  | b2 == stSelected st = b1
                  | otherwise = srch (b2:bs)
                srch _ = ""

selectNext :: IORef St -> IO ()
selectNext st_ref = modifyIORef st_ref $ \st -> st{stSelected = f st}
  where f st
          | null (stSelected st) = head skeleton_bone_names
          | otherwise = srch skeleton_bone_names
          where srch (b1:b2:bs)
                  | b1 == stSelected st = b2
                  | otherwise = srch (b2:bs)
                srch [b] = head skeleton_bone_names


reset :: IO ()
reset = do
  (Size w h) <- screenSize
  matrixMode $= Projection
  loadIdentity
  let fov = 70.0
      aspect = fromIntegral w / fromIntegral h
      z_near = 0.1
      z_far = 10000.0
  perspective fov aspect z_near z_far
  --
  matrixMode $= Modelview 0
  loadIdentity
  let eye = Vertex3 1 1 3 -- 1 3 5 :: Vertex3 GLdouble
      center = Vertex3 0 0 0 :: Vertex3 GLdouble
      up = Vector3 0 1 0 :: Vector3 GLdouble
  lookAt eye center up
  -- polygonMode $= (Line,Line)
  -- dumpMatrices
  postRedisplay Nothing




-- ROPE HAS
-- PRJ
--   1.07   0.00   0.00   0.00
--   0.00   1.43   0.00   0.00
--   0.00   0.00  -1.00  -0.20
--   0.00   0.00  -1.00   0.00
-- MODVW
--   1.00   0.00   0.00   0.00
--   0.00   0.95  -0.32  -0.00
--   0.00   0.32   0.95  -7.91
--   0.00   0.00   0.00   1.00
dumpMatrices :: IO ()
dumpMatrices = do
  putStrLn "**** Projection ****"
  getCurrentMat4x4 (Just Projection) >>= putStrLn . fmtMat4x4
  putStrLn "**** Modelview ****"
  getCurrentMat4x4 (Just (Modelview 0)) >>= putStrLn . fmtMat4x4


renderFigures :: IORef St -> IO ()
renderFigures st_ref = do
  st <- readIORef st_ref
  color3f 1 1 1
  -- renderFigure
  renderHierarchy (stSelected st) (stSkeletonRoot st)


