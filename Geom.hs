{-# LANGUAGE TypeSynonymInstances #-}
module Geom where

import Math

import Graphics.UI.GLUT

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Text.Printf


getCurrentMat4x4 :: Maybe MatrixMode -> IO Mat4x4
getCurrentMat4x4 mmt = do
  mc <- get (matrix mmt) :: IO (GLmatrix GLfloat)
  ms <- getMatrixComponents RowMajor mc
  return $ fromListMat4x4 ms

instance Coord GLint where
  toF = fromIntegral


----------------
normal3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
normal3f x y z = normal $ Normal3 x y z
vertex2i :: Integral i => i -> i -> IO ()
vertex2i x y = vertex $ Vertex2 (fromIntegral x :: GLint) (fromIntegral y :: GLint)
vertex2iv :: Integral i => (i,i) -> IO ()
vertex2iv = uncurry vertex2i
vertex2f :: GLfloat -> GLfloat -> IO ()
vertex2f x y = vertex $ Vertex2 x y
vertex2fv :: (GLfloat,GLfloat) -> IO ()
vertex2fv = uncurry vertex2f
vertex3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertex3f x y z = vertex $ Vertex3 x y z
vertex3fv :: (GLfloat,GLfloat,GLfloat) -> IO ()
vertex3fv = uncurry3 vertex3f
color3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3f x y z = color $ Color3 x y z
color3fv :: (GLfloat,GLfloat,GLfloat) -> IO ()
color3fv = uncurry3 color3f
uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f = \(a,b,c) -> f a b c



lineRect :: Coord c => Vertex2 c -> Vertex2 c -> IO ()
lineRect (Vertex2 x1 y1) (Vertex2 x2 y2) = do
  renderPrimitive LineLoop $ do
    vertex2f (toF x1) (toF y1)
    vertex2f (toF x1) (toF y2)
    vertex2f (toF x2) (toF y2)
    vertex2f (toF x2)( toF y1)

drawText :: Coord c => c -> c -> String -> IO ()
drawText x y txt = body
  where body = preservingMatrix $ preservingAttrib attrs $ do
          depthMask $= Disabled
          loadIdentity
          drawTextPrim (toF x) (toF y) txt
        attrs = [DepthBufferAttributes,CurrentAttributes]

-- Fixed9By15 :: BitmapFont
drawTextPrim :: GLfloat -> GLfloat -> String -> IO ()
drawTextPrim x y txt = do
  -- Fixed9By15
  rasterPos (Vertex2 x y)
  renderString Helvetica18 txt

dft_font = Helvetica18