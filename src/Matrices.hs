{-# LANGUAGE RankNTypes #-}  
{-# LANGUAGE TypeOperators #-}

module Matrices where

import Data.Vec

vec3 :: forall a a1 a2. a -> a1 -> a2 -> a :. (a1 :. (a2 :. ()))
vec3 x y z = x :. y :. z:. ()

projectionMat :: Floating f => f -> f -> f -> f -> Mat44 f
projectionMat near far fov aspect = perspective near far fov aspect

orthoMat :: Floating f => f -> f -> f -> f -> Mat44 f
orthoMat near far width height =
  let
    om = orthogonal near far (width :. height :. ())
  in
    om


viewMat :: Floating f => Vec3 f -> Vec3 f -> Vec3 f -> Mat44 f
viewMat = lookAt

lookAt :: Floating f => Vec3 f -> Vec3 f -> Vec3 f -> Mat44 f
lookAt eye target up = x :. y :. z :. h :. ()
  where
    forward = normalize $ target - eye
    right = normalize $ cross forward up
    up' = cross right forward
    x = snoc right (-(dot right eye))
    y = snoc up' (-(dot up' eye))
    z = snoc (-forward) (dot forward eye)
    h = 0 :. 0 :. 0 :. 1 :. ()

rotMat :: Floating f => f -> f -> f -> Mat44 f
rotMat xRot yRot zRot = rotationEuler $ xRot :. yRot :. zRot :. ()

transMat :: Floating f => f -> f -> f -> Mat44 f
transMat xT yT zT = translation $ vec3 xT yT zT
