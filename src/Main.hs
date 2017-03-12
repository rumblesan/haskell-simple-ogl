module Main where

import Data.Vec

import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import qualified Graphics.GL as GLRaw
import System.Exit
import System.IO

import Foreign.Marshal.Utils
import Foreign.Ptr

import Control.Monad

import GeometryBuffers
import LoadShaders
import Matrices


bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes

unless' :: Monad m => m Bool -> m () -> m ()
unless' action falseAction = do
    b <- action
    unless b falseAction

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m nothingRes f = case m of
    Nothing -> nothingRes
    Just x  -> f x


-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr


main :: IO ()
main = do
  GLFW.setErrorCallback (Just errorCallback)
  successfulInit <- GLFW.init
  bool successfulInit exitFailure $
    do
      v <- GLFW.getVersion
      print v
      GLFW.windowHint $ WindowHint'ContextVersionMajor 3
      GLFW.windowHint $ WindowHint'ContextVersionMinor 2
      GLFW.windowHint $ WindowHint'OpenGLForwardCompat True
      GLFW.windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
      let sW = 640
          sH = 480
      mw <- GLFW.createWindow sW sH "Improviz" Nothing Nothing
      maybe' mw (GLFW.terminate >> exitFailure) $ \window -> do
        GLFW.makeContextCurrent mw
        cvma <- getWindowContextVersionMajor window
        cvmi <- getWindowContextVersionMinor window
        cvr <- getWindowContextVersionRevision window
        print (cvma, cvmi, cvr)
        program <- loadShaders [
            ShaderInfo VertexShader (FileSource "shaders/triangles.vert"),
            ShaderInfo FragmentShader (FileSource "shaders/triangles.frag")]
        currentProgram $= Just program
        cube <- cubeVAO
        let proj = projectionMat 0.1 100 (pi/4) (fromIntegral sW / fromIntegral sH)
            view = viewMat (vec3 4 3 3) (vec3 0 0 0) (vec3 0 1 0)
            vpMat = multmm proj view
        display cube vpMat program window
        GLFW.destroyWindow window
        GLFW.terminate
        exitSuccess

display :: VAO -> Mat44 GLfloat -> Program -> GLFW.Window -> IO ()
display cube vpMat progId w = unless' (GLFW.windowShouldClose w) $
  do
    (width, height) <- GLFW.getFramebufferSize w
    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
    clear [ ColorBuffer, DepthBuffer ]
    (UniformLocation mpMatUniform) <- GL.get $ uniformLocation progId "MPMat"
    with vpMat
      $ GLRaw.glUniformMatrix4fv mpMatUniform 1 (fromBool True)
      . castPtr
    let (VAO bo bai bn) = cube

    bindVertexArrayObject $= Just bo
    drawArrays Triangles bai bn

    GLFW.swapBuffers w
    GLFW.pollEvents
    display cube vpMat progId w
