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
import PostProcessing


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
        (width, height) <- GLFW.getFramebufferSize window
        GLFW.makeContextCurrent mw
        cvma <- getWindowContextVersionMajor window
        cvmi <- getWindowContextVersionMinor window
        cvr <- getWindowContextVersionRevision window
        print (cvma, cvmi, cvr)
        depthFunc $= Just Less
        program <- loadShaders [
            ShaderInfo VertexShader (FileSource "shaders/simple3d.vert"),
            ShaderInfo FragmentShader (FileSource "shaders/simple3d.frag")]
        post <- createPostProcessing (fromIntegral width) (fromIntegral height)
        cube <- cubeVAO
        let proj = projectionMat 0.1 100 (pi/4) (fromIntegral width / fromIntegral height)
            view = viewMat (vec3 4 3 3) (vec3 0 0 0) (vec3 0 1 0)
            vpMat = multmm proj view
        clearColor $= Color4 0.0 0.0 0.0 0.0
        display cube vpMat program post window
        GLFW.destroyWindow window
        GLFW.terminate
        exitSuccess


renderScene :: VAO -> Mat44 GLfloat -> PostProcessing -> Program -> IO ()
renderScene cube vpMat post progId = do
  usePostProcessing post
  depthFunc $= Just Less
  Just t <- GLFW.getTime
  let time = realToFrac t

  clear [ ColorBuffer, DepthBuffer ]
  currentProgram $= Just progId
  let modelMat = multmm (rotMat time time time) (transMat 1.0 1.0 1.0)
  let mvpMat = multmm vpMat modelMat
  colourU <- GL.get $ uniformLocation progId "vertexColor"
  let c = Color4 1.0 1.0 0.5 1.0 :: Color4 GLfloat
  uniform colourU $= c
  (UniformLocation mvpMatUniform) <- GL.get $ uniformLocation progId "MVPMat"
  with mvpMat
    $ GLRaw.glUniformMatrix4fv mvpMatUniform 1 (fromBool True)
    . castPtr
  let (VAO bo bai bn) = cube
  bindVertexArrayObject $= Just bo
  drawArrays Triangles bai bn
  renderPostProcessing post PaintOver


display :: VAO -> Mat44 GLfloat -> Program -> PostProcessing -> GLFW.Window -> IO ()
display cube vpMat progId post w = unless' (GLFW.windowShouldClose w) $
  do

    renderScene cube vpMat post progId

    GLFW.swapBuffers w
    GLFW.pollEvents
    display cube vpMat progId post w
