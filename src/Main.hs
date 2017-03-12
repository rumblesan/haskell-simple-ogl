module Main where

import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL
import System.Exit
import System.IO

import Control.Monad

import GeometryBuffers
import LoadShaders


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
errorCallback err description = hPutStrLn stderr description


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
      mw <- GLFW.createWindow 400 400 "Improviz" Nothing Nothing
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
        display cube window
        GLFW.destroyWindow window
        GLFW.terminate
        exitSuccess

display :: VAO -> GLFW.Window -> IO ()
display cube w = unless' (GLFW.windowShouldClose w) $
  do
    (width, height) <- GLFW.getFramebufferSize w
    let ratio = fromIntegral width / fromIntegral height
    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))

    clear [ ColorBuffer, DepthBuffer ]

    matrixMode $= Projection
    loadIdentity
    ortho (negate ratio) ratio (negate 1.0) 1.0 1.0 (negate 1.0)
    matrixMode $= Modelview 0

    loadIdentity
    let (VAO bo bai bn) = cube

    bindVertexArrayObject $= Just bo
    drawArrays Triangles bai bn

    GLFW.swapBuffers w
    GLFW.pollEvents
    display cube w
