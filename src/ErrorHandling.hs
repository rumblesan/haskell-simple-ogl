module ErrorHandling where

import Graphics.Rendering.OpenGL as GL

printErrors :: IO ()
printErrors = GL.get errors >>= mapM_ print

