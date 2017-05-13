module TextRendering (
    createTextRenderer
  , renderText
  , renderCharacter
  , resizeTextRendererScreen
  , changeTextColour
  , textCoordMatrix
  , TextRenderer
) where


import Control.Monad

import Foreign.Marshal.Utils
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr

import qualified Data.Map.Strict as M

import Graphics.Rendering.OpenGL as GL
import GeometryBuffers (bufferOffset)
import LoadShaders
import FreeType (CharacterMap, Character(..), loadFontCharMap)
import qualified Graphics.GL as GLRaw

import Data.Vec (Mat44, multmm)
import Matrices

import ErrorHandling (printErrors)


data CharQuad = CharQuad VertexArrayObject BufferObject ArrayIndex NumArrayIndices deriving (Show, Eq)

data TextRenderer = TextRenderer {
    characterMap :: CharacterMap
  , charSize :: Int
  , pMatrix :: Mat44 GLfloat
  , program :: Program
  , characterQuad :: CharQuad
  , textColour :: Color3 GLfloat
} deriving (Show, Eq)

textCoordMatrix :: Floating f => f -> f -> f -> f -> f -> f -> Mat44 f
textCoordMatrix left right top bottom near far =
  let
    o = orthoMat left right top bottom near far
    t = transMat (-1) 1 0
  in
    multmm t o

-- TODO - better error handling
getCharacter :: TextRenderer -> Char -> Character
getCharacter (TextRenderer charMap _ _ _ _ _) c = charMap M.! c

charQuad :: IO CharQuad
charQuad = do
  vao <- genObjectName
  bindVertexArrayObject $= Just vao
  arrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just arrayBuffer
  let
    vertexSize = sizeOf (0 :: GLfloat)
    firstPosIndex = 0
    firstTexIndex = 2 * vertexSize
    vPosition = AttribLocation 0
    vTexCoord = AttribLocation 1
    numVertices = 6 * 4
    size = fromIntegral (numVertices * vertexSize)
    stride = fromIntegral (4 * vertexSize)
  bufferData ArrayBuffer $= (size, nullPtr, DynamicDraw)
  vertexAttribPointer vPosition $= (ToFloat, VertexArrayDescriptor 2 Float stride (bufferOffset firstPosIndex))
  vertexAttribPointer vTexCoord $= (ToFloat, VertexArrayDescriptor 2 Float stride (bufferOffset firstTexIndex))
  vertexAttribArray vPosition $= Enabled
  vertexAttribArray vTexCoord $= Enabled
  bindVertexArrayObject $= Nothing
  bindBuffer ArrayBuffer $= Nothing

  return $ CharQuad vao arrayBuffer firstPosIndex 6

drawCharQuad :: CharQuad -> IO ()
drawCharQuad (CharQuad arrayObject _ firstIndex numTriangles) = do
  bindVertexArrayObject $= Just arrayObject
  drawArrays Triangles firstIndex numTriangles

createTextRenderer :: String -> Int -> Color3 GLfloat -> Mat44 GLfloat -> IO TextRenderer
createTextRenderer fontPath charSize textColour projectionMatrix = do
  cq <- charQuad
  program <- loadShaders [
    ShaderInfo VertexShader (FileSource "shaders/textrenderer.vert"),
    ShaderInfo FragmentShader (FileSource "shaders/textrenderer.frag")]
  characters <- loadFontCharMap fontPath charSize
  return $ TextRenderer characters charSize projectionMatrix program cq textColour

resizeTextRendererScreen :: Mat44 GLfloat -> TextRenderer -> TextRenderer
resizeTextRendererScreen orthoMatrix trender =
  trender {
    pMatrix = orthoMatrix
  }

changeTextColour :: Color3 GLfloat -> TextRenderer -> TextRenderer
changeTextColour newColour trender =
  trender {
    textColour = newColour
  }

renderText :: Int -> Int -> TextRenderer -> String -> IO ()
renderText xpos ypos renderer strings = do
  currentProgram $= Just (program renderer)
  foldM_ (\(xp, yp) c ->
    case c of
      '\n' -> return (xpos, yp + charSize renderer)
      _ ->
        do
          let char@(Character _ _ _ adv _) = getCharacter renderer c
          renderCharacter renderer char xp yp
          return (xp + adv, yp)
    ) (xpos, ypos) strings
  printErrors


renderCharacter :: TextRenderer -> Character -> Int -> Int -> IO ()
renderCharacter renderer (Character c width height adv text) x y =
  let
    xPos = fromIntegral x
    yPos = fromIntegral y
    lbottom = yPos + fromIntegral (charSize renderer) :: GLfloat
    ltop = lbottom - fromIntegral height :: GLfloat
    w = fromIntegral width
    h = fromIntegral height
    top = yPos + fromIntegral (charSize renderer) - fromIntegral height
    charVerts = [
      xPos, ltop,         0.0, 0.0,
      xPos, lbottom,      0.0, 1.0,
      xPos + w, ltop,     1.0, 0.0,

      xPos, lbottom,      0.0, 1.0,
      xPos + w, lbottom,  1.0, 1.0,
      xPos + w, ltop,     1.0, 0.0] :: [GLfloat]
    vertSize = sizeOf (head charVerts)
    numVerts = length charVerts
    size = fromIntegral (numVerts * vertSize)
    (CharQuad arrayObject arrayBuffer firstIndex numTriangles) = characterQuad renderer
  in
    do
      activeTexture $= TextureUnit 0
      textureBinding Texture2D $= Just text

      bindVertexArrayObject $= Just arrayObject
      bindBuffer ArrayBuffer $= Just arrayBuffer

      textColourU <- GL.get $ uniformLocation (program renderer) "textColor"
      uniform textColourU $= textColour renderer

      (UniformLocation projU) <- GL.get $ uniformLocation (program renderer) "projection"
      with (pMatrix renderer)
        $ GLRaw.glUniformMatrix4fv projU 1 (fromBool True)
        . castPtr
      withArray charVerts $ \ptr ->
        bufferSubData ArrayBuffer WriteToBuffer 0 size ptr
      drawArrays Triangles firstIndex numTriangles
      printErrors

