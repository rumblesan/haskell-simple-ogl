module TextRendering (
    createTextRenderer
  , renderText
  , renderCharacter
  , renderTextScene
  , TextRenderer
) where

import Control.Monad

import Foreign.Marshal.Utils
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr

import qualified Data.Map.Strict as M
import qualified Data.Char as C

import Graphics.Rendering.OpenGL as GL
import GeometryBuffers (bufferOffset)
import LoadShaders
import FreeType (Character(..), freeType, fontFace, setFaceSize, loadCharacter)
import Graphics.Rendering.FreeType.Internal.Face (FT_Face)
import qualified Graphics.GL as GLRaw

import Data.Vec (Mat44)
import Matrices (orthoMat)

import ErrorHandling (printErrors)
import TextureRenderer (loadTexture)


data CharQuad = CharQuad VertexArrayObject BufferObject ArrayIndex NumArrayIndices deriving (Show, Eq)

data TextRenderer = TextRenderer {
    characters :: M.Map Char Character
  , charWidth :: Int
  , pMatrix :: Mat44 GLfloat
  , program :: Program
  , characterQuad :: CharQuad
  , testTexture :: TextureObject
} deriving (Show, Eq)

getCharacter :: TextRenderer -> Char -> Character
getCharacter (TextRenderer charMap _ _ _ _ _) c = charMap M.! c

createAllCharTextures :: FT_Face -> IO (M.Map Char Character)
createAllCharTextures face =
  let
    chars = C.chr <$> [0..127]
  in
    do
      GL.rowAlignment GL.Pack $= 1
      sequence $ M.fromList $ fmap (\c -> (c, loadCharacter face c)) chars

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

createTextRenderer :: IO TextRenderer
createTextRenderer = do
  let projMat = orthoMat 0.1 100 640 480
  let charWidth = 36
  ft2 <- freeType
  face <- fontFace ft2 "fonts/arial.ttf"
  setFaceSize face charWidth
  cq <- charQuad
  program <- loadShaders [
    ShaderInfo VertexShader (FileSource "shaders/textrenderer.vert"),
    ShaderInfo FragmentShader (FileSource "shaders/textrenderer.frag")]
  characters <- createAllCharTextures face
  testText <- loadTexture "images/simple_texture.bmp"
  return $ TextRenderer characters charWidth projMat program cq testText

renderText :: TextRenderer -> String -> IO ()
renderText renderer strings = do
  currentProgram $= Just (program renderer)
  let
    chars = getCharacter renderer <$> strings
    xStart = 0
    yStart = 0
  foldM_ (\xp c -> 
    do
      renderCharacter renderer c xp yStart
      let (Character _ _ _ adv _) = c
      return $ xp + adv
    ) xStart chars
  printErrors


renderCharacter :: TextRenderer -> Character -> Int -> Int -> IO ()
renderCharacter renderer (Character c width height adv text) x y =
  let
    xPos = fromIntegral x
    yPos = fromIntegral y
    w = fromIntegral width
    h = fromIntegral height
    charVerts = [
      xPos, yPos + h,      0.0, 0.0,
      xPos, yPos,          0.0, 1.0,
      xPos + w, yPos + h,  1.0, 0.0,

      xPos + w, yPos + h,  1.0, 0.0,
      xPos, yPos,          0.0, 1.0,
      xPos + w, yPos,      1.0, 1.0] :: [GLfloat]
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
      let c = Color3 1.0 0.0 0.0 :: Color3 GLfloat
      uniform textColourU $= c

      (UniformLocation projU) <- GL.get $ uniformLocation (program renderer) "projection"
      with (pMatrix renderer)
        $ GLRaw.glUniformMatrix4fv projU 1 (fromBool True)
        . castPtr
      withArray charVerts $ \ptr ->
        bufferSubData ArrayBuffer WriteToBuffer 0 size ptr
      drawArrays Triangles firstIndex numTriangles
      printErrors

renderTextScene :: TextRenderer -> String -> IO ()
renderTextScene trender string = do
  blend $= Enabled
  blendEquationSeparate $= (FuncAdd, FuncAdd)
  blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))
  cullFace $= Nothing
  depthFunc $= Nothing
  clearColor $= Color4 1.0 1.0 1.0 1.0
  clear [ ColorBuffer ]
  bindFramebuffer Framebuffer $= defaultFramebufferObject
  renderText trender string


