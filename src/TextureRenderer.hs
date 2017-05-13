module TextureRenderer where

import Graphics.Rendering.OpenGL as GL
import qualified  Data.ByteString.Unsafe as BSU

import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr
import Codec.BMP

import LoadShaders
import GeometryBuffers (VAO(..), bufferOffset)
import ErrorHandling (printErrors)

data TextureScene = TextureScene VAO TextureObject Program

-- 2D positions and texture coordinates
quadVertices :: [GLfloat]
quadVertices = [
    -1,  1, -1, 0,  1,
    -1, -1, -1, 0,  0,
     1,  1, -1, 1,  1,

     1,  1, -1, 1,  1,
    -1, -1, -1, 0,  0,
     1, -1, -1, 1,  0
  ]

quadVAO :: IO VAO
quadVAO = do
  vao <- genObjectName
  bindVertexArrayObject $= Just vao
  arrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just arrayBuffer
  let
    vertexSize = sizeOf (head quadVertices)
    firstPosIndex = 0
    firstTexIndex = 3 * vertexSize
    vPosition = AttribLocation 0
    vTexCoord = AttribLocation 1
    numVertices = length quadVertices
    size = fromIntegral (numVertices * vertexSize)
    stride = fromIntegral (5 * vertexSize)
  withArray quadVertices $ \ptr ->
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)
  vertexAttribPointer vPosition $= (ToFloat, VertexArrayDescriptor 3 Float stride (bufferOffset firstPosIndex))
  vertexAttribPointer vTexCoord $= (ToFloat, VertexArrayDescriptor 2 Float stride (bufferOffset firstTexIndex))
  vertexAttribArray vPosition $= Enabled
  vertexAttribArray vTexCoord $= Enabled
  return $ VAO vao firstPosIndex 6

loadTexture :: FilePath -> IO TextureObject
loadTexture path = do
  text <- genObjectName
  textureBinding Texture2D $= Just text
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')

  Right image <- readBMP path
  let
    rgba = unpackBMPToRGBA32 image
    dta = unpackBMPToRGBA32 image
    (width, height) = bmpDimensions image
  bPtr <- BSU.unsafeUseAsCString dta $ \cstr -> 
                return (castPtr cstr)
  let pd = PixelData RGBA UnsignedByte bPtr
  let tSize = TextureSize2D (fromIntegral width) (fromIntegral height)
  texImage2D Texture2D NoProxy 0 RGBA' tSize 0 pd
  textureBinding Texture2D $= Nothing
  return text

createTextureScene :: IO TextureScene
createTextureScene = do
  texture <- loadTexture "images/simple_texture.bmp"
  quad <- quadVAO
  program <- loadShaders [
    ShaderInfo VertexShader (FileSource "shaders/textquad.vert"),
    ShaderInfo FragmentShader (FileSource "shaders/textquad.frag")]
  return $ TextureScene quad texture program

renderTextureScene :: TextureScene -> IO ()
renderTextureScene (TextureScene quadVAO texture program) =
  do
    depthFunc $= Nothing
    clearColor $= Color4 1.0 1.0 1.0 1.0
    clear [ ColorBuffer ]
    bindFramebuffer Framebuffer $= defaultFramebufferObject
    currentProgram $= Just program
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just texture

    printErrors

    let (VAO qbo qbai qbn) = quadVAO
    bindVertexArrayObject $= Just qbo
    drawArrays Triangles qbai qbn
    printErrors

