module PostProcessing where

import Graphics.Rendering.OpenGL as GL

import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr

import LoadShaders
import GeometryBuffers

data PostProcessing = PostProcessing {
  defaultFrameBuffer :: FramebufferObject,
  frameBuffer :: FramebufferObject,
  textO :: TextureObject,
  rbuff :: RenderbufferObject,
  postShaders :: Program,
  renderQuadVAO :: VAO
}

-- 2D positions and texture coordinates
quadVertices :: [GLfloat]
quadVertices = [
    -1,  1, 0, 0,  1,
    -1, -1, 0, 0,  0,
     1,  1, 0, 1,  1,

     1,  1, 0, 1,  1,
    -1, -1, 0, 0,  0,
     1, -1, 0, 1,  0
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

createPostProcessing :: GLint -> GLint -> IO PostProcessing
createPostProcessing width height = do
  fbo <- genObjectName
  bindFramebuffer Framebuffer $= fbo
  text <- genObjectName
  textureBinding Texture2D $= Just text
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  let pd = PixelData RGB UnsignedByte nullPtr
  texImage2D Texture2D NoProxy 0 RGB' (TextureSize2D width height) 0 pd
  framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D text 0
  rbo <- genObjectName
  bindRenderbuffer Renderbuffer $= rbo
  renderbufferStorage Renderbuffer DepthComponent24 (RenderbufferSize width height)
  framebufferRenderbuffer Framebuffer DepthAttachment Renderbuffer rbo

  qvao <- quadVAO
  program <- loadShaders [
    ShaderInfo VertexShader (FileSource "shaders/postprocessing.vert"),
    ShaderInfo FragmentShader (FileSource "shaders/postprocessing.frag")]

  return $ PostProcessing defaultFramebufferObject fbo text rbo program qvao
