module GeometryBuffers where

import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr
import Data.List (genericIndex)

import Graphics.Rendering.OpenGL

triVertexArray :: [Vertex3 GLfloat] -> [(Integer, Integer, Integer)] -> [Vertex3 GLfloat]
triVertexArray verts points =
  reverse $ foldl (\va (p1, p2, p3) -> genericIndex verts p1:genericIndex verts p2:genericIndex verts p3:va) [] points

lineVertexArray :: [Vertex3 GLfloat] -> [(Integer, Integer)] -> [Vertex3 GLfloat]
lineVertexArray verts points =
  reverse $ foldl (\va (p1, p2) -> genericIndex verts p1:genericIndex verts p2:va) [] points


cubeVertices :: GLfloat -> [Vertex3 GLfloat]
cubeVertices s = [
    (Vertex3 (-s) (-s) (-s)), (Vertex3 (-s) (-s) s),
    (Vertex3 s (-s) s),       (Vertex3 s (-s) (-s)),
    (Vertex3 (-s) s (-s)),    (Vertex3 (-s) s s),
    (Vertex3 s s s),          (Vertex3 s s (-s))
  ]

cubeTriangles :: [(Integer, Integer, Integer)]
cubeTriangles = [
    (0, 2, 1), (0, 3, 2),
    (0, 1, 5), (0, 5, 4),
    (1, 2, 6), (2, 6, 5),
    (2, 3, 7), (2, 7, 6),
    (3, 0, 4), (3, 4, 7),
    (4, 5, 6), (6, 7, 4)
  ]


data VAO = VAO VertexArrayObject ArrayIndex NumArrayIndices deriving (Show, Eq)

cubeVAO :: IO VAO
cubeVAO = createBuffer $ triVertexArray (cubeVertices 0.2) cubeTriangles

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

createBuffer :: [Vertex3 GLfloat] -> IO VAO
createBuffer verts = do
  vao <- genObjectName
  bindVertexArrayObject $= Just vao
  arrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just arrayBuffer
  let
    firstIndex = 0
    vPosition = AttribLocation 0
    numVertices = length verts
    vertexSize = sizeOf (head verts)
    size = fromIntegral (numVertices * vertexSize)
  withArray verts $ \ptr ->
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)
  vertexAttribPointer vPosition $= (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset firstIndex))
  vertexAttribArray vPosition $= Enabled
  return $ VAO vao firstIndex (fromIntegral numVertices)
