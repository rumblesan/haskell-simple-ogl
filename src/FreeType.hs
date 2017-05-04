module FreeType where

import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.String

import Control.Monad

import Graphics.Rendering.FreeType.Internal
import Graphics.Rendering.FreeType.Internal.Library
import Graphics.Rendering.FreeType.Internal.Face
import Graphics.Rendering.FreeType.Internal.Vector
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import Graphics.Rendering.FreeType.Internal.GlyphSlot as GS
import Graphics.Rendering.FreeType.Internal.Bitmap as BM
import Graphics.Rendering.FreeType.Internal.BitmapGlyph
import Graphics.Rendering.FreeType.Internal.BitmapSize

import Graphics.Rendering.OpenGL as GL

data Character = Character Char Int Int Int GL.TextureObject deriving Eq

instance Show Character where
  show (Character c _ _ _ _) = show c

runFreeType :: IO FT_Error -> IO ()
runFreeType m = do
    r <- m
    unless (r == 0) $ fail $ "FreeType Error:" ++ show r

freeType :: IO FT_Library
freeType = alloca $ \p -> do
    runFreeType $ ft_Init_FreeType p
    peek p

fontFace :: FT_Library -> FilePath -> IO FT_Face
fontFace ft fp = withCString fp $ \str ->
    alloca $ \ptr -> do
        runFreeType $ ft_New_Face ft str 0 ptr
        peek ptr

setFaceSize :: FT_Face -> Int -> IO ()
setFaceSize ff px = runFreeType $ ft_Set_Pixel_Sizes ff (fromIntegral px) 0

glyphFormatString :: FT_Glyph_Format -> String
glyphFormatString fmt
    | fmt == ft_GLYPH_FORMAT_COMPOSITE = "ft_GLYPH_FORMAT_COMPOSITE"
    | fmt == ft_GLYPH_FORMAT_OUTLINE = "ft_GLYPH_FORMAT_OUTLINE"
    | fmt == ft_GLYPH_FORMAT_PLOTTER = "ft_GLYPH_FORMAT_PLOTTER"
    | fmt == ft_GLYPH_FORMAT_BITMAP = "ft_GLYPH_FORMAT_BITMAP"
    | otherwise = "ft_GLYPH_FORMAT_NONE"

loadCharacter :: FT_Face -> Char -> IO Character
loadCharacter ff char = do
  chNdx <- ft_Get_Char_Index ff $ fromIntegral $ fromEnum char
  runFreeType $ ft_Load_Glyph ff chNdx 0
  slot <- peek $ glyph ff

  runFreeType $ ft_Render_Glyph slot ft_RENDER_MODE_NORMAL
  bmp <- peek $ GS.bitmap slot
  let
    bmpWidth = BM.width bmp
    bmpHeight = BM.rows bmp
  (FT_Vector advx advy) <- peek $ GS.advance slot

  text <- genObjectName
  textureBinding Texture2D $= Just text
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  let pd = PixelData Red UnsignedByte (BM.buffer bmp)
  let tSize = TextureSize2D (fromIntegral bmpWidth) (fromIntegral bmpHeight)
  texImage2D Texture2D NoProxy 0 R8 tSize 0 pd
  textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
  textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
  textureBinding Texture2D $= Nothing
  return $ Character char 36 36 36 text

