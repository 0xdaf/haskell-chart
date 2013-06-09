{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -XTemplateHaskell #-}

module Graphics.Rendering.Chart.Types
  ( CRender
  , CEnv(..)
  , runCRender
  , c
  
  , LineCap(..)
  , LineJoin(..)
  , LineStyle(..)
  
  , FontWeight(..)
  , FontSlant(..)
  , FontStyle(..)
  
  , PointShape(..)
  , PointStyle(..)
  
  , FillStyle(..)

  , HTextAnchor(..)
  , VTextAnchor(..)
  , FontWeight(..)
  ) where

import Data.Colour
import Data.Accessor
import Data.Accessor.Template

import Control.Monad.Reader

import qualified Graphics.Rendering.Cairo as C

import Graphics.Rendering.Chart.Geometry

-- -----------------------------------------------------------------------
-- Render Monad
-- -----------------------------------------------------------------------

-- | The environment present in the CRender Monad.
data CEnv = CEnv {
    -- | An adjustment applied immediately prior to points
    --   being displayed in device coordinates.
    --
    --   When device coordinates correspond to pixels, a cleaner
    --   image is created if this transform rounds to the nearest
    --   pixel. With higher-resolution output, this transform can
    --   just be the identity function.
    cenv_point_alignfn :: Point -> Point,

    -- | A adjustment applied immediately prior to coordinates
    --   being transformed.
    cenv_coord_alignfn :: Point -> Point
}

-- | The reader monad containing context information to control
--   the rendering process.
newtype CRender a = DR (ReaderT CEnv C.Render a)
  deriving (Functor, Monad, MonadReader CEnv)

runCRender :: CRender a -> CEnv -> C.Render a
runCRender (DR m) e = runReaderT m e

c :: C.Render a -> CRender a
c = DR . lift

-- -----------------------------------------------------------------------
-- Line Types
-- -----------------------------------------------------------------------

-- | The different supported line ends.
data LineCap = LineCapButt   -- ^ Just cut the line straight.
             | LineCapRound  -- ^ Make a rounded line end.
             | LineCapSquare -- ^ Make a square that ends the line.

-- | The different supported ways to join line ends.
data LineJoin = LineJoinMiter -- ^ Extends the outline until they meet each other.
              | LineJoinRound -- ^ Draw a circle fragment to connet line end.
              | LineJoinBevel -- ^ Like miter, but cuts it off if a certain 
                              --   threshold is exceeded.

-- | Data type for the style of a line.
data LineStyle = LineStyle {
   line_width_  :: Double,
   line_color_  :: AlphaColour Double,
   line_dashes_ :: [Double],
   line_cap_    :: LineCap,
   line_join_   :: LineJoin
}

-- -----------------------------------------------------------------------
-- Point Types
-- -----------------------------------------------------------------------

data PointShape = PointShapeCircle
                | PointShapePolygon Int Bool -- ^ Number of vertices and is right-side-up?
                | PointShapePlus
                | PointShapeCross
                | PointShapeStar

-- | Abstract data type for the style of a plotted point.
--
--   The contained Cairo action draws a point in the desired
--   style, at the supplied device coordinates.
data PointStyle = PointStyle 
  { point_color_ :: AlphaColour Double
  , point_border_color_ :: AlphaColour Double
  , point_border_width_ :: Double
  , point_radius_ :: Double
  , point_shape_ :: PointShape
  }

-- -----------------------------------------------------------------------
-- Font & Text Types
-- -----------------------------------------------------------------------

-- | The possible slants of a font.
data FontSlant = FontSlantNormal  -- ^ Normal font style without slant.
               | FontSlantItalic  -- ^ With a slight slant.
               | FontSlantOblique -- ^ With a greater slant.

-- | The possible weights of a font.
data FontWeight = FontWeightNormal -- ^ Normal font style without weight.
                | FontWeightBold   -- ^ Bold font.

-- | Data type for a font.
data FontStyle = FontStyle {
      font_name_   :: String,
      font_size_   :: Double,
      font_slant_  :: FontSlant,
      font_weight_ :: FontWeight,
      font_color_  :: AlphaColour Double
}

data HTextAnchor = HTA_Left | HTA_Centre | HTA_Right
data VTextAnchor = VTA_Top | VTA_Centre | VTA_Bottom | VTA_BaseLine

-- -----------------------------------------------------------------------
-- Fill Types
-- -----------------------------------------------------------------------

-- | Abstract data type for a fill style.
--
--   The contained Cairo action sets the required fill
--   style in the Cairo rendering state.
newtype FillStyle = FillStyleSolid { fill_colour_ :: AlphaColour Double }

-- -----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor
-- for each field.
$( deriveAccessors '' LineStyle )
$( deriveAccessors '' FontStyle )
