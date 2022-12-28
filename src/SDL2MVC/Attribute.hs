module SDL2MVC.Attribute
  ( Attribute(..)
  , Color, toRGBA

  , AttrAssignment
  , pattern (:=>)

  , Attributes
  , fromList, toAttrList
  , attr
  , (!?)
  , foldMapAttrs, foldrAttrs
  ) where

--------------------------------------------------------------------------------
import           Data.Traversable
import           Control.Lens
import           Data.Colour
import qualified Data.Colour as Colour
import           Data.Colour.SRGB
import           Data.Constraint (Dict(..))
import           Data.Constraint.Extras (ConstraintsFor(..))
import qualified Data.Dependent.Map as DMap
import qualified Data.Dependent.Sum as DSum
import           Data.GADT.Compare
import           Data.GADT.Show
import qualified SDL2MVC.Event as Event
import           Linear.V4 (V4(..))

--------------------------------------------------------------------------------

type Color = AlphaColour Double

-- | retursn the R,G,B, and a values in in the range [0,1]
toRGBA   :: Color -> V4 Double
toRGBA c = let c'        = alphaColourConvert c
               a         = alphaChannel c'
               RGB r g b = toSRGB (c' `Colour.over` black)
           in V4 r g b a


-- | The various possible attributes
data Attribute action v where
  Fill    ::                Attribute action Color
  Stroke  ::                Attribute action Color
  OnEvent :: Event.Event -> Attribute action action

deriving instance Eq   (Attribute action v)
deriving instance Ord  (Attribute action v)
deriving instance Show (Attribute action v)




instance GShow (Attribute action) where
  gshowsPrec = showsPrec

instance GEq (Attribute action) where
  geq Fill         Fill         = Just Refl
  geq Stroke       Stroke       = Just Refl
  geq (OnEvent e)  (OnEvent e')
    | e == e'                   = Just Refl
  geq _            _            = Nothing

instance GCompare (Attribute action) where
  gcompare Fill         Fill         = GEQ
  gcompare Fill         _            = GLT

  gcompare Stroke       Fill         = GGT
  gcompare Stroke       Stroke       = GEQ
  gcompare Stroke       _            = GLT

  gcompare (OnEvent _)  Fill         = GGT
  gcompare (OnEvent _)  Stroke       = GGT
  gcompare (OnEvent e)  (OnEvent e') = case compare e e' of
                                         LT -> GLT
                                         EQ -> GEQ
                                         GT -> GGT

instance ArgDict c (Attribute action) where
  type ConstraintsFor (Attribute action) c =
    (c Color, c action)
  argDict = \case
    Fill       -> Dict
    Stroke     -> Dict
    OnEvent {} -> Dict

-- | Attributes
newtype Attributes action = Attributes (DMap.DMap (Attribute action) Identity)
  deriving newtype (Show,Eq,Semigroup,Monoid)

-- | An Attribute assignement, i.e. an attribute together with its value
type AttrAssignment action = DSum.DSum (Attribute action) Identity

-- | A convenient way of constructing Attribute assignments.
pattern (:=>)   :: Attribute action v -> v -> AttrAssignment action
pattern k :=> v = (k DSum.:=> Identity v)
{-# COMPLETE  (:=>) #-}

-- | Traverse an attribute assignment
travAssignment   :: Applicative f
                 => (action -> f action') -> AttrAssignment action -> f (AttrAssignment action')
travAssignment f = \case
    Fill      :=> v -> pure $ Fill      :=> v
    Stroke    :=> v -> pure $ Stroke    :=> v
    OnEvent e :=> v -> (OnEvent e :=>) <$> f v

instance Functor Attributes where
  fmap = fmapDefault

instance Foldable Attributes where
  foldMap = foldMapDefault

instance Traversable Attributes where
  traverse f = fmap fromList . traverse (travAssignment f) . toAttrList

-- | Creates the attributes from the list of assignments
fromList :: [AttrAssignment action] -> Attributes action
fromList = foldMap (\(k :=> v) -> attr k v)

-- | Produces a list of assignments
toAttrList                :: Attributes action -> [AttrAssignment action]
toAttrList (Attributes m) = DMap.toAscList m

-- | Create a singleton attribute
attr     :: Attribute action v -> v -> Attributes action
attr k v = Attributes $ DMap.singleton k (Identity v)

-- | lookup a particular attribute.
(!?) :: Attributes action -> Attribute action v -> Maybe v
(Attributes m) !? k = runIdentity <$> DMap.lookup k m

infixl 9 !?

-- | Fold over the attributes with the attribute names.
foldMapAttrs   :: Monoid m
               => (AttrAssignment action -> m) -> Attributes action ->  m
foldMapAttrs f = foldMap f . toAttrList

-- | Fold over the attributes with the attribute names.
foldrAttrs     :: (AttrAssignment action -> b -> b) -> b -> Attributes action ->  b
foldrAttrs f z = foldr f z . toAttrList