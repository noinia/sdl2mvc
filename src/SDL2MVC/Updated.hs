module SDL2MVC.Updated
  ( Updated(..)
  , combineWith
  ) where

-- | Type indicating whether the model has changed. This is essentially just a 'Maybe
-- model'.
data Updated model = Unchanged | Changed model
                   deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

instance Semigroup model => Monoid (Updated model) where
  mempty = Unchanged

instance Semigroup model => Semigroup (Updated model) where
  (<>) = combineWith id id (<>)

combineWith             :: (a -> c) -> (b -> c) -> (a -> b -> c)
                        -> Updated a -> Updated b -> Updated c
combineWith f g combine = (<.>)
  where
    Unchanged     <.> r           = g <$> r
    (Changed l )  <.> (Changed r) = Changed $ l `combine` r
    l             <.> Unchanged   = f <$> l
