-- | Utilities: common sin of programming, repository for a grab-bag of unrelated functions
module CSE.Utilities where

onRightOf :: (r -> rp) -> (l, r) -> (l, rp)
onRightOf f (l, r) = (l, f r)

tryReplace :: a -> (a -> Maybe a) -> a
tryReplace x f = maybe x id (f x)