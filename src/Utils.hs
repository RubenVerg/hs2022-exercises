module Utils
    ( count
    , count2
    , (!?)
    ) where

{- | GHC 9 comes with @GHC.Utils.Misc.count@, this is GHC 8 tho
-}
count :: (Foldable t, Num n) => (a -> Bool) -> t a -> n
count p =
    foldr inner 0
    where
        inner x a' | p x = a' + 1
        inner _ a'       = a'

count2 :: (Foldable t, Num n) => (a -> Bool) -> (a -> Bool) -> t a -> (n, n)
count2 p q =
    foldr inner (0, 0)
    where
        inner x (a', b') | p x && q x = (a' + 1, b' + 1)
        inner x (a', b') | p x        = (a' + 1, b'    )
        inner x (a', b') |        q x = (a'    , b' + 1)
        inner _ (a', b')              = (a', b')


infixl 9 !?
-- As per https://hackage.haskell.org/package/extra-1.7.10/docs/src/Data.List.Extra.html#%21%3F
{- | Like @!!@, but wraps the result in a @Maybe@ and returns @Nothing@ if there is no element at that position
-}
(!?) :: [a] -> Int -> Maybe a
_ !? n | n < 0 = Nothing
xs !? n = foldr (\x r k -> case k of
    0 -> Just x
    _ -> r (k - 1)) (const Nothing) xs n