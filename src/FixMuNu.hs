{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module FixMuNu where


-- newtype Fix f = Fix { runFix :: f (Fix f) }
newtype Fix f where
  Fix :: f (Fix f) -> Fix f

inFix :: f (Fix f) -> Fix f
inFix = Fix

outFix :: Fix f -> f (Fix f)
outFix (Fix f) = f

cataFix :: Functor f => (f a -> a) -> Fix f -> a
cataFix alg = alg . fmap (cataFix alg) . outFix

-- | Uses rank 2 types
--   newtype Mu f = Mu { runMu :: forall r. (f r -> r) -> r }
newtype Mu f where
  Mu :: (forall a. (f a -> a) -> a) -> Mu f

-- | 'Mu' takes a F-Algebra and returns a value of the carrier type 'a', so we
--   pass it an algebra and get an 'a'.
runMu :: (f a -> a) -> Mu f -> a
runMu alg (Mu f) = f alg

-- | An F-Algebra with carrier 'Mu f'.
inMu :: Functor f => f (Mu f) -> Mu f
inMu fmuf = Mu $ \f -> f (runMu f <$> fmuf)

outMu :: Functor f => Mu f -> f (Mu f)
outMu = runMu $ fmap inMu

-- | It turns out that 'cataMu' == 'runMu' since 'Mu' is morally a Church
--   encoding, but I write it this way for consistency with 'Fix' and 'Nu'.
cataMu :: Functor f => (f a -> a) -> Mu f -> a
cataMu alg = alg . fmap (cataMu alg) . outMu

-- Uses existentail quantification
-- data Nu f = forall x. Nu x (x -> f x)
data Nu f where
  Nu :: (a -> f a) -> a -> Nu f

inNu :: Functor f => f (Nu f) -> Nu f
inNu = Nu (fmap outNu)

outNu :: Functor f => Nu f -> f (Nu f)
outNu (Nu f a) = Nu f <$> f a

cataNu :: Functor f => (f a -> a) -> Nu f -> a
cataNu alg = alg . fmap (cataNu alg) . outNu


instance Show (f (Fix f)) => Show (Fix f) where
  showsPrec p (Fix x) = showParen (p > 10)
                      $ showString "Fix " . showsPrec 11 x

instance (Functor f, Show (Fix f)) => Show (Mu f) where
  showsPrec p mu = showParen (p > 10)
                 $ showString "fixToMu " . showsPrec 11 (muToFix mu)

instance (Functor f, Show (Fix f)) => Show (Nu f) where
  showsPrec p nu = showParen (p > 10)
                 $ showString "fixToNu " . showsPrec 11 (nuToFix nu)

nuToFix :: Functor f => Nu f -> Fix f
nuToFix = inFix . fmap nuToFix . outNu

muToFix :: Functor f => Mu f -> Fix f
muToFix = inFix . fmap muToFix . outMu
-- muToFix muf = inFix (fmap muToFix (outMu (Mu f)))
-- muToFix (Mu f) = inFix (fmap muToFix (runMu $ fmap inMu (Mu f)))
-- muToFix (Mu f) = inFix (fmap muToFix (runMu $ fmap inMu (Mu f)))
-- muToFix (Mu f) = f inFix

fixToMu :: Functor f => Fix f -> Mu f
fixToMu = inMu . fmap fixToMu . outFix

nuToMu :: Functor f => Nu f -> Mu f
nuToMu = inMu . fmap nuToMu . outNu

fixToNu :: Functor f => Fix f -> Nu f
fixToNu = inNu . fmap fixToNu . outFix

muToNu :: Functor f => Mu f -> Nu f
muToNu = inNu . fmap muToNu . outMu
--

-- nuZero, nuOne, nuTwo :: Nu Maybe
-- nuZero = Nu (const Nothing) ()
-- nuOne  = Nu (fmap (const Nothing)) (Just ())
-- nuTwo  = Nu (fmap (fmap (const Nothing))) (Just (Just ()))

-- succFix :: Fix Maybe -> Fix Maybe
-- succFix = Fix . Just

-- -- nuSucc :: Nu Maybe -> Nu Maybe
-- -- nuSucc (Nu x f) = Nu (fmap f) (Just x)

-- fixToInt :: Fix Maybe -> Integer
-- fixToInt (Fix Nothing)     = 0
-- fixToInt (Fix (Just fixm)) = 1 + fixToInt fixm

-- muToInt :: Mu Maybe -> Integer
-- muToInt (Mu f) = f go
--   where
--     go Nothing  = 0
--     go (Just r) = 1 + r

-- nuToInt :: Nu Maybe -> Integer
-- nuToInt (Nu x f) = go x
--   where
--     go y = case f y of
--       Nothing -> 0
--       Just z  -> 1 + go z

-- --

-- zeroFix' :: Fix Maybe
-- zeroFix' = inFix Nothing

-- muZero' :: Mu Maybe
-- muZero' = inMu Nothing

-- nuZero' :: Nu Maybe
-- nuZero' = inNu Nothing

-- succFix' :: Fix Maybe -> Fix Maybe
-- succFix' = inFix . Just

-- muSucc' :: Mu Maybe -> Mu Maybe
-- muSucc' = inMu . Just

-- nuSucc' :: Nu Maybe -> Nu Maybe
-- nuSucc' = inNu . Just

-- fixToInt' :: Fix Maybe -> Integer
-- fixToInt' = maybe 0 (succ . fixToInt') . outFix

-- muToInt' :: Mu Maybe -> Integer
-- muToInt' = maybe 0 (succ . muToInt') . outMu

-- nuToInt' :: Nu Maybe -> Integer
-- nuToInt' = maybe 0 (succ . nuToInt') . outNu