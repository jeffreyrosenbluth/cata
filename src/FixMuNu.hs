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

-- | Uses rank 2 types
--   newtype Mu f = Mu { runMu :: forall r. (f r -> r) -> r }
newtype Mu f where
  Mu :: (forall a. (f a -> a) -> a) -> Mu f

inMu :: Functor f => f (Mu f) -> Mu f
inMu fmuf = Mu (\f -> f (fmap (cataMu f) fmuf))

outMu :: Functor f => Mu f -> f (Mu f)
outMu = cataMu (fmap inMu)

cataMu :: (f a -> a) -> Mu f -> a
cataMu alg (Mu f) = f alg

-- Uses existentail quantification
-- data Nu f = forall x. Nu x (x -> f x)
data Nu f where
  Nu :: (a -> f a) -> a -> Nu f

inNu :: Functor f => f (Nu f) -> Nu f
inNu = Nu (fmap outNu)
-- inNu fnuf = Nu $ fnuf (fmap outNu)

outNu :: Functor f => Nu f -> f (Nu f)
outNu (Nu f a) = Nu f <$> f a
-- outNu (Nu x f) = fmap (\a -> Nu a f) (f x)

instance Show (f (Fix f)) => Show (Fix f) where
  showsPrec p (Fix x) = showParen (p > 10)
                      $ showString "Fix " . showsPrec 11 x

instance Show (Fix f) => Show (Mu f) where
  showsPrec p mu = showParen (p > 10)
                 $ showString "fixMu " . showsPrec 11 (muFix mu)

instance (Functor f, Show (Fix f)) => Show (Nu f) where
  showsPrec p nu = showParen (p > 10)
                 $ showString "fixNu " . showsPrec 11 (nuFix nu)

fixMu :: Functor f => Fix f -> Mu f
fixMu = inMu . fmap fixMu . outFix

nuMu :: Functor f => Nu f -> Mu f
nuMu = inMu . fmap nuMu . outNu

nuFix :: Functor f => Nu f -> Fix f
nuFix = inFix . fmap nuFix . outNu

muFix :: Mu f -> Fix f
muFix (Mu f) = f inFix

muNu :: Functor f => Mu f -> Nu f
muNu (Mu f) = f inNu

fixNu :: Fix f -> Nu f
fixNu = (`Nu` outFix)

--

fixZero, fixOne, fixTwo :: Fix Maybe
fixZero = Fix Nothing
fixOne  = Fix . Just . Fix $ Nothing
fixTwo  = Fix . Just . Fix . Just . Fix $ Nothing

muZero, muOne, muTwo :: Mu Maybe
muZero = Mu $ \f -> f Nothing
muOne  = Mu $ \f -> f . Just . f $ Nothing
muTwo  = Mu $ \f -> f . Just . f . Just . f $ Nothing

nuZero, nuOne, nuTwo :: Nu Maybe
nuZero = Nu () (const Nothing)
nuOne  = Nu (Just ()) (fmap (const Nothing))
nuTwo  = Nu (Just (Just ())) (fmap (fmap (const Nothing)))

fixSucc :: Fix Maybe -> Fix Maybe
fixSucc = Fix . Just

muSucc :: Mu Maybe -> Mu Maybe
muSucc mum = Mu (\f -> f (Just (mum `runMu` f)))

nuSucc :: Nu Maybe -> Nu Maybe
nuSucc (Nu x f) = Nu (Just x) (fmap f)

fixToInt :: Fix Maybe -> Integer
fixToInt (Fix Nothing)     = 0
fixToInt (Fix (Just fixm)) = 1 + fixToInt fixm

muToInt :: Mu Maybe -> Integer
muToInt (Mu f) = f go
  where
    go Nothing  = 0
    go (Just r) = 1 + r

nuToInt :: Nu Maybe -> Integer
nuToInt (Nu x f) = go x
  where
    go y = case f y of
      Nothing -> 0
      Just z  -> 1 + go z

--

fixZero' :: Fix Maybe
fixZero' = inFix Nothing

muZero' :: Mu Maybe
muZero' = inMu Nothing

nuZero' :: Nu Maybe
nuZero' = inNu Nothing

fixSucc' :: Fix Maybe -> Fix Maybe
fixSucc' = inFix . Just

muSucc' :: Mu Maybe -> Mu Maybe
muSucc' = inMu . Just

nuSucc' :: Nu Maybe -> Nu Maybe
nuSucc' = inNu . Just

fixToInt' :: Fix Maybe -> Integer
fixToInt' = maybe 0 (succ . fixToInt') . outFix

muToInt' :: Mu Maybe -> Integer
muToInt' = maybe 0 (succ . muToInt') . outMu

nuToInt' :: Nu Maybe -> Integer
nuToInt' = maybe 0 (succ . nuToInt') . outNu