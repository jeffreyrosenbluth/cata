{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}

module Cata where

data Fix f = In {out :: f (Fix f)}

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . (fmap (cata alg)) . out

data List e a = Nil | Cons e a deriving Functor

toList :: [e] -> Fix (List e)
toList [] = In Nil
toList (x:xs) = In (Cons x (toList xs))

fromList :: Fix (List e) -> [e]
fromList (In Nil) = []
fromList (In (Cons e es)) = e : fromList es

consAlg :: List e [e] -> [e]
consAlg Nil = []
consAlg (Cons e es) = e : es

addAlg :: List Int Int -> Int
addAlg Nil = 0
addAlg (Cons a b) = a + b

subAlg :: List Int Int -> Int
subAlg Nil = 0
subAlg (Cons a b) = a - b

composeAlg :: List (a -> a) (a -> a) -> (a -> a)
composeAlg Nil        = id
composeAlg (Cons f g) = f . g

foldR :: (a -> b -> b) -> b -> [a] -> b
foldR f b = cata alg . toList
  where
    alg Nil        = b
    alg (Cons a b) = f a b

foldRf :: (a -> b -> b) -> b -> [a] -> b
foldRf f b as = cata alg (toList as) b
  where
    alg Nil        = id
    alg (Cons e g) = f e . g

foldL :: (b -> a -> b) -> b -> [a] -> b
foldL _ b [] = b
foldL f z (x:xs) = foldL f (f z x) xs

foldLf :: (b -> a -> b) -> b -> [a] -> b
foldLf f b as = cata alg (toList as) b
  where
    alg Nil        = id
    alg (Cons e g) = g . flip f e

foldl2 :: (b -> a -> b) -> b -> [a] -> b
foldl2 f z y = go f z (reverse y)
  where
    go _ z [] = z
    go f z (x:xs) = f (go f z xs) x

foldLr :: (b -> a ->b) -> b -> [a] -> b
foldLr f z xs = foldr (flip (.) . flip f) id xs z

foldRr :: (a -> b -> b) -> b -> [a] -> b
foldRr f z xs = foldr ((.) . f) id xs z

decimal :: [Int] -> Int
decimal xs = foldr (flip (.) . \a b -> a + b*10) id xs 0

dec2 :: [Double] -> (Double, Double, Bool)
dec2 xs = foldr f id xs (0, 0, False)
  where
    f =  flip (.) . \a b -> case (a, b) of
      (0, (b', c', False)) -> (b', c', True)
      (a, (b', c', False)) -> (a + b' * 10, c', False)
      (a, (b', c', True )) -> (b', (a + c') / 10, True)

newtype Church a = Church {unChurch :: forall r. r -> (a -> r -> r) -> r}

instance Foldable Church where
  foldr f r (Church xs) = xs r f

instance Functor Church where
  fmap f = foldr (cons . f) nil

instance Show a => Show (Church a) where
  show xs = "|" ++ (show . fromChurch $ xs) ++ "|"

nil :: Church a
nil = Church $ \r f -> r

cons :: a -> Church a -> Church a
cons x xs = Church $ \r f -> f x (unChurch xs r f)

toChurch :: [a] -> Church a
toChurch [] = nil
toChurch (x:xs) = cons x (toChurch xs)

fromChurch :: Church a -> [a]
fromChurch (Church xs) = xs [] (:)

newtype Shrine a = Shrine {unShrine :: forall r. r -> (r -> a -> r) -> r}

empty :: Shrine a
empty = Shrine $ \r f -> r

append :: a -> Shrine a -> Shrine a
append x xs = Shrine $ \r f -> f (unShrine xs r f) x

toShrine :: [a] -> Shrine a
toShrine [] = empty
toShrine (x:xs) = append x (toShrine xs)

fromShrine :: Shrine a -> [a]
fromShrine (Shrine xs) = reverse $ xs [] (flip (:))

instance Show a => Show (Shrine a) where
  show xs = "(" ++ (show . fromShrine $ xs) ++ ")"

foldS :: (b -> a -> b) -> b -> Shrine a -> b
foldS f b (Shrine xs) = xs b f

data Tree e = Leaf e | Branch (Tree e) (Tree e) deriving Show

data TreeF e a = LeafF e | BranchF a a deriving Functor

fromTree :: Tree e -> Fix (TreeF e)
fromTree (Leaf e) = In (LeafF e)
fromTree (Branch s t) = In (BranchF (fromTree s) (fromTree t))

toTree :: Fix (TreeF e) -> Tree e
toTree (In (LeafF e)) = Leaf e
toTree (In (BranchF b1 b2)) = Branch (toTree b1) (toTree b2)


tree :: Tree ()
tree = Branch
         (Leaf ())
         (Branch
            (Branch
              (Leaf ())
              (Leaf ()))
            (Leaf ()))

depthAlg :: TreeF () Int -> Int
depthAlg (LeafF _) = 0
depthAlg (BranchF s t) = 1 + max s t

foldTree :: (b -> b -> b) -> (a -> b) -> Tree a -> b
foldTree _ g (Leaf e) = g e
foldTree f g (Branch b1 b2) = f (foldTree f g b1) (foldTree f g b2)

depth :: Tree () -> Int
depth = foldTree f g
  where
    f s t = 1 + max s t
    g e = 0

data LeftList a = Empty | LCons (LeftList a) a deriving Show

instance Foldable LeftList where
  foldr f z Empty  = z
  foldr f z (LCons xs x) = (flip f) (foldr f z xs) x

data List2 a
  = Null
  | R a (List2 a)
  | L (List2 a) a
  deriving Show

foldList2 :: (a -> b -> b) -> (b -> a -> b) -> b -> List2 a -> b
foldList2 _ _ z Null     = z
foldList2 f g z (R a xs) = f a (foldList2 f g z xs)
foldList2 f g z (L xs a) = g (foldList2 f g z xs) a

zig :: [a] -> List2 a
zig xs = go 0 xs
  where
    go _ [] = Null
    go n (x:xs) = case n `mod` 2 of
      0 -> R x (go (n-1) xs)
      1 -> L (go (n-1) xs) x

