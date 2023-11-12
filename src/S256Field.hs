{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module S256Field ( S256
                 , s256
                 , FiniteField (..)
                 ) where

-- | Bitcoin operations are made on a very speficic
-- field with p = 2**256 - 2**32 - 977. So, instead
-- of defining a generic field element, we can model
-- the arithmetic more simply.

-- | Internal representation using a generic a so we
-- can define numerical instances more elegantly.
newtype S256Field a = S256Field { ffElem :: a }
  deriving (Show)


-- | Obivous Functor instance
instance Functor S256Field where
  fmap :: (a -> b) -> S256Field a -> S256Field b
  fmap f (S256Field a) = S256Field $ f a

-- | Obvious Applicative instance
instance Applicative S256Field where
  pure :: a -> S256Field a
  pure = S256Field

  (<*>) :: S256Field (a -> b) -> S256Field a -> S256Field b
  (S256Field f) <*> (S256Field a) = S256Field $ f a


-- | The type synomin that actually represent the
-- finite field we are interested in. We are using
-- Integer as the internal representation, but I
-- believe it could be refactored to use fixed size
-- limbs representation.
type S256 = S256Field Integer

-- | Constructor to keep the data type opaque.
s256 :: Integer -> S256
s256 n = normalize $ S256Field n


-- | We define a FiniteField class just because I
-- was using this `mod` p operation all the time.
class FiniteField f where
  -- | Return the prime order over which the finite
  -- field is defined in.
  order :: f -> Integer

  -- | Normalize the number: n `mod` order
  normalize :: f -> f


instance FiniteField S256 where
  order = const $
    (2 :: Integer)^(256 :: Integer) - (2 :: Integer)^(32 :: Integer) - 977
  normalize = s256Normalize


s256Normalize :: S256 -> S256
s256Normalize n = (`mod` order n) <$> n


instance Eq S256 where
  n1 == n2 =
    let S256Field n1' = normalize n1
        S256Field n2' = normalize n2
    in  n1' == n2'

instance Num S256 where
  -- (S256Field n1) + (S256Field n2) = normalize $ S256Field (n1 + n2)
  n1 + n2 = normalize $ (+) <$> n1 <*> n2
  -- (S256Field n1) - (S256Field n2) = normalize $ S256Field (n1 - n2)
  n1 - n2 = normalize $ (-) <$> n1 <*> n2
  -- (S256Field n1) * (S256Field n2) = normalize $ S256Field (n1 * n2)
  n1 * n2 = normalize $ (*) <$> n1 <*> n2
  -- negate n = let S256Field n' = normalize n
  --            in  normalize $ S256Field (0 - n')
  --negate n = normalize $ negate <$> normalize n
  negate n = negate <$> n
  abs = normalize
  -- signum n = case normalize n of
  --   S256Field 0 -> S256Field 0
  --   S256Field _ -> S256Field 1
  signum n = (\k -> if k == 0 then 0 else 1) <$> normalize n
  fromInteger = S256Field

instance Ord S256 where
  n1 <= n2 =
    let S256Field n1' = normalize n1
        S256Field n2' = normalize n2
    in  n1' <= n2'

instance Real S256 where
  -- toRational n =
  --   let S256Field n' = normalize n
  --   in  toRational n'
  toRational n = toRational . ffElem $ normalize n

instance Enum S256 where
  -- | sucessor (normalized)
  -- succ :: a -> a
  succ n = normalize $ n + 1

  -- | predecessor (normalized)
  -- pred :: a -> a
  pred n = normalize $ n - 1

  -- toEnum :: Int -> a
  toEnum k = normalize $ S256Field (fromIntegral k)

  -- fromEnum :: a -> Int
  fromEnum n = let S256Field n' = normalize n
               in  fromEnum n'

  -- enumFrom :: a -> [a]
  enumFrom (S256Field n) =
    let xs = enumFrom n :: [Integer]
    in  map s256 xs

  -- enumFromThen :: a -> a -> [a]
  enumFromThen (S256Field start) (S256Field next) =
    let xs = enumFromThen start next :: [Integer]
    in  map s256 xs

  -- enumFromTo :: a -> a -> [a]
  enumFromTo (S256Field start) (S256Field finish) =
    let xs = enumFromTo start finish :: [Integer]
    in  map s256 xs

  -- enumFromThenTo :: a -> a -> a -> [a]
  enumFromThenTo (S256Field start) (S256Field next) (S256Field finish) =
    let xs = enumFromThenTo start next finish :: [Integer]
    in  map s256 xs


-- instance Integral S256 where
--   quotRem n1 n2 =
--     let S256Field n1' = normalize n1
--         S256Field n2' = normalize n2
--         (q, r) = quotRem n1' n2'
--     in  (S256Field q, S256Field r)

--   toInteger n =
--     let S256Field n' = normalize n
--     in  n'
