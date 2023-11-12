module FiniteField where

data FieldElement = FE { feNum :: Integer   -- ^ Finite Field value
                       , fePrime :: Integer -- ^ Finite Field order
                       }

instance Eq FieldElement where
  fe1@(FE _ p1) == fe2@(FE _ p2)
    | p1 /= p2 = False
    | otherwise =
        let (FE n1' _) = feNormalize fe1
            (FE n2' _) = feNormalize fe2
        in  n1' == n2'

instance Show FieldElement where
  show (FE n p) = "{" <> show n <> "}_" <> show p


-- | FeNormalize a finite field element so that 0 <= n < p.
feNormalize :: FieldElement -> FieldElement
feNormalize (FE n p) = FE (n `mod` p) p

-- | Build a feNormalized element of the field.
newFieldElement :: Integer      -- ^ Prime order of the field (unchecked)
                -> Integer      -- ^ Element value
                -> FieldElement -- ^ Field element
newFieldElement p n = feNormalize $ FE n p


data FEError = FEErrorNotSameField
  deriving (Eq, Show)

addFieldElement :: FieldElement -> FieldElement -> Either FEError FieldElement
addFieldElement (FE n1 p1) (FE n2 p2)
  | p1 /= p2 = Left FEErrorNotSameField
  | otherwise = Right . feNormalize $ FE (n1 + n2) p1

subFieldElement :: FieldElement -> FieldElement -> Either FEError FieldElement
subFieldElement (FE n1 p1) (FE n2 p2)
  | p1 /= p2 = Left FEErrorNotSameField
  | otherwise = Right . feNormalize $ FE (n1 - n2) p1

mulFieldElement :: FieldElement -> FieldElement -> Either FEError FieldElement
mulFieldElement (FE n1 p1) (FE n2 p2)
  | p1 /= p2 = Left FEErrorNotSameField
  | otherwise = Right . feNormalize $ FE (n1 * n2) p1

powFieldElement :: FieldElement -> Integer -> FieldElement
powFieldElement (FE n p) expon = feNormalize $ newFieldElement p (n^exp')
  where (FE exp' _) = feNormalize $ newFieldElement (p - 1) expon

divFieldElement :: FieldElement -> FieldElement -> Either FEError FieldElement
divFieldElement fe1@(FE _n1 p1) fe2@(FE _n2 p2)
  | p1 /= p2 = Left FEErrorNotSameField
  | otherwise = feNormalize <$> mulFieldElement fe1 (powFieldElement fe2 (p1 - 2))

mulScalarFieldElement :: FieldElement -> Integer -> FieldElement
mulScalarFieldElement (FE n p) scalar = feNormalize $ FE (n * scalar) p
