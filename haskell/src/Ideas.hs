module Ideas where

import GHC.Natural (Natural)

data Nat
  = Zero
  | PlusOne Nat
  deriving Eq

natToNatural :: Nat -> Natural
natToNatural Zero = 0
natToNatural (PlusOne n) = 1 + natToNatural n

{-
Zero
PlusOne NatUnit -- 1
PlusOne (PlusOne NatUnit) -- 2
PlusOne (PlusOne (PlusOne NatUnit)) -- 3

Zero

   PlusOne
     Zero

 PlusOne
   PlusOne
     Zero
-}

data Sum
  = Unit
  | Sum [Sum]
  deriving (Eq, Show)

{-
Unit -- 1

Sum []  -- 0
Sum [Unit] -- 1
Sum [Unit, Unit] -- 2
Sum [Unit, Unit, Unit] --3

Sum [Sum []] -- 0
Sum [Sum [Unit]] -- 1
Sum [Sum [Unit, Unit]] -- 2

      Sum
       []

      Sum
      [Unit]

      Sum
    [ Sum ]
      []

      Sum
  [   Sum   ]
     [Unit]

      Sum
 [    Sum      ]
   [Unit,Unit]


Sum [Sum [Unit, Unit], Sum []] -- 2

      Sum
 [    Sum      , Sum ]
   [Unit,Unit]    []

-}

sumToNat :: Sum -> Natural
sumToNat Unit = 1
sumToNat (Sum nestedSums) = sum (fmap sumToNat nestedSums)

natToNormalizedSum :: Natural -> Sum
natToNormalizedSum n | n == 0 = Sum []
natToNormalizedSum n | n == 1 = Unit
natToNormalizedSum n = Sum (take (fromIntegral n) $ repeat Unit)

flattenSum :: Sum -> [Sum]
flattenSum Unit = [Unit]
flattenSum (Sum []) = []
flattenSum (Sum nestedSums@(_ : _)) = concatMap flattenSum nestedSums

normalizeSum :: Sum -> Sum
normalizeSum input =
  case flattenSum input of
    [] -> Sum []
    [Unit] -> Unit
    many@(_ : _) -> Sum many
