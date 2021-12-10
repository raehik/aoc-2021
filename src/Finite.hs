{-# LANGUAGE MagicHash, AllowAmbiguousTypes #-}

-- TODO don't export Finite (constructor)
-- TODO what is my binder standard??? I'm confusing myself here
module Finite where

import           GHC.TypeNats
import           GHC.Exts
import           Data.Proxy
import           Data.Kind
import           Unsafe.Coerce
import           Data.Type.Equality
import           GHC.Generics
import qualified Data.Finite        as OfficialFinite

-- | Finite natural. @'Finite' n@ is inhabited by exactly @n@ values. Invariant:
--
-- prop> getFinite x < natVal x
newtype Finite (n :: Natural) = Finite { getFinite :: Natural } deriving (Eq, Ord, Generic, Show)

instance KnownNat n => Num (Finite n) where
    fromInteger = finite . fromInteger

finite :: KnownNat n => Natural -> Finite n
finite x = result
  where result = if   x < natVal result
                 then Finite x
                 else error $ "Finite.finite: can't fit " <> show x <> " in Finite " <> show (natVal result)

-- | Given any 'Finite n', we can use it at runtime as a 'KnownNat m' that comes
--   with a proof @m < n@.
withFinite
    :: forall (n :: Natural) (r :: Type)
    .  Finite n
    -> (forall m. (KnownNat m, CmpNat m n ~ 'LT) => Proxy# m -> r)
    -> r
withFinite x f =
    case someNatVal (getFinite x) of
      SomeNat (_ :: Proxy m) ->
        case unsafeCoerce Refl :: CmpNat m n :~: 'LT of
          Refl -> f (proxy# :: Proxy# m)

convertFinite :: KnownNat n => Finite n -> OfficialFinite.Finite n
convertFinite = OfficialFinite.finite . fromIntegral . getFinite

convertFinite' :: KnownNat n => OfficialFinite.Finite n -> Finite n
convertFinite' = undefined
