{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module Haspara.Internal.FXQuoteDatabase where

import qualified Data.HashMap.Strict       as HM
import           GHC.TypeLits              (KnownNat, Nat)
import           Haspara.Internal.Currency (CurrencyPair)
import           Haspara.Internal.Date     (Date, addDays)
import           Haspara.Internal.FXQuote  (FXQuote)


type FXQuoteDatabase (n :: Nat) = HM.HashMap CurrencyPair (FXQuotePairDatabase n)


data FXQuotePairDatabase (n :: Nat) = FXQuotePairDatabase
  { fxQuotePairDatabasePair  :: !CurrencyPair
  , fxQuotePairDatabaseTable :: !(HM.HashMap Date (FXQuote n))
  , fxQuotePairDatabaseSince :: !Date
  , fxQuotePairDatabaseUntil :: !Date
  }


findFXQuote :: KnownNat n => FXQuoteDatabase n -> CurrencyPair -> Date -> Maybe (FXQuote n)
findFXQuote db cp d = case HM.lookup cp db of
  Nothing  -> Nothing
  Just pdb -> findFXQuoteAux d pdb


findFXQuoteAux :: KnownNat n => Date -> FXQuotePairDatabase n -> Maybe (FXQuote n)
findFXQuoteAux d db
  | d < fxQuotePairDatabaseSince db = Nothing
  | otherwise = case HM.lookup d (fxQuotePairDatabaseTable db) of
      Nothing -> findFXQuoteAux (addDays (-1) d) db
      Just fx -> Just fx
