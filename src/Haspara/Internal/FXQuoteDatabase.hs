{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module Haspara.Internal.FXQuoteDatabase where

import qualified Data.Map.Strict           as SM
import           Data.Time                 (Day, addDays)
import           GHC.TypeLits              (KnownNat, Nat)
import           Haspara.Internal.Currency (CurrencyPair)
import           Haspara.Internal.FXQuote  (FXQuote)


type FXQuoteDatabase (n :: Nat) = SM.Map CurrencyPair (FXQuotePairDatabase n)


data FXQuotePairDatabase (n :: Nat) = FXQuotePairDatabase
  { fxQuotePairDatabasePair  :: !CurrencyPair
  , fxQuotePairDatabaseTable :: !(SM.Map Day (FXQuote n))
  , fxQuotePairDatabaseSince :: !Day
  , fxQuotePairDatabaseUntil :: !Day
  }


findFXQuote :: KnownNat n => FXQuoteDatabase n -> CurrencyPair -> Day -> Maybe (FXQuote n)
findFXQuote db cp d = case SM.lookup cp db of
  Nothing  -> Nothing
  Just pdb -> findFXQuoteAux d pdb


findFXQuoteAux :: KnownNat n => Day -> FXQuotePairDatabase n -> Maybe (FXQuote n)
findFXQuoteAux d db
  | d < fxQuotePairDatabaseSince db = Nothing
  | otherwise = case SM.lookup d (fxQuotePairDatabaseTable db) of
      Nothing -> findFXQuoteAux (addDays (-1) d) db
      Just fx -> Just fx
