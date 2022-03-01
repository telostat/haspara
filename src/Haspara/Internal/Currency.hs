-- | This module provides internal definitions for modeling and working with
-- currencies.
--
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Haspara.Internal.Currency where

import           Control.Monad.Except       (MonadError(throwError))
import qualified Data.Aeson                 as Aeson
import           Data.Hashable              (Hashable)
import           Data.String                (IsString(..))
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import qualified Language.Haskell.TH.Syntax as TH
import qualified Text.Megaparsec            as MP


-- * Data Definition
-- &definition

-- | Type encoding for currencies.
newtype Currency = MkCurrency { currencyCode :: T.Text }
  deriving (Eq, Hashable, Ord, TH.Lift)


-- | 'Show' instance for 'Currency'.
--
-- >>> MkCurrency "USD"
-- USD
instance Show Currency where
  show (MkCurrency x) = T.unpack x


-- | 'IsString' instance for 'Currency'.
--
-- >>> "USD" :: Currency
-- USD
instance IsString Currency where
  fromString =  either error id . currency . T.pack


-- | 'Aeson.FromJSON' instance for 'Currency'.
--
-- >>> Aeson.eitherDecode "\"\"" :: Either String Currency
-- Left "Error in $: Currency code error! Expecting at least 3 uppercase characters, but received: \"\""
-- >>> Aeson.eitherDecode "\"ABC\"" :: Either String Currency
-- Right ABC
instance Aeson.FromJSON Currency where
  parseJSON = Aeson.withText "Currency" $ either fail pure . currency


-- | 'Aeson.ToJSON' instance for 'Currency'.
--
-- >>> Aeson.encode (MkCurrency "USD")
-- "\"USD\""
instance Aeson.ToJSON Currency where
  toJSON (MkCurrency c) = Aeson.String c


-- * Constructors
-- &constructors


-- | Smart constructor for 'Currency' values within 'MonadError' context.
--
-- >>> currency "" :: Either String Currency
-- Left "Currency code error! Expecting at least 3 uppercase characters, but received: \"\""
-- >>> currency " " :: Either String Currency
-- Left "Currency code error! Expecting at least 3 uppercase characters, but received: \" \""
-- >>> currency "AB" :: Either String Currency
-- Left "Currency code error! Expecting at least 3 uppercase characters, but received: \"AB\""
-- >>> currency " ABC " :: Either String Currency
-- Left "Currency code error! Expecting at least 3 uppercase characters, but received: \" ABC \""
-- >>> currency "ABC" :: Either String Currency
-- Right ABC
currency :: MonadError String m => T.Text -> m Currency
currency x = either
  (const . throwError $ "Currency code error! Expecting at least 3 uppercase characters, but received: " <> show x)
  (pure . MkCurrency)
  (MP.runParser currencyCodeParser "Currency Code" x)


-- | Smart constructor for 'Currency' values within 'MonadFail' context.
--
-- >>> currencyFail "" :: Maybe Currency
-- Nothing
-- >>> currencyFail "US" :: Maybe Currency
-- Nothing
-- >>> currencyFail "usd" :: Maybe Currency
-- Nothing
-- >>> currencyFail "USD" :: Maybe Currency
-- Just USD
currencyFail :: MonadFail m => T.Text -> m Currency
currencyFail = either fail pure . currency


-- * Auxiliaries
-- &auxiliaries


-- | Parser that parses currency codes.
--
-- >>> MP.runParser currencyCodeParser "Example" ""
-- Left (ParseErrorBundle {bundleErrors = TrivialError 0 (Just EndOfInput) (fromList []) :| [], bundlePosState = PosState {pstateInput = "", pstateOffset = 0, pstateSourcePos = SourcePos {sourceName = "Example", sourceLine = Pos 1, sourceColumn = Pos 1}, pstateTabWidth = Pos 8, pstateLinePrefix = ""}})
-- >>> MP.runParser currencyCodeParser "Example" " "
-- Left (ParseErrorBundle {bundleErrors = TrivialError 0 (Just (Tokens (' ' :| ""))) (fromList []) :| [], bundlePosState = PosState {pstateInput = " ", pstateOffset = 0, pstateSourcePos = SourcePos {sourceName = "Example", sourceLine = Pos 1, sourceColumn = Pos 1}, pstateTabWidth = Pos 8, pstateLinePrefix = ""}})
-- >>> MP.runParser currencyCodeParser "Example" "a"
-- Left (ParseErrorBundle {bundleErrors = TrivialError 0 (Just (Tokens ('a' :| ""))) (fromList []) :| [], bundlePosState = PosState {pstateInput = "a", pstateOffset = 0, pstateSourcePos = SourcePos {sourceName = "Example", sourceLine = Pos 1, sourceColumn = Pos 1}, pstateTabWidth = Pos 8, pstateLinePrefix = ""}})
-- >>> MP.runParser currencyCodeParser "Example" "A"
-- Left (ParseErrorBundle {bundleErrors = TrivialError 1 (Just EndOfInput) (fromList []) :| [], bundlePosState = PosState {pstateInput = "A", pstateOffset = 0, pstateSourcePos = SourcePos {sourceName = "Example", sourceLine = Pos 1, sourceColumn = Pos 1}, pstateTabWidth = Pos 8, pstateLinePrefix = ""}})
-- >>> MP.runParser currencyCodeParser "Example" "AB"
-- Left (ParseErrorBundle {bundleErrors = TrivialError 2 (Just EndOfInput) (fromList []) :| [], bundlePosState = PosState {pstateInput = "AB", pstateOffset = 0, pstateSourcePos = SourcePos {sourceName = "Example", sourceLine = Pos 1, sourceColumn = Pos 1}, pstateTabWidth = Pos 8, pstateLinePrefix = ""}})
-- >>> MP.runParser currencyCodeParser "Example" "ABC"
-- Right "ABC"
currencyCodeParser :: MP.Parsec Void T.Text T.Text
currencyCodeParser = do
  mandatory <- MP.count 3 parserChar
  optionals <- MP.many parserChar
  pure . T.pack $ mandatory <> optionals
  where
    validChars = ['A'..'Z']
    parserChar = MP.oneOf validChars


newtype CurrencyPair = MkCurrencyPair { unCurrencyPair :: (Currency, Currency) }
  deriving (Eq, Hashable, Ord, TH.Lift)


-- | 'Show' instance for currency pairs.
--
-- >>> MkCurrencyPair ("EUR", "USD")
-- EUR/USD
instance Show CurrencyPair where
  show (MkCurrencyPair (x, y)) = show x <> "/" <> show y


toTuple :: CurrencyPair -> (Currency, Currency)
toTuple = unCurrencyPair


baseCurrency :: CurrencyPair -> Currency
baseCurrency = fst . unCurrencyPair


quoteCurrency :: CurrencyPair -> Currency
quoteCurrency = snd . unCurrencyPair


currencyPair :: MonadError String m => Currency -> Currency -> m CurrencyPair
currencyPair c1 c2
  | c1 == c2 = throwError $ "Can not create currency pair from same currencies: " <> show c1 <> " and " <> show c2
  | otherwise = pure (MkCurrencyPair (c1, c2))


currencyPairFail :: MonadFail m => Currency -> Currency -> m CurrencyPair
currencyPairFail = (either fail pure .) . currencyPair
