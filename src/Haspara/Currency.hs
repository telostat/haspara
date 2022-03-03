-- | This module provides definitions for modeling and working with currencies.

{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}

module Haspara.Currency where

import           Control.Monad.Except       (MonadError(throwError))
import qualified Data.Aeson                 as Aeson
import           Data.Hashable              (Hashable)
import           Data.String                (IsString(..))
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import qualified Deriving.Aeson.Stock       as DAS
import qualified Language.Haskell.TH.Syntax as TH
import qualified Text.Megaparsec            as MP


-- * Currency
-- $currency


-- | Type encoding for currency symbol values with a syntax of @[A-Z]{3}[A-Z]*@.
--
-- 'Currency' values can be constructed via 'mkCurrencyError' that works in
-- @'MonadError' 'T.Text'@ context:
--
-- >>> mkCurrencyError "EUR" :: Either T.Text Currency
-- Right EUR
--
-- ... or via 'mkCurrencyFail' that works in 'MonadFail' context:
--
-- >>> mkCurrencyFail "EUR" :: Maybe Currency
-- Just EUR
--
-- An 'IsString' instance is provided as well which is unsafe but convenient:
--
-- >>> "EUR" :: Currency
-- EUR
newtype Currency = MkCurrency { currencyCode :: T.Text }
  deriving (Eq, Hashable, Ord, TH.Lift)


-- | 'IsString' instance for 'Currency'.
--
-- >>> "USD" :: Currency
-- USD
instance IsString Currency where
  fromString =  either (error . T.unpack) id . mkCurrencyError . T.pack


-- | 'Show' instance for 'Currency'.
--
-- >>> "USD" :: Currency
-- USD
instance Show Currency where
  show (MkCurrency x) = T.unpack x


-- | 'Aeson.FromJSON' instance for 'Currency'.
--
-- >>> Aeson.eitherDecode "\"\"" :: Either String Currency
-- Left "Error in $: Currency code error! Expecting at least 3 uppercase ASCII letters, but received: "
-- >>> Aeson.eitherDecode "\"A\"" :: Either String Currency
-- Left "Error in $: Currency code error! Expecting at least 3 uppercase ASCII letters, but received: A"
-- >>> Aeson.eitherDecode "\"AB\"" :: Either String Currency
-- Left "Error in $: Currency code error! Expecting at least 3 uppercase ASCII letters, but received: AB"
-- >>> Aeson.eitherDecode "\"ABC\"" :: Either String Currency
-- Right ABC
-- >>> Aeson.eitherDecode "\"ABCD\"" :: Either String Currency
-- Right ABCD
instance Aeson.FromJSON Currency where
  parseJSON = Aeson.withText "Currency" $ either (fail . T.unpack) pure . mkCurrencyError


-- | 'Aeson.ToJSON' instance for 'Currency'.
--
-- >>> Aeson.encode ("USD" :: Currency)
-- "\"USD\""
instance Aeson.ToJSON Currency where
  toJSON (MkCurrency c) = Aeson.String c


-- | Smart constructor for 'Currency' values within 'MonadError' context.
--
-- >>> mkCurrencyError "" :: Either T.Text Currency
-- Left "Currency code error! Expecting at least 3 uppercase ASCII letters, but received: "
-- >>> mkCurrencyError " " :: Either T.Text Currency
-- Left "Currency code error! Expecting at least 3 uppercase ASCII letters, but received:  "
-- >>> mkCurrencyError "AB" :: Either T.Text Currency
-- Left "Currency code error! Expecting at least 3 uppercase ASCII letters, but received: AB"
-- >>> mkCurrencyError " ABC " :: Either T.Text Currency
-- Left "Currency code error! Expecting at least 3 uppercase ASCII letters, but received:  ABC "
-- >>> mkCurrencyError "ABC" :: Either T.Text Currency
-- Right ABC
mkCurrencyError :: MonadError T.Text m => T.Text -> m Currency
mkCurrencyError x = either
  (const . throwError $ "Currency code error! Expecting at least 3 uppercase ASCII letters, but received: " <> x)
  (pure . MkCurrency)
  (MP.runParser currencyCodeParser "Currency Code" x)


-- | Smart constructor for 'Currency' values within 'MonadFail' context.
--
-- >>> mkCurrencyFail "" :: Maybe Currency
-- Nothing
-- >>> mkCurrencyFail "US" :: Maybe Currency
-- Nothing
-- >>> mkCurrencyFail "usd" :: Maybe Currency
-- Nothing
-- >>> mkCurrencyFail "USD" :: Maybe Currency
-- Just USD
mkCurrencyFail :: MonadFail m => T.Text -> m Currency
mkCurrencyFail = either (fail . T.unpack) pure . mkCurrencyError


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
-- >>> MP.runParser currencyCodeParser "Example" "ABCD"
-- Right "ABCD"
-- >>> MP.runParser currencyCodeParser "Example" " ABCD "
-- Left (ParseErrorBundle {bundleErrors = TrivialError 0 (Just (Tokens (' ' :| ""))) (fromList []) :| [], bundlePosState = PosState {pstateInput = " ABCD ", pstateOffset = 0, pstateSourcePos = SourcePos {sourceName = "Example", sourceLine = Pos 1, sourceColumn = Pos 1}, pstateTabWidth = Pos 8, pstateLinePrefix = ""}})
currencyCodeParser :: MP.Parsec Void T.Text T.Text
currencyCodeParser = do
  mandatory <- MP.count 3 validChar
  optionals <- MP.many validChar
  pure . T.pack $ mandatory <> optionals
  where
    validChar = MP.oneOf ['A'..'Z']


-- * Currency Pair
-- $currencyPair


-- | Type encoding of a currency pair.
--
-- 'CurrencyPair' values are constructed via the data constructor:
--
-- >>> CurrencyPair "EUR" "USD"
-- EUR/USD
--
-- 'Aeson.FromJSON' and 'Aeson.ToJSON' instances are provided as well:
--
-- >>> Aeson.decode "{\"base\": \"EUR\", \"quote\": \"EUR\"}" :: Maybe CurrencyPair
-- Just EUR/EUR
-- >>> Aeson.encode (CurrencyPair "EUR" "USD")
-- "{\"base\":\"EUR\",\"quote\":\"USD\"}"
data CurrencyPair = CurrencyPair
  { currencyPairBase  :: !Currency  -- ^ /Base currency/ of the currency pair. Also referred to as /counter currency/.
  , currencyPairQuote :: !Currency  -- ^ /Quote currency/ of the currency pair. Also referred to as /transaction currency/.
  }
  deriving (Eq, DAS.Generic, Ord, TH.Lift)
  deriving (DAS.FromJSON, DAS.ToJSON) via DAS.PrefixedSnake "currencyPair" CurrencyPair


-- | 'Show' instance for 'CurrencyPair'.
--
-- >>> CurrencyPair "EUR" "USD"
-- EUR/USD
instance Show CurrencyPair where
  show (CurrencyPair x y) = show x <> "/" <> show y


-- | Converts a 'CurrencyPair' to a 2-tuple of 'Currency' values.
--
-- >>> toCurrencyTuple (CurrencyPair "EUR" "USD")
-- (EUR,USD)
toCurrencyTuple :: CurrencyPair -> (Currency, Currency)
toCurrencyTuple (CurrencyPair x y) = (x, y)


-- | Converts a 2-tuple of 'Currency' values to a 'CurrencyPair'.
--
-- >>> fromCurrencyTuple ("EUR", "USD")
-- EUR/USD
fromCurrencyTuple :: (Currency, Currency) -> CurrencyPair
fromCurrencyTuple = uncurry CurrencyPair
