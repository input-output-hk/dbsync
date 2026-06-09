{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Parsed shape of an off-chain governance anchor document.
--
-- CIP-100 is the envelope (@authors@, @hashAlgorithm@, @body@,
-- @\@context@). CIP-108 specialises the body for gov-action anchors
-- (title / abstract / motivation / rationale). CIP-119 specialises it
-- for DRep anchors (givenName / objectives / …).
--
-- 'eitherDecodeOffChainVoteData' picks the specialised decoder based
-- on the on-chain 'AnchorType' and falls back to the minimal
-- CIP-100-only shape when the specialised parse fails. Conflicts
-- with the database row type of the same name are handled at use
-- sites via qualified imports (idiomatically @import qualified
-- DbSync.Worker.OffChain.Vote.Types as Vote@).
module DbSync.Worker.OffChain.Vote.Types
  ( OffChainVoteData (..)
  , OffChainVoteDataTp (..)
  , MinimalBody (..)
  , GABody (..)
  , DrepBody (..)
  , Author (..)
  , Witness (..)
  , Reference (..)
  , ReferenceHash (..)
  , ExternalUpdate (..)
  , Image (..)
  , TextValue (..)
  , BoolValue (..)

    -- * Anchor-type dispatch
  , eitherDecodeOffChainVoteData

    -- * Accessors used by the worker's persistence step
  , getAuthors
  , getMinimalBody
  , getLanguage
  ) where

import Cardano.Prelude

import Control.Monad.Fail (fail)
import Data.Aeson (FromJSON (..), Object, Value, eitherDecode', withObject, (.!=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Key, Parser)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text

import DbSync.Db.Types (AnchorType (..))

-- ---------------------------------------------------------------------------
-- * Top-level discriminated union
-- ---------------------------------------------------------------------------

data OffChainVoteData
  = OffChainVoteDataOther (OffChainVoteDataTp OtherOffChainData)
  | OffChainVoteDataGa    (OffChainVoteDataTp GovernanceOffChainData)
  | OffChainVoteDataDr    (OffChainVoteDataTp DrepOffChainData)

deriving stock instance Show OffChainVoteData

-- | For gov-action and drep anchors, try the specialised decoder
-- first and fall back to the CIP-100-only shape if it fails so that
-- minimally-CIP-conforming documents still parse.
eitherDecodeOffChainVoteData
  :: LBS.ByteString -> AnchorType -> Either [Char] OffChainVoteData
eitherDecodeOffChainVoteData lbs = \case
  GovActionAnchor ->
    pickAlternative
      (OffChainVoteDataGa    <$> eitherDecode' lbs)
      (OffChainVoteDataOther <$> eitherDecode' lbs)
  DrepAnchor ->
    pickAlternative
      (OffChainVoteDataDr    <$> eitherDecode' lbs)
      (OffChainVoteDataOther <$> eitherDecode' lbs)
  VoteAnchor           -> OffChainVoteDataOther <$> eitherDecode' lbs
  CommitteeDeRegAnchor -> OffChainVoteDataOther <$> eitherDecode' lbs
  OtherAnchor          -> OffChainVoteDataOther <$> eitherDecode' lbs
  ConstitutionAnchor   -> Left "constitution anchors are not fetched"

pickAlternative
  :: Either [Char] OffChainVoteData
  -> Either [Char] OffChainVoteData
  -> Either [Char] OffChainVoteData
pickAlternative one two = case one of
  Right _ -> one
  Left e1 -> case two of
    Right _ -> two
    Left e2 -> Left $ e1 <> "; CIP-100 fallback: " <> e2

-- ---------------------------------------------------------------------------
-- * Accessors (drive worker's subtable writes)
-- ---------------------------------------------------------------------------

getAuthors :: OffChainVoteData -> [Author]
getAuthors = \case
  OffChainVoteDataOther b -> authors b
  OffChainVoteDataGa    b -> authors b
  OffChainVoteDataDr    b -> authors b

getLanguage :: OffChainVoteData -> Text
getLanguage = \case
  OffChainVoteDataOther b -> language (context b)
  OffChainVoteDataGa    b -> language (context b)
  OffChainVoteDataDr    b -> language (context b)

-- | Project any body variant onto the shared (references, comment,
-- external-updates) shape. The phantom tag is erased; 'Reference'
-- lists merge cleanly because their structure is identical.
getMinimalBody :: OffChainVoteData -> MinimalBody OtherOffChainData
getMinimalBody = \case
  OffChainVoteDataOther b -> body b
  OffChainVoteDataGa    b -> coerceMinimalBody @GovernanceOffChainData $ toMinimal $ body b
  OffChainVoteDataDr    b -> coerceMinimalBody @DrepOffChainData $ toMinimal $ body b

-- ---------------------------------------------------------------------------
-- * Envelope
-- ---------------------------------------------------------------------------

-- | CIP-100 envelope. 'tp' is a phantom tag selecting the body shape.
data OffChainVoteDataTp tp = OffChainVoteDataTp
  { hashAlgorithm :: !TextValue
  , authors       :: ![Author]
  , body          :: !(Body tp)
  , context       :: !Context
  }

deriving stock instance Show (Body tp) => Show (OffChainVoteDataTp tp)

data Author = Author
  { name    :: !(Maybe TextValue)
  , witness :: !Witness
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data Witness = Witness
  { witnessAlgorithm :: !TextValue
  , publicKey        :: !TextValue
  , signature        :: !TextValue
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

-- | Body fields common to every variant.
data MinimalBody tp = MinimalBody
  { references      :: !(Maybe [Reference tp])
  , comment         :: !(Maybe TextValue)
  , externalUpdates :: !(Maybe [ExternalUpdate])
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

-- | Erase the phantom tag on a 'MinimalBody'. Used by 'getMinimalBody'.
coerceMinimalBody :: MinimalBody tp -> MinimalBody tp'
coerceMinimalBody mb = MinimalBody
  { references      = map coerceReference <$> references mb
  , comment         = comment mb
  , externalUpdates = externalUpdates mb
  }
  where
    coerceReference rf = Reference
      { rtype         = rtype rf
      , label         = label rf
      , uri           = uri rf
      , referenceHash = referenceHash rf
      }

-- ---------------------------------------------------------------------------
-- * Body variants
-- ---------------------------------------------------------------------------

data GABody = GABody
  { gabMinimal :: !(MinimalBody GovernanceOffChainData)
  , title      :: !TextValue  -- 80 chars max
  , abstract   :: !TextValue  -- 2500 chars max
  , motivation :: !TextValue
  , rationale  :: !TextValue
  }
  deriving stock (Show)

data DrepBody = DrepBody
  { drbMinimal     :: !(MinimalBody DrepOffChainData)
  , paymentAddress :: !(Maybe TextValue)
  , givenName      :: !TextValue  -- 80 chars max
  , image          :: !(Maybe Image)
  , objectives     :: !(Maybe TextValue)  -- 1000 chars max
  , motivations    :: !(Maybe TextValue)  -- 1000 chars max
  , qualifications :: !(Maybe TextValue)  -- 1000 chars max
  , doNotList      :: !(Maybe BoolValue)
  }
  deriving stock (Show)

-- | 'content' can be either a real URL with companion sha256, or a
-- @data:@-URI carrying the bytes inline. The latter is normalised
-- into 'Image' with @msha256 = Nothing@.
data Image = Image
  { content :: !TextValue
  , msha256 :: !(Maybe TextValue)
  }
  deriving stock (Show)

data ImageUrl = ImageUrl
  { contentUrl :: !TextValue
  , sha256     :: !TextValue
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

fromImageUrl :: ImageUrl -> Image
fromImageUrl img = Image (contentUrl img) (Just (sha256 img))

data Reference tp = Reference
  { rtype         :: !TextValue
  , label         :: !TextValue
  , uri           :: !TextValue
  , referenceHash :: !(Maybe ReferenceHash)
  }
  deriving stock (Show)

data ReferenceHash = ReferenceHash
  { hashDigest      :: !TextValue
  , rhHashAlgorithm :: !TextValue
  }
  deriving stock (Show)

data ExternalUpdate = ExternalUpdate
  { euTitle :: !TextValue
  , euUri   :: !TextValue
  }
  deriving stock (Show)

-- ---------------------------------------------------------------------------
-- * Phantom tags + HasBody dispatch
-- ---------------------------------------------------------------------------

data OtherOffChainData
data GovernanceOffChainData
data DrepOffChainData

-- | Associates each body variant with its allowed reference types
-- and a projection back to 'MinimalBody'.
class HasBody tp where
  type Body tp
  parseAuthors :: Object -> Parser [Author]
  referenceTypes :: [Text]
  toMinimal :: Body tp -> MinimalBody tp

instance HasBody OtherOffChainData where
  type Body OtherOffChainData = MinimalBody OtherOffChainData
  parseAuthors o = o .: "authors"
  referenceTypes = ["Other", "GovernanceMetadata"]
  toMinimal = identity

instance HasBody GovernanceOffChainData where
  type Body GovernanceOffChainData = GABody
  parseAuthors o = o .: "authors"
  referenceTypes = ["Other", "GovernanceMetadata"]
  toMinimal = gabMinimal

instance HasBody DrepOffChainData where
  type Body DrepOffChainData = DrepBody
  parseAuthors _ = pure []
  referenceTypes = ["Other", "GovernanceMetadata", "Identity", "Link"]
  toMinimal = drbMinimal

-- ---------------------------------------------------------------------------
-- * FromJSON instances
-- ---------------------------------------------------------------------------

instance (HasBody tp, FromJSON (Body tp)) => FromJSON (OffChainVoteDataTp tp) where
  parseJSON =
    withObject "OffChainVoteDataTp" $ \o ->
      OffChainVoteDataTp
        <$> o .: "hashAlgorithm"
        <*> parseAuthors @tp o
        <*> o .: "body"
        <*> o .:? "@context" .!= defaultContext

instance HasBody tp => FromJSON (Reference tp) where
  parseJSON =
    withObject "reference" $ \o ->
      Reference
        <$> parseRefType o (referenceTypes @tp)
        <*> o .: "label"
        <*> o .: "uri"
        <*> o .:? "referenceHash"

instance FromJSON ReferenceHash where
  parseJSON =
    withObject "referenceHash" $ \o ->
      ReferenceHash
        <$> o .: "hashDigest"
        <*> o .: "hashAlgorithm"

instance FromJSON ExternalUpdate where
  parseJSON =
    withObject "externalUpdate" $ \o ->
      ExternalUpdate
        <$> o .: "title"
        <*> o .: "uri"

instance FromJSON GABody where
  parseJSON v = do
    minimal <- parseJSON v
    withObject "GABody"
      ( \o ->
          GABody minimal
            <$> parseTextLimit 80 "title" o
            <*> parseTextLimit 2500 "abstract" o
            <*> o .: "motivation"
            <*> o .: "rationale"
      )
      v

instance FromJSON DrepBody where
  parseJSON v = do
    minimal <- parseJSON v
    withObject "DrepBody"
      ( \o ->
          DrepBody minimal
            <$> o .:? "paymentAddress"
            <*> parseTextLimit 80 "givenName" o
            <*> o .:? "image"
            <*> parseTextLimitMaybe 1000 "objectives" o
            <*> parseTextLimitMaybe 1000 "motivations" o
            <*> parseTextLimitMaybe 1000 "qualifications" o
            <*> o .:? "doNotList"
      )
      v

instance FromJSON Image where
  parseJSON v = withObject "Image"
    ( \o -> do
        curl <- o .: "contentUrl"
        case Text.stripPrefix "data:" (textValue curl) of
          Just ctb
            | (_, tb) <- Text.break (== '/') ctb
            , Text.isPrefixOf "/" tb
            , (_, b) <- Text.break (== ';') tb
            , Just imageData <- Text.stripPrefix ";base64," b ->
                pure $ Image (TextValue imageData) Nothing
          _ -> fromImageUrl <$> parseJSON v
    )
    v

parseRefType :: Object -> [Text] -> Parser TextValue
parseRefType obj typeKeys = do
  tp <- obj .: "@type"
  if textValue tp `elem` typeKeys
    then pure tp
    else fail $
      "reference type should be one of " <> show typeKeys
        <> " but it's " <> Text.unpack (textValue tp)

parseTextLimit :: Int -> Key -> Object -> Parser TextValue
parseTextLimit maxSize k o = do
  txt <- o .: k
  if Text.length (textValue txt) <= maxSize
    then pure txt
    else fail $
      show k <> " must have at most " <> show maxSize
        <> " characters, but it has " <> show (Text.length (textValue txt))

parseTextLimitMaybe :: Int -> Key -> Object -> Parser (Maybe TextValue)
parseTextLimitMaybe maxSize k o = do
  mtxt <- o .:? k
  case mtxt of
    Nothing -> pure Nothing
    Just txt
      | Text.length (textValue txt) <= maxSize -> pure (Just txt)
      | otherwise -> fail $
          show k <> " must have at most " <> show maxSize
            <> " characters, but it has " <> show (Text.length (textValue txt))

-- ---------------------------------------------------------------------------
-- * Context
-- ---------------------------------------------------------------------------

newtype Context = Context
  { language :: Text  -- key is "@language"
  }
  deriving stock (Show)

defaultContext :: Context
defaultContext = Context "en-us"

instance FromJSON Context where
  parseJSON =
    withObject "Context" $ \o ->
      Context
        <$> o .:? "@language" .!= "en-us"

-- ---------------------------------------------------------------------------
-- * Scalar wrappers (JSON-LD allows scalar OR @{"@value": ...}@)
-- ---------------------------------------------------------------------------

newtype TextValue = TextValue { textValue :: Text }
  deriving stock (Show)

newtype BoolValue = BoolValue { boolValue :: Bool }
  deriving stock (Show)

instance FromJSON TextValue where
  parseJSON v = case v of
    Aeson.String t -> pure (TextValue t)
    Aeson.Object o -> TextValue <$> o .: "@value"
    _              -> fail ("expected String or Object with @value, got " <> typeOfValue v)

instance FromJSON BoolValue where
  parseJSON v = case v of
    Aeson.Bool b   -> pure (BoolValue b)
    Aeson.Object o -> BoolValue <$> o .: "@value"
    _              -> fail ("expected Bool or Object with @value, got " <> typeOfValue v)

typeOfValue :: Value -> [Char]
typeOfValue = \case
  Aeson.Object _ -> "Object"
  Aeson.Array _  -> "Array"
  Aeson.String _ -> "String"
  Aeson.Number _ -> "Number"
  Aeson.Bool _   -> "Boolean"
  Aeson.Null     -> "Null"
