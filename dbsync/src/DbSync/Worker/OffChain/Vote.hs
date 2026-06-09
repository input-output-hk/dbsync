{-# LANGUAGE OverloadedStrings #-}

-- | Vote-specific glue for the off-chain fetch worker.
--
-- The worker discovers anchors by polling PG via the work-queue
-- statements in 'DbSync.Db.Statement.Worker.OffChainVote'. Each anchor is
-- fetched via a pluggable 'OffChainFetcher': 'httpVoteFetcher' for
-- the live HTTP path, 'stubVoteFetcher' for tests.
--
-- Persistence handles all four outcomes the HTTP fetcher can produce:
--
--   * Network or hash failure → @off_chain_vote_fetch_error@ row.
--   * HTTP 200 + valid JSON + CIP-conforming →
--     @off_chain_vote_data@ (is_valid = TRUE) plus the per-anchor-kind
--     subtable rows (gov_action / drep / authors / references /
--     external_updates).
--   * HTTP 200 + valid JSON but schema mismatch →
--     @off_chain_vote_data@ (is_valid = FALSE), no subtables.
--   * HTTP 200 + body is not JSON →
--     @off_chain_vote_data@ (is_valid = NULL), no subtables.
module DbSync.Worker.OffChain.Vote
  ( OffChainVoteWorker
  , OffChainVoteConfig (..)
  , defaultOffChainVoteConfig

    -- * Lifecycle
  , mkOffChainVoteWorker
  , closeOffChainVoteWorker

    -- * Single-cycle entry (exported for tests)
  , runOneVoteCycle
  , voteHooks

    -- * Fetchers
  , httpVoteFetcher
  , stubVoteFetcher
  ) where

import Cardano.Prelude

import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import qualified Hasql.Connection as Conn
import qualified Hasql.Connection.Settings as Settings
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt
import qualified Network.HTTP.Client as Http

import DbSync.Db.Schema.Ids (OffChainVoteDataId, VotingAnchorId)
import DbSync.Db.Schema.OffChainVote
  ( OffChainVoteAuthor (..)
  , OffChainVoteData (..)
  , OffChainVoteDrepData (..)
  , OffChainVoteExternalUpdate (..)
  , OffChainVoteFetchError (..)
  , OffChainVoteGovActionData (..)
  , OffChainVoteReference (..)
  )
import DbSync.Db.Statement.Worker.OffChainVote
  ( PendingVoteFetch (..)
  , insertOffChainVoteAuthorRowStmt
  , insertOffChainVoteDataReturningIdStmt
  , insertOffChainVoteDrepDataRowStmt
  , insertOffChainVoteExternalUpdateRowStmt
  , insertOffChainVoteFetchErrorRowStmt
  , insertOffChainVoteGovActionDataRowStmt
  , insertOffChainVoteReferenceRowStmt
  , queryNewPendingVoteFetchesStmt
  , queryRetryPendingVoteFetchesStmt
  , selectMaxVoteRetryCountStmt
  )
import DbSync.Db.Types (unVoteUrl)
import DbSync.Error (throwDb)
import DbSync.Trace.Types (AppTracer)
import DbSync.Worker.OffChain.Fetcher
  ( OffChainHooks (..)
  , OffChainWorker
  , closeOffChainWorker
  , mkOffChainWorker
  , runOneCycle
  )
import qualified DbSync.Worker.OffChain.Http as Http
import DbSync.Worker.OffChain.Retry (Retry (..), retryAgain)
import DbSync.Worker.OffChain.Types
  ( FetchError (..)
  , OffChainFetcher (..)
  , VoteMetadata (..)
  , VotingAnchorRef (..)
  , renderFetchError
  )
import qualified DbSync.Worker.OffChain.Vote.Types as Vote

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

type OffChainVoteWorker = OffChainWorker PendingVoteFetch

-- | Knobs the consumer wires from config / test harness.
data OffChainVoteConfig = OffChainVoteConfig
  { ovcSleepMicros :: !Int
  , ovcBatchSize   :: !Int32
  , ovcIpfsGateways :: ![Text]
    -- ^ HTTPS prefixes to try when an anchor URL starts with
    -- @ipfs://@. Empty disables IPFS resolution.
  }

-- | Production defaults: 5-minute cycle, 100 anchors per cycle, no
-- IPFS gateways. Tests override 'ovcSleepMicros' for fast cycles.
defaultOffChainVoteConfig :: OffChainVoteConfig
defaultOffChainVoteConfig = OffChainVoteConfig
  { ovcSleepMicros  = 5 * 60 * 1_000_000
  , ovcBatchSize    = 100
  , ovcIpfsGateways = []
  }

componentLabel :: Text
componentLabel = "OffChainVoteWorker"

-- ---------------------------------------------------------------------------
-- * Lifecycle
-- ---------------------------------------------------------------------------

mkOffChainVoteWorker
  :: AppTracer
  -> Settings.Settings
  -> OffChainVoteConfig
  -> OffChainFetcher
  -> IO OffChainVoteWorker
mkOffChainVoteWorker tracer settings cfg fetcher =
  mkOffChainWorker
    tracer
    componentLabel
    settings
    (ovcSleepMicros cfg)
    (ovcBatchSize cfg)
    (voteHooks fetcher)

closeOffChainVoteWorker :: OffChainVoteWorker -> IO ()
closeOffChainVoteWorker = closeOffChainWorker

-- | Drive a single cycle against an externally-owned connection.
-- Used by integration tests so they do not need the sleeping loop.
runOneVoteCycle
  :: AppTracer
  -> Conn.Connection
  -> Int32
  -> OffChainFetcher
  -> IO ()
runOneVoteCycle tracer conn batchSize fetcher =
  runOneCycle tracer componentLabel conn batchSize (voteHooks fetcher)

-- ---------------------------------------------------------------------------
-- * Hooks
-- ---------------------------------------------------------------------------

-- | The complete hook triple for a vote worker.
voteHooks :: OffChainFetcher -> OffChainHooks PendingVoteFetch VoteMetadata
voteHooks fetcher = OffChainHooks
  { ohLoadPending = loadPendingVoteFetches
  , ohFetch       = voteFetch fetcher
  , ohPersist     = persistVoteResult
  }

-- | Read the work queue from PG: new anchors first, then retry-due
-- failures filtered by the backoff schedule.
loadPendingVoteFetches :: Conn.Connection -> Int32 -> IO [PendingVoteFetch]
loadPendingVoteFetches conn maxCount = do
  now     <- utcTimeToPOSIXSeconds <$> getCurrentTime
  fresh   <- runStmt conn maxCount queryNewPendingVoteFetchesStmt
  retried <- runStmt conn maxCount queryRetryPendingVoteFetchesStmt
  pure $ fresh <> filter (isDue now) retried

isDue :: POSIXTime -> PendingVoteFetch -> Bool
isDue now pvf = case pvfPrevFetchTime pvf of
  Nothing -> True
  Just t  ->
    let r = retryAgain (utcTimeToPOSIXSeconds t) (pvfPrevRetryCount pvf)
    in retryRetryTime r <= now

-- | Project a 'PendingVoteFetch' into the public 'VotingAnchorRef'
-- shape that 'OffChainFetcher' speaks.
toFetchRef :: PendingVoteFetch -> VotingAnchorRef
toFetchRef pvf = VotingAnchorRef
  { varUrl        = unVoteUrl (pvfUrl pvf)
  , varMetaHash   = pvfHash pvf
  , varAnchorType = pvfAnchorType pvf
  }

-- | Invoke the fetcher and normalise its 'FetchError' to a short
-- text for the @fetch_error@ column.
voteFetch
  :: OffChainFetcher
  -> PendingVoteFetch
  -> IO (Either Text VoteMetadata)
voteFetch fetcher pvf = do
  res <- ofFetchVoteMetadata fetcher (toFetchRef pvf)
  pure $ case res of
    Right ok -> Right ok
    Left  e  -> Left (renderFetchError e)

-- ---------------------------------------------------------------------------
-- * Persistence
-- ---------------------------------------------------------------------------

persistVoteResult
  :: Conn.Connection
  -> PendingVoteFetch
  -> Either Text VoteMetadata
  -> IO ()
persistVoteResult conn pvf outcome = do
  now <- getCurrentTime
  case outcome of
    Right vm  -> writeSuccess conn (pvfVotingAnchorId pvf) vm
    Left  err -> writeError conn (pvfVotingAnchorId pvf) now err

-- | Decide @is_valid@ and language from the validation flags, write
-- the parent row, then (only when the body matched the CIP schema)
-- write the per-kind subtable rows.
writeSuccess
  :: Conn.Connection
  -> VotingAnchorId
  -> VoteMetadata
  -> IO ()
writeSuccess conn vaId vm = do
  let mVote      = vmVoteData vm
      mIsValid   = case (vmIsValidJson vm, mVote) of
                     (True,  Just _ ) -> Just True   -- valid JSON + CIP match
                     (True,  Nothing) -> Just False  -- valid JSON, schema mismatch
                     (False, _      ) -> Nothing     -- body wasn't JSON at all
      language   = maybe "" Vote.getLanguage mVote
      mMinimal   = Vote.getMinimalBody <$> mVote
      mComment   = mMinimal >>= (fmap Vote.textValue . Vote.comment)
      parentRow  = OffChainVoteData
        { offChainVoteDataVotingAnchorId = vaId
        , offChainVoteDataHash           = vmHash vm
        , offChainVoteDataJson           = vmCanonicalJson vm
        , offChainVoteDataBytes          = vmRawBytes vm
        , offChainVoteDataWarning        = vmWarning vm
        , offChainVoteDataLanguage       = language
        , offChainVoteDataComment        = mComment
        , offChainVoteDataIsValid        = mIsValid
        }
  parentId <- runStmt conn parentRow insertOffChainVoteDataReturningIdStmt
  forM_ mVote (writeSubtables conn parentId)

-- | Insert every subtable row this anchor contributes. The anchor
-- kind decides which subtables apply: gov-action anchors land at
-- most one 'off_chain_vote_gov_action_data' row; drep anchors land
-- at most one 'off_chain_vote_drep_data' row; every kind can carry
-- author / reference / external-update rows.
writeSubtables
  :: Conn.Connection
  -> OffChainVoteDataId
  -> Vote.OffChainVoteData
  -> IO ()
writeSubtables conn parentId vote = do
  forM_ (govActionRow parentId vote) $ \row ->
    runStmt conn row insertOffChainVoteGovActionDataRowStmt
  forM_ (drepRow parentId vote) $ \row ->
    runStmt conn row insertOffChainVoteDrepDataRowStmt
  forM_ (authorRows parentId vote) $ \row ->
    runStmt conn row insertOffChainVoteAuthorRowStmt
  forM_ (referenceRows parentId vote) $ \row ->
    runStmt conn row insertOffChainVoteReferenceRowStmt
  forM_ (externalUpdateRows parentId vote) $ \row ->
    runStmt conn row insertOffChainVoteExternalUpdateRowStmt

govActionRow
  :: OffChainVoteDataId -> Vote.OffChainVoteData -> Maybe OffChainVoteGovActionData
govActionRow parentId = \case
  Vote.OffChainVoteDataGa dt ->
    let b = Vote.body dt
     in Just OffChainVoteGovActionData
          { offChainVoteGovActionDataOffChainVoteDataId = parentId
          , offChainVoteGovActionDataTitle              = Vote.textValue (Vote.title b)
          , offChainVoteGovActionDataAbstract           = Vote.textValue (Vote.abstract b)
          , offChainVoteGovActionDataMotivation         = Vote.textValue (Vote.motivation b)
          , offChainVoteGovActionDataRationale          = Vote.textValue (Vote.rationale b)
          }
  _ -> Nothing

drepRow
  :: OffChainVoteDataId -> Vote.OffChainVoteData -> Maybe OffChainVoteDrepData
drepRow parentId = \case
  Vote.OffChainVoteDataDr dt ->
    let b = Vote.body dt
     in Just OffChainVoteDrepData
          { offChainVoteDrepDataOffChainVoteDataId = parentId
          , offChainVoteDrepDataPaymentAddress     = Vote.textValue <$> Vote.paymentAddress b
          , offChainVoteDrepDataGivenName          = Vote.textValue (Vote.givenName b)
          , offChainVoteDrepDataObjectives         = Vote.textValue <$> Vote.objectives b
          , offChainVoteDrepDataMotivations        = Vote.textValue <$> Vote.motivations b
          , offChainVoteDrepDataQualifications     = Vote.textValue <$> Vote.qualifications b
          , offChainVoteDrepDataImageUrl           = Vote.textValue . Vote.content <$> Vote.image b
          , offChainVoteDrepDataImageHash          = Vote.textValue <$> (Vote.msha256 =<< Vote.image b)
          }
  _ -> Nothing

authorRows
  :: OffChainVoteDataId -> Vote.OffChainVoteData -> [OffChainVoteAuthor]
authorRows parentId vote = map mkRow (Vote.getAuthors vote)
  where
    mkRow au = OffChainVoteAuthor
      { offChainVoteAuthorOffChainVoteDataId = parentId
      , offChainVoteAuthorName               = Vote.textValue <$> Vote.name au
      , offChainVoteAuthorWitnessAlgorithm   = Vote.textValue (Vote.witnessAlgorithm (Vote.witness au))
      , offChainVoteAuthorPublicKey          = Vote.textValue (Vote.publicKey (Vote.witness au))
      , offChainVoteAuthorSignature          = Vote.textValue (Vote.signature (Vote.witness au))
      , offChainVoteAuthorWarning            = Nothing
      }

referenceRows
  :: OffChainVoteDataId -> Vote.OffChainVoteData -> [OffChainVoteReference]
referenceRows parentId vote = map mkRow refs
  where
    refs = fromMaybe [] (Vote.references (Vote.getMinimalBody vote))
    mkRow rf = OffChainVoteReference
      { offChainVoteReferenceOffChainVoteDataId = parentId
      , offChainVoteReferenceLabel              = Vote.textValue (Vote.label rf)
      , offChainVoteReferenceUri                = Vote.textValue (Vote.uri rf)
      , offChainVoteReferenceHashDigest         = Vote.textValue . Vote.hashDigest <$> Vote.referenceHash rf
      , offChainVoteReferenceHashAlgorithm      = Vote.textValue . Vote.rhHashAlgorithm <$> Vote.referenceHash rf
      }

externalUpdateRows
  :: OffChainVoteDataId -> Vote.OffChainVoteData -> [OffChainVoteExternalUpdate]
externalUpdateRows parentId vote = map mkRow updates
  where
    updates = fromMaybe [] (Vote.externalUpdates (Vote.getMinimalBody vote))
    mkRow u = OffChainVoteExternalUpdate
      { offChainVoteExternalUpdateOffChainVoteDataId = parentId
      , offChainVoteExternalUpdateTitle              = Vote.textValue (Vote.euTitle u)
      , offChainVoteExternalUpdateUri                = Vote.textValue (Vote.euUri u)
      }

writeError
  :: Conn.Connection
  -> VotingAnchorId
  -> UTCTime
  -> Text
  -> IO ()
writeError conn vaId now err = do
  retryNo <- nextRetryCount conn vaId
  let row = OffChainVoteFetchError
        { offChainVoteFetchErrorVotingAnchorId = vaId
        , offChainVoteFetchErrorFetchError     = err
        , offChainVoteFetchErrorFetchTime      = now
        , offChainVoteFetchErrorRetryCount     = retryNo
        }
  runStmt conn row insertOffChainVoteFetchErrorRowStmt

-- | @prev + 1@ for the next attempt, or @0@ if no prior error has
-- been recorded for this anchor.
nextRetryCount :: Conn.Connection -> VotingAnchorId -> IO Word64
nextRetryCount conn vaId = do
  mPrev <- runStmt conn vaId selectMaxVoteRetryCountStmt
  pure $ case mPrev of
    Just n  -> n + 1
    Nothing -> 0

-- ---------------------------------------------------------------------------
-- * Fetchers
-- ---------------------------------------------------------------------------

-- | Production fetcher: performs a real HTTP request via the shared
-- restricted 'Http.Manager', with IPFS gateway fallback.
httpVoteFetcher :: Http.Manager -> [Text] -> OffChainFetcher
httpVoteFetcher manager gateways = OffChainFetcher
  { ofFetchPoolMetadata = \_ ->
      pure $ Left (FetchErrorHttp "vote fetcher: pool fetches not supported")
  , ofFetchVoteMetadata = \var ->
      Http.fetchVoteMetadata manager gateways
        (varUrl var) (varMetaHash var) (varAnchorType var)
  , ofGetPendingPools = pure []
  , ofGetPendingVotes = pure []
  , ofSavePoolResult  = \_ _ -> pure ()
  , ofSaveVoteResult  = \_ _ -> pure ()
  }

-- | Always returns an HTTP error. Used by tests to exercise the
-- @off_chain_vote_fetch_error@ path without network access.
stubVoteFetcher :: OffChainFetcher
stubVoteFetcher = OffChainFetcher
  { ofFetchPoolMetadata = \_ ->
      pure $ Left (FetchErrorHttp "stub: HTTP fetcher not yet implemented")
  , ofFetchVoteMetadata = \_ ->
      pure $ Left (FetchErrorHttp "stub: HTTP fetcher not yet implemented")
  , ofGetPendingPools = pure []
  , ofGetPendingVotes = pure []
  , ofSavePoolResult  = \_ _ -> pure ()
  , ofSaveVoteResult  = \_ _ -> pure ()
  }

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

runStmt :: Conn.Connection -> a -> Stmt.Statement a b -> IO b
runStmt conn p stmt = do
  res <- Conn.use conn (Sess.statement p stmt)
  case res of
    Right b -> pure b
    Left e  -> throwDb $ "OffChainVoteWorker session failed: " <> show e
