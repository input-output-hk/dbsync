{-# LANGUAGE OverloadedStrings #-}

-- | INSERT/SELECT round-trip tests for every data-table schema record.
--
-- For each schema record we
--
--   1. Build a sample row whose fields hold distinctive, non-default
--      values (so a column-order swap in the production INSERT shows
--      up as a wrong-value after read-back).
--   2. INSERT via the production @insert\<Foo\>RowStmt@.
--   3. SELECT via the production @entity\<Foo\>Decoder@, naming each
--      column explicitly in declaration order (so the decoder's
--      positional consumption order is what we actually exercise).
--   4. Assert that the read-back record equals the inserted record.
--
-- The encoder, the INSERT column list, and the decoder are the three
-- places where a column-order error can hide; if any one of them
-- drifts, the round-trip fails on the value comparison.
module DbSync.Db.Statement.RoundTripSpec (spec) where

import Cardano.Prelude

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Hasql.Connection as Conn
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import Test.Hspec
  ( Spec
  , afterAll_
  , beforeAll_
  , before_
  , describe
  , it
  , shouldBe
  )

import DbSync.Db.Schema.Address
  ( Address (..)
  , addressTableDef
  , entityAddressDecoder
  )
import DbSync.Db.Schema.CBOR
  ( TxCbor (..)
  , entityTxCborDecoder
  , txCborTableDef
  )
import DbSync.Db.Schema.Core
  ( Block (..)
  , SlotLeader (..)
  , Tx (..)
  , blockTableDef
  , entityBlockDecoder
  , entitySlotLeaderDecoder
  , entityTxDecoder
  , slotLeaderTableDef
  , txTableDef
  )
import DbSync.Db.Schema.Ids
  ( AddressId (..)
  , BlockId (..)
  , CollateralTxInId (..)
  , CollateralTxOutId (..)
  , DelegationId (..)
  , MaTxMintId (..)
  , MaTxOutId (..)
  , MultiAssetId (..)
  , PoolHashId (..)
  , PoolMetadataRefId (..)
  , PoolOwnerId (..)
  , PoolRelayId (..)
  , PoolRetireId (..)
  , PoolUpdateId (..)
  , ReferenceTxInId (..)
  , SlotLeaderId (..)
  , StakeAddressId (..)
  , StakeDeregistrationId (..)
  , StakeRegistrationId (..)
  , TxCborId (..)
  , TxId (..)
  , TxInId (..)
  , TxMetadataId (..)
  , TxOutId (..)
  , WithdrawalId (..)
  )
import DbSync.Db.Schema.Metadata
  ( TxMetadata (..)
  , entityTxMetadataDecoder
  , txMetadataTableDef
  )
import DbSync.Db.Schema.MultiAsset
  ( MaTxMint (..)
  , MaTxOut (..)
  , MultiAsset (..)
  , entityMaTxMintDecoder
  , entityMaTxOutDecoder
  , entityMultiAssetDecoder
  , maTxMintTableDef
  , maTxOutTableDef
  , multiAssetTableDef
  )
import DbSync.Db.Schema.Pool
  ( PoolHash (..)
  , PoolMetadataRef (..)
  , PoolOwner (..)
  , PoolRelay (..)
  , PoolRetire (..)
  , PoolUpdate (..)
  , entityPoolHashDecoder
  , entityPoolMetadataRefDecoder
  , entityPoolOwnerDecoder
  , entityPoolRelayDecoder
  , entityPoolRetireDecoder
  , entityPoolUpdateDecoder
  , poolHashTableDef
  , poolMetadataRefTableDef
  , poolOwnerTableDef
  , poolRelayTableDef
  , poolRetireTableDef
  , poolUpdateTableDef
  )
import DbSync.Db.Schema.StakeDelegation
  ( Delegation (..)
  , StakeAddress (..)
  , StakeDeregistration (..)
  , StakeRegistration (..)
  , Withdrawal (..)
  , delegationTableDef
  , entityDelegationDecoder
  , entityStakeAddressDecoder
  , entityStakeDeregistrationDecoder
  , entityStakeRegistrationDecoder
  , entityWithdrawalDecoder
  , stakeAddressTableDef
  , stakeDeregistrationTableDef
  , stakeRegistrationTableDef
  , withdrawalTableDef
  )
import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , TableDef (..)
  )
import DbSync.Db.Schema.UTxO
  ( CollateralTxIn (..)
  , CollateralTxOut (..)
  , ReferenceTxIn (..)
  , TxIn (..)
  , TxOut (..)
  , collateralTxInTableDef
  , collateralTxOutTableDef
  , entityCollateralTxInDecoder
  , entityCollateralTxOutDecoder
  , entityReferenceTxInDecoder
  , entityTxInDecoder
  , entityTxOutDecoder
  , referenceTxInTableDef
  , txInTableDef
  , txOutTableDef
  )
import DbSync.Db.Sql (quoteIdent)
import DbSync.Db.Statement.Address (insertAddressRowStmt)
import DbSync.Db.Statement.Block (insertBlockRowStmt)
import DbSync.Db.Statement.CollateralTxIn (insertCollateralTxInRowStmt)
import DbSync.Db.Statement.CollateralTxOut (insertCollateralTxOutRowStmt)
import DbSync.Db.Statement.Delegation (insertDelegationRowStmt)
import DbSync.Db.Statement.MaTxMint (insertMaTxMintRowStmt)
import DbSync.Db.Statement.MaTxOut (insertMaTxOutRowStmt)
import DbSync.Db.Statement.MultiAsset (insertMultiAssetRowStmt)
import DbSync.Db.Statement.PoolHash (insertPoolHashRowStmt)
import DbSync.Db.Statement.PoolMetadataRef (insertPoolMetadataRefRowStmt)
import DbSync.Db.Statement.PoolOwner (insertPoolOwnerRowStmt)
import DbSync.Db.Statement.PoolRelay (insertPoolRelayRowStmt)
import DbSync.Db.Statement.PoolRetire (insertPoolRetireRowStmt)
import DbSync.Db.Statement.PoolUpdate (insertPoolUpdateRowStmt)
import DbSync.Db.Statement.ReferenceTxIn (insertReferenceTxInRowStmt)
import DbSync.Db.Statement.SlotLeader (insertSlotLeaderRowStmt)
import DbSync.Db.Statement.StakeAddress (insertStakeAddressRowStmt)
import DbSync.Db.Statement.StakeDeregistration (insertStakeDeregistrationRowStmt)
import DbSync.Db.Statement.StakeRegistration (insertStakeRegistrationRowStmt)
import DbSync.Db.Statement.Tx (insertTxRowStmt)
import DbSync.Db.Statement.TxCbor (insertTxCborRowStmt)
import DbSync.Db.Statement.TxIn (insertTxInRowStmt)
import DbSync.Db.Statement.TxMetadata (insertTxMetadataRowStmt)
import DbSync.Db.Statement.TxOut (insertTxOutRowStmt)
import DbSync.Db.Statement.Withdrawal (insertWithdrawalRowStmt)
import DbSync.Db.Types (DbLovelace (..), DbWord64 (..))
import DbSync.Test.Database
  ( setupFollowTipSchema
  , teardownSchema
  , truncateAllTables
  )
import DbSync.Test.Hasql (runStatement, withTestConnection)

-- ---------------------------------------------------------------------------
-- Tables and versions covered by this spec
-- ---------------------------------------------------------------------------

allTables :: [TableDef]
allTables =
  [ -- Core
    blockTableDef, txTableDef, slotLeaderTableDef
    -- UTxO
  , txOutTableDef, txInTableDef, collateralTxInTableDef
  , collateralTxOutTableDef, referenceTxInTableDef
    -- Address
  , addressTableDef
    -- Metadata
  , txMetadataTableDef
    -- MultiAsset
  , multiAssetTableDef, maTxMintTableDef, maTxOutTableDef
    -- StakeDelegation
  , stakeAddressTableDef, stakeRegistrationTableDef
  , stakeDeregistrationTableDef, delegationTableDef, withdrawalTableDef
    -- Pool
  , poolHashTableDef, poolUpdateTableDef, poolMetadataRefTableDef
  , poolOwnerTableDef, poolRetireTableDef, poolRelayTableDef
    -- CBOR
  , txCborTableDef
  ]

allVersions :: [(Text, Int)]
allVersions =
  [ ("core", 1)
  , ("utxo", 1)
  , ("metadata", 1)
  , ("multi_asset", 1)
  , ("stake_delegation", 1)
  , ("pool", 1)
  , ("cbor", 1)
  ]

-- ---------------------------------------------------------------------------
-- Round-trip helper
-- ---------------------------------------------------------------------------

-- | Build a @SELECT \<cols\> FROM \<table\> ORDER BY id LIMIT 1@
-- statement that reads back one row through the supplied entity
-- decoder.
--
-- The column list is taken from 'tdColumns' (excluding generated
-- columns), in declaration order. That order must match what the
-- decoder consumes — if it doesn't, the test fails on the value
-- comparison, which is exactly the kind of silent column-swap bug
-- we want to catch.
selectFirstRow :: D.Row (k, a) -> TableDef -> Stmt.Statement () (Maybe (k, a))
selectFirstRow decoder td =
  Stmt.unpreparable
    ("SELECT " <> cols <> " FROM " <> quoteIdent (tdName td) <> " ORDER BY id LIMIT 1")
    E.noParams
    (D.rowMaybe decoder)
  where
    cols = T.intercalate ", " (map (quoteIdent . cdName) nonGenerated)
    nonGenerated =
      filter (\c -> cdName c `notElem` map fst (tdGeneratedColumns td))
             (tdColumns td)

-- | Convenience: build, INSERT, SELECT, compare in one call.
--
-- @runRoundTrip conn td decoder ins (k, sample)@:
--
--   1. Inserts @sample@ keyed by @k@ via @ins@.
--   2. SELECTs the first row of @td@ via @decoder@.
--   3. Asserts the result equals @Just (k, sample)@.
runRoundTrip
  :: (Eq k, Eq a, Show k, Show a)
  => Conn.Connection
  -> TableDef
  -> D.Row (k, a)
  -> Stmt.Statement (k, a) ()
  -> (k, a)
  -> IO ()
runRoundTrip conn td decoder ins pair = do
  runStatement conn pair ins
  result <- runStatement conn () (selectFirstRow decoder td)
  result `shouldBe` Just pair

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec =
  beforeAll_ (setupFollowTipSchema allTables allVersions) $
  afterAll_  (teardownSchema allTables) $
  before_    (truncateAllTables (map tdName allTables)) $
    describe "INSERT/SELECT round-trip" $ do
      blockSpec
      txSpec
      slotLeaderSpec
      txOutSpec
      txInSpec
      collateralTxInSpec
      collateralTxOutSpec
      referenceTxInSpec
      addressSpec
      txMetadataSpec
      multiAssetSpec
      maTxMintSpec
      maTxOutSpec
      stakeAddressSpec
      stakeRegistrationSpec
      stakeDeregistrationSpec
      delegationSpec
      withdrawalSpec
      poolHashSpec
      poolUpdateSpec
      poolMetadataRefSpec
      poolOwnerSpec
      poolRetireSpec
      poolRelaySpec
      txCborSpec

-- ---------------------------------------------------------------------------
-- Core
-- ---------------------------------------------------------------------------

blockSpec :: Spec
blockSpec = it "Block" $ withTestConnection $ \conn -> do
  let bid = BlockId 1
      sample = Block
        { blockHash          = bs32 0xAA
        , blockEpochNo       = Just 11
        , blockSlotNo        = Just 22
        , blockEpochSlotNo   = Just 33
        , blockBlockNo       = Just 44
        , blockPreviousId    = Nothing
        , blockSlotLeaderId  = SlotLeaderId 999
        , blockSize          = 55
        , blockTime          = sampleTime
        , blockTxCount       = 66
        , blockProtoMajor    = 7
        , blockProtoMinor    = 8
        , blockVrfKey        = Just "vrf-key-text"
        , blockOpCert        = Just (bs32 0xBB)
        , blockOpCertCounter = Just 99
        }
  runRoundTrip conn blockTableDef entityBlockDecoder insertBlockRowStmt (bid, sample)

txSpec :: Spec
txSpec = it "Tx" $ withTestConnection $ \conn -> do
  let tid = TxId 1
      sample = Tx
        { txHash             = bs32 0x11
        , txBlockId          = BlockId 7
        , txBlockIndex       = 2
        , txOutSum           = DbLovelace 1_000_000
        , txFee              = DbLovelace 174_000
        , txDeposit          = Just (-500)
        , txSize             = 312
        , txInvalidBefore    = Just (DbWord64 100)
        , txInvalidHereafter = Just (DbWord64 200)
        , txValidContract    = True
        , txScriptSize       = 41
        , txTreasuryDonation = DbLovelace 50
        }
  runRoundTrip conn txTableDef entityTxDecoder insertTxRowStmt (tid, sample)

slotLeaderSpec :: Spec
slotLeaderSpec = it "SlotLeader" $ withTestConnection $ \conn -> do
  let sid = SlotLeaderId 1
      sample = SlotLeader
        { slotLeaderHash        = bs28 0xC1
        , slotLeaderPoolHashId  = Just (PoolHashId 42)
        , slotLeaderDescription = "test-leader"
        }
  runRoundTrip conn slotLeaderTableDef entitySlotLeaderDecoder insertSlotLeaderRowStmt (sid, sample)

-- ---------------------------------------------------------------------------
-- UTxO
-- ---------------------------------------------------------------------------

txOutSpec :: Spec
txOutSpec = it "TxOut" $ withTestConnection $ \conn -> do
  let oid = TxOutId 1
      sample = TxOut
        { txOutTxId              = TxId 7
        , txOutIndex             = 3
        , txOutAddressId         = Just (AddressId 17)
        , txOutStakeAddressId    = Just (StakeAddressId 19)
        , txOutValue             = DbLovelace 5_000_000
        , txOutDataHash          = Just (bs32 0xD1)
        , txOutInlineDatumId     = Nothing
        , txOutReferenceScriptId = Nothing
        , txOutConsumedByTxId    = Just (TxId 31)
        }
  runRoundTrip conn txOutTableDef entityTxOutDecoder insertTxOutRowStmt (oid, sample)

txInSpec :: Spec
txInSpec = it "TxIn" $ withTestConnection $ \conn -> do
  let iid = TxInId 1
      sample = TxIn
        { txInTxInId     = TxId 41
        , txInTxOutId    = Just (TxId 23)
        , txInTxOutIndex = 5
        , txInTxOutHash  = bs32 0xE1
        , txInRedeemerId = Nothing
        }
  runRoundTrip conn txInTableDef entityTxInDecoder insertTxInRowStmt (iid, sample)

collateralTxInSpec :: Spec
collateralTxInSpec = it "CollateralTxIn" $ withTestConnection $ \conn -> do
  let iid = CollateralTxInId 1
      sample = CollateralTxIn
        { collateralTxInTxInId     = TxId 71
        , collateralTxInTxOutId    = Just (TxId 13)
        , collateralTxInTxOutIndex = 8
        , collateralTxInTxOutHash  = bs32 0xF1
        }
  runRoundTrip conn collateralTxInTableDef entityCollateralTxInDecoder
    insertCollateralTxInRowStmt (iid, sample)

collateralTxOutSpec :: Spec
collateralTxOutSpec = it "CollateralTxOut" $ withTestConnection $ \conn -> do
  let oid = CollateralTxOutId 1
      sample = CollateralTxOut
        { collateralTxOutTxId              = TxId 91
        , collateralTxOutIndex             = 6
        , collateralTxOutAddressId         = Just (AddressId 73)
        , collateralTxOutStakeAddressId    = Just (StakeAddressId 71)
        , collateralTxOutValue             = DbLovelace 12_345_678
        , collateralTxOutDataHash          = Just (bs32 0xC0)
        , collateralTxOutMultiAssetsDescr  = "policy:asset=1"
        , collateralTxOutInlineDatumId     = Nothing
        , collateralTxOutReferenceScriptId = Nothing
        }
  runRoundTrip conn collateralTxOutTableDef entityCollateralTxOutDecoder
    insertCollateralTxOutRowStmt (oid, sample)

referenceTxInSpec :: Spec
referenceTxInSpec = it "ReferenceTxIn" $ withTestConnection $ \conn -> do
  let iid = ReferenceTxInId 1
      sample = ReferenceTxIn
        { referenceTxInTxInId     = TxId 51
        , referenceTxInTxOutId    = Just (TxId 13)
        , referenceTxInTxOutIndex = 9
        , referenceTxInTxOutHash  = bs32 0xA1
        }
  runRoundTrip conn referenceTxInTableDef entityReferenceTxInDecoder
    insertReferenceTxInRowStmt (iid, sample)

-- ---------------------------------------------------------------------------
-- Address
-- ---------------------------------------------------------------------------

addressSpec :: Spec
addressSpec = it "Address" $ withTestConnection $ \conn -> do
  let aid = AddressId 1
      sample = Address
        { addressAddress        = "addr1q-bech32-text"
        , addressRaw            = bs32 0xAD
        , addressHasScript      = True
        , addressPaymentCred    = Just (bs28 0xBE)
        , addressStakeAddressId = Just (StakeAddressId 13)
        }
  runRoundTrip conn addressTableDef entityAddressDecoder
    insertAddressRowStmt (aid, sample)

-- ---------------------------------------------------------------------------
-- Metadata
-- ---------------------------------------------------------------------------

txMetadataSpec :: Spec
txMetadataSpec = it "TxMetadata" $ withTestConnection $ \conn -> do
  let mid = TxMetadataId 1
      -- The @json@ column is @jsonb@; Postgres normalises whitespace
      -- and key order on storage. Use the canonical form
      -- (@{"k": "v"}@ with one space after each colon, no leading or
      -- trailing whitespace) so the round-trip is loss-free.
      sample = TxMetadata
        { txMetadataKey   = DbWord64 674
        , txMetadataJson  = Just "{\"key\": \"value\"}"
        , txMetadataBytes = bs32 0x55
        , txMetadataTxId  = TxId 27
        }
  runRoundTrip conn txMetadataTableDef entityTxMetadataDecoder
    insertTxMetadataRowStmt (mid, sample)

-- ---------------------------------------------------------------------------
-- MultiAsset
-- ---------------------------------------------------------------------------

multiAssetSpec :: Spec
multiAssetSpec = it "MultiAsset" $ withTestConnection $ \conn -> do
  let mid = MultiAssetId 1
      sample = MultiAsset
        { multiAssetPolicy      = bs28 0xC2
        , multiAssetName        = "TestToken"
        , multiAssetFingerprint = "asset1abc..."
        }
  runRoundTrip conn multiAssetTableDef entityMultiAssetDecoder
    insertMultiAssetRowStmt (mid, sample)

maTxMintSpec :: Spec
maTxMintSpec = it "MaTxMint" $ withTestConnection $ \conn -> do
  let mid = MaTxMintId 1
      sample = MaTxMint
        { maTxMintQuantity = -42  -- negative = burn
        , maTxMintTxId     = TxId 19
        , maTxMintIdent    = MultiAssetId 23
        }
  runRoundTrip conn maTxMintTableDef entityMaTxMintDecoder
    insertMaTxMintRowStmt (mid, sample)

maTxOutSpec :: Spec
maTxOutSpec = it "MaTxOut" $ withTestConnection $ \conn -> do
  let mid = MaTxOutId 1
      sample = MaTxOut
        { maTxOutQuantity = DbWord64 1_337
        , maTxOutTxOutId  = TxOutId 29
        , maTxOutIdent    = MultiAssetId 37
        }
  runRoundTrip conn maTxOutTableDef entityMaTxOutDecoder
    insertMaTxOutRowStmt (mid, sample)

-- ---------------------------------------------------------------------------
-- StakeDelegation
-- ---------------------------------------------------------------------------

stakeAddressSpec :: Spec
stakeAddressSpec = it "StakeAddress" $ withTestConnection $ \conn -> do
  let sid = StakeAddressId 1
      sample = StakeAddress
        { stakeAddressHashRaw    = bs28 0xE7
        , stakeAddressView       = "stake1u-bech32-text"
        , stakeAddressScriptHash = Just (bs28 0x5C)
        }
  runRoundTrip conn stakeAddressTableDef entityStakeAddressDecoder
    insertStakeAddressRowStmt (sid, sample)

stakeRegistrationSpec :: Spec
stakeRegistrationSpec = it "StakeRegistration" $ withTestConnection $ \conn -> do
  let rid = StakeRegistrationId 1
      sample = StakeRegistration
        { stakeRegistrationAddrId    = StakeAddressId 17
        , stakeRegistrationCertIndex = 3
        , stakeRegistrationEpochNo   = 257
        , stakeRegistrationTxId      = TxId 41
        , stakeRegistrationDeposit   = Just (DbLovelace 2_000_000)
        }
  runRoundTrip conn stakeRegistrationTableDef entityStakeRegistrationDecoder
    insertStakeRegistrationRowStmt (rid, sample)

stakeDeregistrationSpec :: Spec
stakeDeregistrationSpec = it "StakeDeregistration" $ withTestConnection $ \conn -> do
  let rid = StakeDeregistrationId 1
      sample = StakeDeregistration
        { stakeDeregistrationAddrId     = StakeAddressId 23
        , stakeDeregistrationCertIndex  = 5
        , stakeDeregistrationEpochNo    = 312
        , stakeDeregistrationTxId       = TxId 43
        , stakeDeregistrationRedeemerId = Nothing
        }
  runRoundTrip conn stakeDeregistrationTableDef entityStakeDeregistrationDecoder
    insertStakeDeregistrationRowStmt (rid, sample)

delegationSpec :: Spec
delegationSpec = it "Delegation" $ withTestConnection $ \conn -> do
  let did = DelegationId 1
      sample = Delegation
        { delegationAddrId        = StakeAddressId 29
        , delegationCertIndex     = 7
        , delegationPoolHashId    = PoolHashId 31
        , delegationActiveEpochNo = 414
        , delegationTxId          = TxId 47
        , delegationSlotNo        = 100_000
        , delegationRedeemerId    = Nothing
        }
  runRoundTrip conn delegationTableDef entityDelegationDecoder
    insertDelegationRowStmt (did, sample)

withdrawalSpec :: Spec
withdrawalSpec = it "Withdrawal" $ withTestConnection $ \conn -> do
  let wid = WithdrawalId 1
      sample = Withdrawal
        { withdrawalAddrId     = StakeAddressId 37
        , withdrawalTxId       = TxId 53
        , withdrawalAmount     = DbLovelace 3_141_592
        , withdrawalRedeemerId = Nothing
        }
  runRoundTrip conn withdrawalTableDef entityWithdrawalDecoder
    insertWithdrawalRowStmt (wid, sample)

-- ---------------------------------------------------------------------------
-- Pool
-- ---------------------------------------------------------------------------

poolHashSpec :: Spec
poolHashSpec = it "PoolHash" $ withTestConnection $ \conn -> do
  let pid = PoolHashId 1
      sample = PoolHash
        { poolHashHashRaw = bs28 0xF7
        , poolHashView    = "pool1-bech32-text"
        }
  runRoundTrip conn poolHashTableDef entityPoolHashDecoder
    insertPoolHashRowStmt (pid, sample)

poolUpdateSpec :: Spec
poolUpdateSpec = it "PoolUpdate" $ withTestConnection $ \conn -> do
  let pid = PoolUpdateId 1
      sample = PoolUpdate
        { poolUpdateHashId         = PoolHashId 41
        , poolUpdateCertIndex      = 11
        , poolUpdateVrfKeyHash     = bs32 0x99
        , poolUpdatePledge         = DbLovelace 100_000_000
        , poolUpdateActiveEpochNo  = 615
        , poolUpdateMetaId         = Just (PoolMetadataRefId 71)
        , poolUpdateMargin         = 0.0125
        , poolUpdateFixedCost      = DbLovelace 340_000_000
        , poolUpdateRegisteredTxId = TxId 59
        , poolUpdateRewardAddrId   = StakeAddressId 43
        , poolUpdateDeposit        = Just (DbLovelace 500_000_000)
        }
  runRoundTrip conn poolUpdateTableDef entityPoolUpdateDecoder
    insertPoolUpdateRowStmt (pid, sample)

poolMetadataRefSpec :: Spec
poolMetadataRefSpec = it "PoolMetadataRef" $ withTestConnection $ \conn -> do
  let pid = PoolMetadataRefId 1
      sample = PoolMetadataRef
        { poolMetadataRefPoolId         = PoolHashId 47
        , poolMetadataRefUrl            = "https://example.com/meta.json"
        , poolMetadataRefHash           = bs32 0x8A
        , poolMetadataRefRegisteredTxId = TxId 61
        }
  runRoundTrip conn poolMetadataRefTableDef entityPoolMetadataRefDecoder
    insertPoolMetadataRefRowStmt (pid, sample)

poolOwnerSpec :: Spec
poolOwnerSpec = it "PoolOwner" $ withTestConnection $ \conn -> do
  let pid = PoolOwnerId 1
      sample = PoolOwner
        { poolOwnerAddrId       = StakeAddressId 53
        , poolOwnerPoolUpdateId = PoolUpdateId 59
        }
  runRoundTrip conn poolOwnerTableDef entityPoolOwnerDecoder
    insertPoolOwnerRowStmt (pid, sample)

poolRetireSpec :: Spec
poolRetireSpec = it "PoolRetire" $ withTestConnection $ \conn -> do
  let pid = PoolRetireId 1
      sample = PoolRetire
        { poolRetireHashId        = PoolHashId 61
        , poolRetireCertIndex     = 13
        , poolRetireAnnouncedTxId = TxId 67
        , poolRetireRetiringEpoch = 712
        }
  runRoundTrip conn poolRetireTableDef entityPoolRetireDecoder
    insertPoolRetireRowStmt (pid, sample)

poolRelaySpec :: Spec
poolRelaySpec = it "PoolRelay" $ withTestConnection $ \conn -> do
  let pid = PoolRelayId 1
      sample = PoolRelay
        { poolRelayUpdateId   = PoolUpdateId 67
        , poolRelayIpv4       = Just "203.0.113.1"
        , poolRelayIpv6       = Just "2001:db8::1"
        , poolRelayDnsName    = Just "relay.example"
        , poolRelayDnsSrvName = Just "_pool._tcp.example"
        , poolRelayPort       = Just 3001
        }
  runRoundTrip conn poolRelayTableDef entityPoolRelayDecoder
    insertPoolRelayRowStmt (pid, sample)

-- ---------------------------------------------------------------------------
-- CBOR
-- ---------------------------------------------------------------------------

txCborSpec :: Spec
txCborSpec = it "TxCbor" $ withTestConnection $ \conn -> do
  let cid = TxCborId 1
      sample = TxCbor
        { txCborTxId  = TxId 73
        , txCborBytes = bs32 0xCB
        }
  runRoundTrip conn txCborTableDef entityTxCborDecoder
    insertTxCborRowStmt (cid, sample)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2024 6 15) (secondsToDiffTime 43200)

-- | A 32-byte ByteString filled with the supplied byte. Distinct
-- values per test ensure that a wrong-column read shows up as a
-- value mismatch instead of an accidental match.
bs32 :: Word8 -> ByteString
bs32 b = BS.replicate 32 b

-- | A 28-byte ByteString filled with the supplied byte; used for
-- credential / pool-key hashes.
bs28 :: Word8 -> ByteString
bs28 b = BS.replicate 28 b
