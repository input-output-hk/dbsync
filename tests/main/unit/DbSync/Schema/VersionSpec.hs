{-# LANGUAGE OverloadedStrings #-}

-- | Pins the declared schema fingerprint. The fingerprint computed from the
-- live 'TableDef's must equal the value recorded for 'currentSchemaVersion'
-- in 'releasedSchemaFingerprints'. Any schema change moves the fingerprint
-- and fails this test until the pin is refreshed (and, post-release, a
-- migration added) — the fast guard against an unacknowledged schema edit.
module DbSync.Schema.VersionSpec (spec) where

import Cardano.Prelude

import Data.List (lookup)
import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Extractor.Registry (declaredSchemaFingerprint)
import DbSync.Schema.Version
  ( currentSchemaVersion
  , releasedSchemaFingerprints
  , unFingerprint
  )

spec :: Spec
spec = describe "DbSync.Schema.Version" $
  it "the declared schema fingerprint matches the pin for currentSchemaVersion" $
    unFingerprint declaredSchemaFingerprint `shouldBe` pinnedFingerprint
  where
    pinnedFingerprint =
      maybe
        "<no releasedSchemaFingerprints entry for currentSchemaVersion>"
        unFingerprint
        (lookup currentSchemaVersion releasedSchemaFingerprints)
