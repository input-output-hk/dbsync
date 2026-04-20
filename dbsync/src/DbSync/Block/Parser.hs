-- | Block parsing.
--
-- Deserialises raw CBOR-encoded blocks from the node into era-independent
-- 'GenericBlock' values.
module DbSync.Block.Parser
  ( parseBlock
  ) where

import Cardano.Prelude

import DbSync.Block.Types (GenericBlock)

-- | Parse a CBOR-encoded block into a 'GenericBlock'.
parseBlock :: ByteString -> Either Text GenericBlock
parseBlock = panic "TODO: not implemented"
