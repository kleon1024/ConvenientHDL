{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Cynide.Syntax.Ident
  ( -- * Identifier
    Ident(..)
  ) where

import Data.Binary      ( Binary )
import Data.Generics    ( Data, Typeable )

--------------------------------------------------------------------------------

-- TODO check if an identifier is valid; convert to valid identifier

newtype Ident = Ident String
  deriving (Eq, Ord, Show, Binary, Data, Typeable)

--------------------------------------------------------------------------------
