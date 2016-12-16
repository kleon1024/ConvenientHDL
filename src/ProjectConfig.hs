{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ProjectConfig 
(
TopData(..), ProjectData(..), ConfigData(..),
Port(..), ParamUnit(..), ParaBlk(..),
ModuleUnit(..), ProjectTree(..), DirecTree(..),
Version(..), ElemType(..), Direction(..)
) where

import GHC.Generics (Generic)
import Prelude
import Control.Monad
import Control.DeepSeq
import Data.Data (Typeable, Data)
import Data.Aeson.TH (deriveJSON, defaultOptions)

data Direction = DInput | DOutput | DInOut deriving (Show, Typeable, Data, Enum, Generic)

instance NFData Direction

data ElemType  = EWire | EReg deriving (Show, Typeable, Data, Enum, Generic)

instance NFData ElemType

data Version = Version {
    v0 :: Int,
    v1 :: Int,
    v2 :: Int,
    v3 :: Int
} deriving (Show, Typeable, Data, Generic)

instance NFData Version

data DirecTree = DirecTree {
    node_name :: String,
    tree_node :: [DirecTree]
} deriving (Show, Typeable, Data, Generic)

instance NFData DirecTree

data ProjectTree = ProjectTree {
    hier_name   :: String,
    modu_name   :: String,
    hier_node   :: [ProjectTree]
} deriving (Show, Typeable, Data, Generic)

instance NFData ProjectTree

data ModuleUnit = ModuleUnit {
    mod_name    :: String,
    mod_file    :: FilePath,
    mod_note    :: String,
    paras       :: [ParaBlk],
    ports       :: [Port]
} deriving (Show, Typeable, Data, Generic)

instance NFData ModuleUnit

data ParaBlk = ParaBlk {
    params :: [ParamUnit]
} deriving (Show, Typeable, Data, Generic)

instance NFData ParaBlk

data ParamUnit = ParamUnit {
    para    :: String,
    exprs   :: String
} deriving (Show, Typeable, Data, Generic)

instance NFData ParamUnit

data Port = Port {
    port        :: String,
    direction   :: Direction,
    eletype     :: ElemType,
    port_len    :: String,
    port_wid    :: String,
    port_note   :: String
} deriving (Show, Typeable, Data, Generic)

instance NFData Port

data ConfigData = ConfigData {
    author      :: String,
    contact     :: String,
    version     :: Version,
    verstep     :: Version
} deriving (Show, Typeable, Data, Generic)

instance NFData ConfigData

data ProjectData = ProjectData {
    directory   :: DirecTree,
    hierarchy   :: [ProjectTree],
    modules     :: [ModuleUnit]
} deriving (Show, Typeable, Data, Generic)

instance NFData ProjectData

data TopData = TopData ConfigData ProjectData deriving (Show, Typeable, Data, Generic)

instance NFData TopData

-- This splice will derive instances of ToJSON and FromJSON for us.

$(deriveJSON defaultOptions ''ConfigData)
$(deriveJSON defaultOptions ''ProjectData)
$(deriveJSON defaultOptions ''TopData)
$(deriveJSON defaultOptions ''Port)
$(deriveJSON defaultOptions ''ParamUnit)
$(deriveJSON defaultOptions ''ModuleUnit)
$(deriveJSON defaultOptions ''ProjectTree)
$(deriveJSON defaultOptions ''DirecTree)
$(deriveJSON defaultOptions ''Version)
$(deriveJSON defaultOptions ''Direction)
$(deriveJSON defaultOptions ''ElemType)
$(deriveJSON defaultOptions ''ParaBlk)
