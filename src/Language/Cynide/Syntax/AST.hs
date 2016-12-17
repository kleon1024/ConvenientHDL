{-# LANGUAGE DeriveDataTypeable, TypeOperators #-}
{-# OPTIONS_DERIVE --append -d Binary #-}

module Language.Cynide.Syntax.AST
  (
  -- * The top-level types
  Cynide(..), Module(..), 

  -- * Items and Declarations
  Item(..), ParamDecl(..), InputDecl(..), 
  OutputDecl(..), InOutDecl(..),
  ConnectDecl(..), InterConDel(..),
  InstDecl(..), 

  -- * Expressions
  Expression, ConstExpr, Expression'(..), ConstExpr',
  Number(..), Base(..), Sign(..), intExpr,
  UnaryOp(..), BinaryOp(..),

  -- * Miscellaneous
  Ident(..), PortDir(..), PortType(..),
  PortDirWord(..), PortTypeWord(..), ConnectNode(..),
  ParamAssign(..), Range(..)
  ) where

import Data.Binary      ( Binary(..), putWord8, getWord8 )
import Data.Generics    ( Data, Typeable )

import Language.Cynide.Syntax.Ident
import Language.Cynide.Syntax.Expression

-- -----------------------------------------------------------------------------
-- 1. Source Text

newtype Cynide = Cynide [Module]
  deriving (Eq, Ord, Show, Data, Typeable)

-- | A top-level module has the module name, the list of ports (both input and
-- output), and the body of the module (a list of declarations).  In the spec,
-- the ports have a more complicated type than simply @Ident@.
data Module
  = Module Ident     -- The name of module
           [Item]    -- The module's body, a list of declarations.
  deriving (Eq, Ord, Show, Data, Typeable)

-- | A declaration.
data Item
  = ParamDeclItem ParamDecl
  | InputDeclItem InputDecl
  | OutputDeclItem OutputDecl
  | InOutDeclItem InOutDecl
  | InstDeclItem InstDecl
  | ConnectDeclItem ConnectDecl
  | CommentItem String
  deriving (Eq, Ord, Show, Data, Typeable)

-- -----------------------------------------------------------------------------
-- 2. Declarations

newtype ParamDecl
  = ParamDecl [ParamAssign]
  deriving (Eq, Ord, Show, Data, Typeable)

data InputDecl
  = InputDecl (Maybe PortType) Ident (Maybe Length) (Maybe Width)
  deriving (Eq, Ord, Show, Data, Typeable)

data OutputDecl
  = OutputDecl (Maybe PortType) Ident (Maybe Length) (Maybe Width)
  deriving (Eq, Ord, Show, Data, Typeable)

data InOutDecl
  = InOutDecl (Maybe PortType) Ident (Maybe Length) (Maybe Width)
  deriving (Eq, Ord, Show, Data, Typeable)

type Length = ConstExpr
type Width = ConstExpr

data ConnectDecl
  = ConnectDecl Source [Drain]
  deriving (Eq, Ord, Show, Data, Typeable)

data InterConDel 
  = InterConDel ConnectNode ConnectNode
  deriving (Eq, Ord, Show, Data, Typeable)

-- --------------------
-- module instantiations

data InstDecl
  = InstDecl
    Ident                             -- ^ Name of the module (not the instance)
    Ident                             -- ^ Name of the instatiation
    (Maybe [InterConDel])
  deriving (Eq, Ord, Show, Data, Typeable)

data ConnectNode 
  = ConnectNode Ident (Maybe Range) (Maybe Range)
  deriving (Eq, Ord, Show, Data, Typeable)

type Source = ConnectNode
type Drain  = ConnectNode

-- -----------------------------------------------------------------------------
-- Miscellaneous

-- | Assign a parameter in its declaration.
data ParamAssign
  = ParamAssign Ident ConstExpr
  deriving (Eq, Ord, Show, Data, Typeable)

data Range
  = Range ConstExpr (Maybe ConstExpr)
  deriving (Eq, Ord, Show, Data, Typeable)

data PortDirWord
  = Input | Output | InOut
  deriving (Eq, Ord, Bounded, Enum, Data, Typeable)

instance Show PortDirWord where
  show Input  = "i"
  show Output = "o"
  show InOut  = "io"

data PortDir
  = PortDir PortDirWord
  deriving (Eq, Ord, Show, Data, Typeable)

data PortTypeWord
  = Reg | Wire
  deriving (Eq, Ord, Bounded, Enum, Data, Typeable)

instance Show PortTypeWord where
  show Reg  = "r"
  show Wire = "w"

data PortType
  = PortType PortTypeWord
  deriving (Eq, Ord, Show, Data, Typeable)

-- -----------------------------------------------------------------------------
-- GENERATED START


instance Binary Cynide where
        put (Cynide x1) = put x1
        get
          = do x1 <- get
               return (Cynide x1)

instance Binary Module where
        put (Module x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (Module x1 x2)

instance Binary Item where
        put x
          = case x of
                ParamDeclItem x1 -> do putWord8 0
                                       put x1
                InputDeclItem x1 -> do putWord8 1
                                       put x1
                OutputDeclItem x1 -> do putWord8 2
                                        put x1
                InOutDeclItem x1 -> do putWord8 3
                                       put x1
                InstDeclItem x1 -> do putWord8 4
                                      put x1
                ConnectDeclItem x1 -> do putWord8 5
                                         put x1
                CommentItem x1 -> do putWord8 6
                                     put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (ParamDeclItem x1)
                   1 -> do x1 <- get
                           return (InputDeclItem x1)
                   2 -> do x1 <- get
                           return (OutputDeclItem x1)
                   3 -> do x1 <- get
                           return (InOutDeclItem x1)
                   4 -> do x1 <- get
                           return (InstDeclItem x1)
                   5 -> do x1 <- get
                           return (ConnectDeclItem x1)
                   6 -> do x1 <- get
                           return (CommentItem x1)
                   _ -> error "Corrupted binary data for Item"

instance Binary ParamDecl where
        put (ParamDecl x1) = put x1
        get
          = do x1 <- get
               return (ParamDecl x1)


instance Binary InputDecl where
        put (InputDecl x1 x2 x3 x4)
          = do put x1
               put x2
               put x3
               put x4
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               return (InputDecl x1 x2 x3 x4)


instance Binary OutputDecl where
        put (OutputDecl x1 x2 x3 x4)
          = do put x1
               put x2
               put x3
               put x4
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               return (OutputDecl x1 x2 x3 x4)


instance Binary InOutDecl where
        put (InOutDecl x1 x2 x3 x4)
          = do put x1
               put x2
               put x3
               put x4
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               return (InOutDecl x1 x2 x3 x4)

instance Binary ConnectDecl where
        put (ConnectDecl x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (ConnectDecl x1 x2) 

instance Binary InterConDel where
        put (InterConDel x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (InterConDel x1 x2) 

instance Binary InstDecl where
        put (InstDecl x1 x2 x3)
          = do put x1
               put x2
               put x3
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               return (InstDecl x1 x2 x3)

instance Binary ConnectNode where
        put (ConnectNode x1 x2 x3)
          = do put x1
               put x2
               put x3
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               return (ConnectNode x1 x2 x3)

instance Binary ParamAssign where
        put (ParamAssign x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (ParamAssign x1 x2)

instance Binary Range where
        put (Range x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (Range x1 x2)

instance Binary PortDirWord where
        put x
          = case x of 
                Input -> putWord8 0
                Output -> putWord8 1
                InOut -> putWord8 2
        get
          = do i <- getWord8
               case i of
                  0 -> return Input
                  1 -> return Output
                  2 -> return InOut
                  _ -> error "Corrupted binary data for PortDir"

instance Binary PortTypeWord where
        put x
          = case x of 
                Reg -> putWord8 0
                Wire -> putWord8 1
        get
          = do i <- getWord8
               case i of
                  0 -> return Reg
                  1 -> return Wire
                  _ -> error "Corrupted binary data for PortType"

instance Binary PortDir where
        put x
          = case x of
                PortDir x1 -> do putWord8 0
                                 put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (PortDir x1)
                   _ -> error "Corrupted binary data for PortDir"

instance Binary PortType where
        put x
          = case x of
                PortType x1 -> do putWord8 0
                                  put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (PortType x1)
                   _ -> error "Corrupted binary data for PortType"

-- GENERATED STOP
