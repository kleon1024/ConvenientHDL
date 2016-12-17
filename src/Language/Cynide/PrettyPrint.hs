{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Cynide.PrettyPrint where

import Data.Maybe               ( fromMaybe )
import Text.PrettyPrint

import Language.Cynide.Syntax

-- -----------------------------------------------------------------------------
-- some utilities, which should go in a common module elsewhere

commasep :: [Doc] -> Doc
commasep = vcat . punctuate comma

lineParens :: Doc -> Doc
lineParens p =  char '(' $$
                p <> char '\n' <>
                char ')' 

mb :: (x -> Doc) -> Maybe x -> Doc
mb = maybe empty

period :: Doc
period = char '.'

tick :: Doc
tick = char '\''

-- -----------------------------------------------------------------------------
-- 1. Source Text

ppCynide :: Cynide -> Doc
ppCynide (Cynide ds)
  = vcat (map ppModule ds)

ppModule :: Module -> Doc
ppModule (Module name items)
  = text "m" <+> ppIdent name $$
    braces (vcat (map ppItem items)) $$
    char '\n'

ppPortDir :: PortDir -> Doc
ppPortDir (PortDir dir) = text (show dir)

ppPortType :: PortType -> Doc
ppPortType (PortType t) = text (show t)

ppItem :: Item -> Doc
ppItem (ParamDeclItem x)     = ppParamDecl x
ppItem (InputDeclItem x)     = ppInputDecl x
ppItem (OutputDeclItem x)    = ppOutputDecl x
ppItem (InOutDeclItem x)     = ppInOutDecl x
ppItem (InstDeclItem x)      = ppInstDecl x
ppItem (ConnectDeclItem x)   = ppConnectDecl x
ppItem (CommentItem msg)
  = vcat [ text "//" <+> text m | m <- lines msg ]

ppParamDecl :: ParamDecl -> Doc
ppParamDecl (ParamDecl paramAssigns)
  = text "p" <+> ppParamAssigns paramAssigns

ppInputDecl :: InputDecl -> Doc
ppInputDecl (InputDecl mb_pt name mb_length mb_width)
  = text "i" <+> mb ppPortType mb_pt <+> 
    ppIdent name <+> mb ppExpr mb_length<+>
    mb ppExpr mb_width

ppOutputDecl :: OutputDecl -> Doc
ppOutputDecl (OutputDecl mb_pt name mb_length mb_width)
  = text "o" <+> mb ppPortType mb_pt <+> 
    ppIdent name <+> mb ppExpr mb_length<+>
    mb ppExpr mb_width

ppInOutDecl :: InOutDecl -> Doc
ppInOutDecl (InOutDecl mb_pt name mb_length mb_width)
  = text "io" <+> mb ppPortType mb_pt <+> 
    ppIdent name <+> mb ppExpr mb_length<+>
    mb ppExpr mb_width

ppConnectDecl :: ConnectDecl -> Doc
ppConnectDecl (ConnectDecl src drs)
  = text "c" <+> ppConnectNode src <+> vcat (map ppConnectNode drs)

ppInterConDel :: InterConDel -> Doc
ppInterConDel (InterConDel n1 n2)
  = ppConnectNode n1 <+> ppConnectNode n2

ppInstDecl :: InstDecl -> Doc
ppInstDecl (InstDecl n1 n2 mb_cs)
  = text "n" <+> ppIdent n1 <+> ppIdent n2 <+>
    mb ppInterConDels mb_cs

ppInterConDels :: [InterConDel] -> Doc
ppInterConDels cs
  = lineParens (vcat (map ppInterConDel cs))

ppConnectNode :: ConnectNode -> Doc
ppConnectNode (ConnectNode n mb_r1 mb_r2)
  = ppIdent n <> mb ppRange mb_r1 <>
    mb ppRange mb_r2

ppExpr :: Expression -> Doc
ppExpr = ppExpr' 0

-- precedence-aware expression pretty printer - adds parens when it needs to
ppExpr' :: Int -> Expression -> Doc
ppExpr' _ (ExprNum x)
  = text (show x)

ppExpr' _ (ExprVar x)
  = ppIdent x
ppExpr' _ (ExprString x)
  = text (show x)
ppExpr' _ (ExprIndex x expr)
  = ppIdent x <> brackets (ppExpr expr)
ppExpr' _ (ExprSlice x e1 e2)
  = ppIdent x <> brackets (ppExpr e1 <> colon <> ppExpr e2)
ppExpr' _ (ExprSlicePlus x e1 e2)
  = ppIdent x <> brackets (ppExpr e1 <> text "+:" <> ppExpr e2)
ppExpr' _ (ExprSliceMinus x e1 e2)
  = ppIdent x <> brackets (ppExpr e1 <> text "-:" <> ppExpr e2)
ppExpr' _ (ExprConcat es)
  = braces (commasep (map ppExpr es))
ppExpr' _ (ExprMultiConcat e es)
  = braces (ppExpr e <> braces (commasep (map ppExpr es)))
ppExpr' prec (ExprUnary op expr)
  = if prec >= unary_prec then parens e else e
   where
    e = text x <> ppExpr' unary_prec expr
    x = lookupOp op unary_op_table
ppExpr' prec (ExprBinary op expr1 expr2)
  = if prec > op_prec then parens e else e
  where
    e = fsep [ppExpr' op_prec expr1, text x, ppExpr' (op_prec + 1) expr2 ]
    (x, op_prec) = lookupOp op binary_op_table

-- this adds unnecessary parens, but it makes the concrete syntax much easier to
-- read
{-
ppExpr' prec (ExprCond e1 e2 e3)
  = if prec > cond_prec then parens x else x
  where
    x = fsep [ pp e1, char '?', pp e2, colon, pp e3 ]

    pp e
      | add_parens e = parens (ppExpr e)
      | otherwise    = ppExpr e

    add_parens :: Expression -> Bool
    add_parens ExprCond{} = True
    add_parens _          = False
-}

ppExpr' prec (ExprCond e1 e2 e3)
  = if prec > cond_prec then parens e else e
  where
    e = fsep [ ppExpr e1, char '?', ppExpr e2, colon, ppExpr e3 ]

ppExpr' _ (ExprFunCall x es)
  = ppIdent x <+> parens (commasep (map ppExpr es))

cond_prec, unary_prec :: Int
cond_prec = 1
unary_prec = 11

lookupOp :: (Eq op, Show op) => op -> [(op, x)] -> x
lookupOp op table
  = fromMaybe (error msg) (lookup op table)
  where msg = "showOp: cannot find operator: " ++ show op

-- precedence tables, also for showing.
-- these tables could also be used for parsing operators.
unary_op_table :: [(UnaryOp, String)]
unary_op_table
  = [ (UPlus, "+"), (UMinus, "-"), (UBang, "!"), (UTilde, "~")
    , (UAnd, "&"), (UNand, "~&"), (UOr, "|"), (UNor, "~|")
    , (UXor, "^"), (UXnor, "~^"), (UXnor, "^~")
    ]

binary_op_table :: [(BinaryOp, (String, Int))]
binary_op_table
  = [ (LOr, ("||", 2))
    , (LAnd, ("&&", 3))
    , (Or, ("|", 4)), (Nor, ("~|", 4))
    , (And, ("&", 5)), (Nand, ("~&", 5)), (Xor, ("^", 5)), (Xnor, ("^~", 5)), (Xnor, ("~^", 5))
    , (Equals, ("==", 6)), (NotEquals, ("!=", 6)), (CEquals, ("===", 6)), (CNotEquals, ("!==", 6))
    , (LessThan, ("<", 7)), (LessEqual, ("<=", 7)), (GreaterThan, (">", 7)), (GreaterEqual, (">=", 7))
    , (ShiftLeft, ("<<", 8)), (ShiftRight, (">>", 8))
    , (Plus, ("+", 9)), (Minus, ("-", 9))
    , (Times, ("*", 10)), (Divide, ("/", 10)), (Modulo, ("%", 10))
    ]

-- -----------------------------------------------------------------------------
-- Miscellaneous

ppParamAssigns :: [ParamAssign] -> Doc
ppParamAssigns paramAssigns
  = commasep (map ppParamAssign paramAssigns)

ppParamAssign :: ParamAssign -> Doc
ppParamAssign (ParamAssign ident expr)
  = ppIdent ident <+> ppExpr expr

ppRange :: Range -> Doc
ppRange (Range e1 mb_e2)
  = brackets (ppExpr e1 <+> mb ppHalfRange mb_e2)

ppHalfRange :: ConstExpr -> Doc
ppHalfRange x = colon <+> ppExpr x

-- TODO: check if the string is a valid Cynide identifier.
--       throw error, or convert it into a valid identifier.
ppIdent :: Ident -> Doc
ppIdent (Ident x) = text x

-- -----------------------------------------------------------------------------
