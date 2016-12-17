{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Cynide.Parser where

import Control.Monad            ( liftM, liftM2 )
import Control.Monad.Identity   ( Identity )
import Data.Either              ( partitionEithers)
import Data.Maybe               ( fromMaybe )

import Text.Parsec
import Text.Parsec.Expr hiding (Operator)
import qualified Text.Parsec.Expr as E
import Text.Parsec.Token hiding (GenTokenParser(..))
import qualified Text.Parsec.Token as T

import Language.Cynide.Syntax

-- --------------------

type P s a = ParsecT s () Identity a

type Operator s a = E.Operator s () Identity a

type OpTable s a = [[Operator s a]]

-- --------------------

cynide :: Stream s Identity Char => T.GenTokenParser s () Identity
cynide = makeTokenParser cynideDef

cynideDef :: Stream s Identity Char => GenLanguageDef s () Identity
cynideDef
  = LanguageDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , nestedComments  = True
    , identStart      = letter <|> char '_'
    , identLetter     = alphaNum <|> oneOf "_$"
    , opStart         = oneOf "+-!~&|^*/%><=?:"
    , opLetter        = oneOf "+-!~&|^*/%><=?:"
    , reservedNames   = cynideKeywords
    , reservedOpNames = cynideOps
    , caseSensitive   = True
    }

lexeme = T.lexeme cynide
lexeme :: (Stream s Identity Char) => P s a -> P s a

whiteSpace :: Stream s Identity Char => P s ()
whiteSpace = T.whiteSpace cynide

identifier :: Stream s Identity Char => P s String
identifier = T.identifier cynide

reserved :: Stream s Identity Char => String -> P s ()
reserved = T.reserved cynide

reservedOp :: Stream s Identity Char => String -> P s ()
reservedOp = T.reservedOp cynide

symbol :: Stream s Identity Char => String -> P s ()
symbol x = T.symbol cynide x >> return ()

stringLiteral :: Stream s Identity Char => P s String
stringLiteral = T.stringLiteral cynide

-- integer :: P s Integer
-- integer = T.integer cynide

parens, brackets, braces :: Stream s Identity Char => P s a -> P s a
parens     = T.parens cynide
brackets   = T.brackets cynide
braces     = T.braces cynide

comma, semi, colon, dot :: Stream s Identity Char => P s ()
comma      = T.comma cynide  >> return ()
semi       = T.semi cynide   >> return ()
colon      = T.colon cynide  >> return ()
dot        = T.dot cynide    >> return ()

commaSep, commaSep1 :: Stream s Identity Char => P s a -> P s [a]
commaSep   = T.commaSep cynide
commaSep1  = T.commaSep1 cynide

-- http://www.hdlworks.com/hdl_corner/cynide_ref/index.html
cynideKeywords :: [String]
cynideKeywords
  = [ "m", "p", "i", "o", "c", "io", "r", "w", "n"]


cynideOps :: [String]
cynideOps
  = [ "+", "-", "!", "~", "&", "~&", "|", "~|", "^", "~^", "^~"
    , "+", "-", "*", "/", "%", ">", ">=", "<", "<="
    , "&&", "||", "==", "!=", "===", "!===", "&", "|", "^", "^~", "~^"
    , "<<", ">>", "<<<", ">>>"
    , "?", ":", "->", "**" ]

-- --------------------

cynideFile :: Stream s Identity Char => P s Cynide
cynideFile
  = do whiteSpace
       ds <- many module_description
       eof
       return (Cynide ds)

module_description :: Stream s Identity Char => P s Module
module_description
  = do reserved "m"
       name <- ident
       items <- braces (many module_item)
       return (Module name items)

module_item :: Stream s Identity Char => P s Item
module_item
  = liftM ParamDeclItem parameter_declaration <|>
    liftM InputDeclItem input_declaration <|>
    liftM OutputDeclItem output_declaration <|>
    liftM InOutDeclItem inout_declaration <|>
    liftM ConnectDeclItem connect_declaration <|>
    liftM InstDeclItem instance_declaration 
  <?> "module item"

parameter_declaration :: Stream s Identity Char => P s ParamDecl
parameter_declaration
  = do reserved "p"
       param_assigns <- (commaSep1 parameter_assignment)
                        <?> "parameter list"
       return (ParamDecl param_assigns)
  <?> "parameter declaration"

input_declaration :: Stream s Identity Char => P s InputDecl
input_declaration
  = do reserved "i"
       pt <- optionMaybe portType
       xs <- ident
       l <- optionMaybe const_expr
       w <- optionMaybe const_expr
       return (InputDecl pt xs l w)
  <?> "input declaration"

output_declaration :: Stream s Identity Char => P s OutputDecl
output_declaration
  = do reserved "o"
       pt <- optionMaybe portType
       xs <- ident
       l <- optionMaybe const_expr
       w <- optionMaybe const_expr
       return (OutputDecl pt xs l w)
  <?> "output declaration"

inout_declaration :: Stream s Identity Char => P s InOutDecl
inout_declaration
  = do reserved "io"
       pt <- optionMaybe portType
       xs <- ident
       l <- optionMaybe const_expr
       w <- optionMaybe const_expr
       return (InOutDecl pt xs l w)
  <?> "inout declaration"

connect_declaration :: Stream s Identity Char => P s ConnectDecl
connect_declaration
  = do reserved "c"
       src <- connect_node
       drs <- many connect_node
       return (ConnectDecl src drs)
    <?> "connect declaration"

connect_node :: Stream s Identity Char => P s ConnectNode
connect_node
  = do node <- ident
       r1 <- optionMaybe range
       r2 <- optionMaybe range
       return (ConnectNode node r1 r2)
    <?> "connect node"

interconnect_declaration :: Stream s Identity Char => P s InterConDel
interconnect_declaration
  = do n1 <- connect_node
       n2 <- connect_node
       return (InterConDel n1 n2)

instance_declaration :: Stream s Identity Char => P s InstDecl
instance_declaration
  = do reserved "n"
       x <- ident
       r <- ident
       c <- optionMaybe (parens (many interconnect_declaration))
       return (InstDecl x r c)

-- -----------------------------------------------------------------------------
-- expressions

const_expr :: Stream s Identity Char => P s Expression
const_expr = expression
             <?> "constant expression"

expression :: Stream s Identity Char => P s Expression
expression
  = do e1 <- expression'
       choice [ do symbol "?"
                   e2 <- expression
                   symbol ":"
                   e3 <- expression
                   return (ExprCond e1 e2 e3)
              , return e1
              ]

expression' :: Stream s Identity Char => P s Expression
expression'
  = buildExpressionParser opTable factor <?> "expression"
  where
    factor
      = choice [ parens expression
               , ident >>= expr_ident
               , expr_number
               , expr_string
               , expr_concat
               ]
      <?> "factor"

-- parse an expression that starts with an identifier
expr_ident :: Stream s Identity Char => Ident -> P s Expression
expr_ident x
  = liftM (ExprFunCall x) (parens (commaSep expression)) <|>
    (brackets $
     do e <- expression
        -- for ExprSlice, 'e' is actually a constant expression,
        -- but const_expr = expression, so it does not matter.
        choice [ colon >> liftM (ExprSlice x e) const_expr
               , symbol "+:" >> liftM (ExprSlicePlus x e) const_expr
               , symbol "-:" >> liftM (ExprSliceMinus x e) const_expr
               , return (ExprIndex x e)
               ]) <|>
    return (ExprVar x)

opTable :: Stream s Identity Char => OpTable s Expression
opTable
  = [ [ unaryOp "+" UPlus
      , unaryOp "-" UMinus
      , unaryOp "!" UBang
      , unaryOp "~" UTilde
      ]

    , [ binaryOp "*" Times
      , binaryOp "/" Divide
      , binaryOp "%" Modulo
      , binaryOp "**" Pow
      ]

    , [ binaryOp "+" Plus
      , binaryOp "-" Minus
      ]

    -- TODO <<< and >>> operators
    , [ binaryOp "<<" ShiftLeft
      , binaryOp ">>" ShiftRight
      ]

    , [ binaryOp "<" LessThan
      , binaryOp "<=" LessEqual
      , binaryOp ">" GreaterThan
      , binaryOp ">=" GreaterEqual
      ]

    , [ binaryOp "==" Equals
      , binaryOp "!=" NotEquals
      , binaryOp "===" CEquals
      , binaryOp "!==" CNotEquals
      ]

    , [ unaryOp "&" UAnd
      , unaryOp "~&" UNand
      , binaryOp "&" And
      ]

    , [ unaryOp "^" UXor
      , unaryOp "^~" UXnor
      , unaryOp "~^" UXnor
      , binaryOp "^" Xor
      , binaryOp "^~" Xnor
      , binaryOp "~^" Xnor
      ]
    , [ unaryOp "|" UOr
      , unaryOp "~|" UNor
      , binaryOp "|" Or
      , binaryOp "~|" Nor
      ]

    , [ binaryOp "&&" LAnd ]

    , [ binaryOp "||" LOr ]

    ]

unaryOp :: Stream s Identity Char => String -> UnaryOp -> Operator s Expression
unaryOp name fun
  = Prefix (reservedOp name >> return (ExprUnary fun))

binaryOp :: Stream s Identity Char => String -> BinaryOp -> Operator s Expression
binaryOp name fun
  = Infix (reservedOp name >> return (ExprBinary fun)) AssocLeft

expr_number :: Stream s Identity Char => P s Expression
expr_number
  = liftM ExprNum number

{- syntax for numbers:
  [ sign ] [ size ] [ 'base ] value               // integer
  [ sign ] value[.value] [ sign ] baseExponent    // real

where an integer value is allowed to have some subset of
"0123456789abcdefABCDEFxXzZ?_", depending on the base,
and a real value contains only decimal characters: "0123456789".
-}

expr_string :: Stream s Identity Char => P s Expression
expr_string
  = liftM ExprString stringLiteral

expr_concat :: Stream s Identity Char => P s Expression
expr_concat
  = do symbol "{"
       e <- expression
       choice [ do comma
                   es <- commaSep expression
                   symbol "}"
                   return (ExprConcat (e:es))
              , do es <- braces (commaSep expression)
                   symbol "}"
                   return (ExprMultiConcat e es)
              ]

number :: Stream s Identity Char => P s Number
number
  = do { s <- optionMaybe sign
       ; whiteSpace
       ; base_integral s Nothing <|>
         do n <- decimal_number
            whiteSpace
            -- n could be the size of an integral, the integral value itself,
            -- or the integral part of a real.
            base_integral s (Just n) <|> real_number s n
       }
  where
    base_integral maybe_sign maybe_size

      = do b <- base
           whiteSpace
           x <- digits b
           whiteSpace
           return (IntNum maybe_sign maybe_size (Just b) x)

    -- given the optional sign and the integral part, parse the remainder of a
    -- real number, or yield an integer.
    real_number maybe_sign int_value
      = choice [ do maybe_fractional <- optionMaybe (dot >> decimal_number)
                    whiteSpace
                    maybe_exponent <- optionMaybe $
                                      do _ <- oneOf "eE"
                                         s <- optionMaybe sign
                                         e <- decimal_number
                                         return (s, e)
                    case (maybe_fractional, maybe_exponent) of
                      (Nothing, Nothing)
                        -> return $ IntNum maybe_sign Nothing Nothing int_value
                      _ -> return $ RealNum maybe_sign int_value
                                            maybe_fractional maybe_exponent
               ]


decimal_number :: Stream s Identity Char => P s String
decimal_number = digits DecBase

digits :: Stream s Identity Char => Base -> P s String
digits BinBase
  = many1 (oneOf "01xXzZ?_") <?> "binary digit"
digits OctBase
  = many1 (oneOf "01234567xXzZ?_") <?> "octal digit"
digits HexBase
  = many1 (oneOf "0123456789abcdefABCDEFxXzZ?_") <?> "hexadecimal digit"
digits DecBase
  = many1 (oneOf "0123456789_") <?> "decimal digit"

sign :: Stream s Identity Char => P s Sign
sign = (symbol "+" >> return Pos) <|>
       (symbol "-" >> return Neg)

base :: Stream s Identity Char => P s Base
base = do { _ <- char '\''
          ; (oneOf "bB" >> return BinBase) <|>
            (oneOf "oO" >> return OctBase) <|>
            (oneOf "dD" >> return DecBase) <|>
            (oneOf "hH" >> return HexBase)
          } <?> "base"

-- -----------------------------------------------------------------------------
-- miscellaneous

ident :: Stream s Identity Char => P s Ident
ident = liftM Ident identifier

portDir :: Stream s Identity Char => P s PortDir
portDir = (reserved "i"  >> return (PortDir Input))  <|>
          (reserved "o" >> return (PortDir Output)) <|>
          (reserved "i"  >> return (PortDir InOut))  <?> "port direction"

portType :: Stream s Identity Char => P s PortType
portType = (reserved "w"  >> return (PortType Wire))  <|>
           (reserved "r"   >> return (PortType Reg))

parameter_assignment :: Stream s Identity Char => P s ParamAssign
parameter_assignment
  = do x <- ident
       e <- const_expr
       return (ParamAssign x e)

range :: Stream s Identity Char => P s Range
range
  = brackets $ do e1 <- const_expr
                  e2 <- optionMaybe halfRange
                  return (Range e1 e2)

halfRange :: Stream s Identity Char => P s ConstExpr
halfRange
  = do colon
       e <- const_expr
       return e

-- -----------------------------------------------------------------------------
