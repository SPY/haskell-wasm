{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Wasm.Parser (
    
) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLEncoding
import qualified Data.Text.Lazy.Read as TLRead

import qualified Data.ByteString.Lazy as LBS
import Numeric.Natural (Natural)
import Data.Maybe (fromMaybe)

import Language.Wasm.Lexer (
        Token (
            TKeyword,
            TIntLit,
            TFloatLit,
            TStringLit,
            TId,
            TOpenBracket,
            TCloseBracket,
            TReserved,
            EOF
        )
    )

}

%name functype functype
%tokentype { Token }

%token

'('                { TOpenBracket }
')'                { TCloseBracket }
'func'             { TKeyword "func" }
'param'            { TKeyword "param" }
'result'           { TKeyword "result" }
'i32'              { TKeyword "i32" }
'i64'              { TKeyword "i64" }
'f32'              { TKeyword "f32" }
'f64'              { TKeyword "f64" }
'mut'              { TKeyword "mut" }
'anyfunc'          { TKeyword "anyfunc" }
'type'             { TKeyword "type" }
'unreachable'      { TKeyword "unreachable" }
'nop'              { TKeyword "nop" }
'br'               { TKeyword "br" }
'br_if'            { TKeyword "br_if" }
'br_table'         { TKeyword "br_table" }
'return'           { TKeyword "return" }
'call'             { TKeyword "call" }
'call_indirect'    { TKeyword "call_indirect" }
'drop'             { TKeyword "drop" }
'select'           { TKeyword "select" }
'get_local'        { TKeyword "get_local" }
'set_local'        { TKeyword "set_local" }
'tee_local'        { TKeyword "tee_local" }
'get_global'       { TKeyword "get_global" }
'set_global'       { TKeyword "set_global" }
'i32.load'         { TKeyword "i32.load" }
'i64.load'         { TKeyword "i64.load" }
'f32.load'         { TKeyword "f32.load" }
'f64.load'         { TKeyword "f64.load" }
'i32.load8_s'      { TKeyword "i32.load8_s" }
'i32.load8_u'      { TKeyword "i32.load8_u" }
'i32.load16_s'     { TKeyword "i32.load16_s" }
'i32.load16_u'     { TKeyword "i32.load16_u" }
'i64.load8_s'      { TKeyword "i64.load8_s" }
'i64.load8_u'      { TKeyword "i64.load8_u" }
'i64.load16_s'     { TKeyword "i64.load16_s" }
'i64.load16_u'     { TKeyword "i64.load16_u" }
'i64.load32_s'     { TKeyword "i64.load32_s" }
'i64.load32_u'     { TKeyword "i64.load32_u" }
'i32.store'        { TKeyword "i32.store" }
'i64.store'        { TKeyword "i64.store" }
'f32.store'        { TKeyword "f32.store" }
'f64.store'        { TKeyword "f64.store" }
id                 { TId $$ }
u32                { TIntLit (asUInt32 -> Just $$) }
offset             { TKeyword (asOffset -> Just $$) }
align              { TKeyword (asAlign -> Just $$) }

%%

functype :: { FuncType }
    : '(' 'func' paramtypes resulttypes ')' { FuncType $3 $4 }

paramtypes :: { [ParamType] }
    : list(paramtype) { concat $1 }

paramtype :: { [ParamType] }
    : '(' 'param' ident valtype ')' { [ParamType (Just $3) $4] }
    | '(' 'param' list1(valtype) ')' { map (ParamType Nothing) $3 }

ident :: { Ident }
    : id { Ident (TL.toStrict (TLEncoding.decodeUtf8 $1)) }

valtype :: { ValueType }
    : 'i32' { I32 }
    | 'i64' { I64 }
    | 'f32' { F32 }
    | 'f64' { F64 }

resulttypes :: { [ValueType] }
    : list(resulttype) { concat $1 }

resulttype :: { [ValueType] }
    : '(' 'result' list1(valtype) ')' { $3 }

limits :: { Limit }
    : u32 u32 { Limit (fromIntegral $1) (Just $ fromIntegral $2) }
    | u32 { Limit (fromIntegral $1) Nothing }

elemtype :: { ElemType }
    : 'anyfunc' { AnyFunc }

tabletype :: { TableType }
    : limits elemtype { TableType $1 $2 }

globaltype :: { GlobalType }
    : valtype { Const $1 }
    | '(' 'mut' valtype ')' { Mut $3 }

labelidx :: { LabelIndex }
    : u32 { $1 }

funcidx :: { FuncIndex }
    : u32 { $1 }

typeidx :: { TypeIndex }
    : u32 { $1 }

localidx :: { LocalIndex }
    : u32 { $1 }

globalidx :: { GlobalIndex }
    : u32 { $1 }

plaininstr :: { PlainInstr }
    -- control instructions
    : 'unreachable'                  { Unreachable }
    | 'nop'                          { Nop }
    | 'br' labelidx                  { Br $2 }
    | 'br_if' labelidx               { BrIf $2 }
    | 'br_table' rev_list1(labelidx) { BrTable (reverse $ tail $2) (head $2) }
    | 'return'                       { Return }
    | 'call' funcidx                 { Call $2 }
    | 'call_indirect' typeuse        { CallIndirect $2 }
    -- parametric instructions
    | 'drop'                         { Drop }
    | 'select'                       { Select }
    -- variable instructions
    | 'get_local' localidx           { GetLocal $2 }
    | 'set_local' localidx           { SetLocal $2 }
    | 'tee_local' localidx           { TeeLocal $2 }
    | 'get_global' globalidx         { GetGlobal $2 }
    | 'set_global' globalidx         { SetGlobal $2 }
    -- memory instructions
    | 'i32.load' memarg4             { I32Load $2 }
    | 'i64.load' memarg8             { I64Load $2 }
    | 'f32.load' memarg4             { F32Load $2 }
    | 'f64.load' memarg8             { F64Load $2 }
    | 'i32.load8_s' memarg1          { I32Load8S $2 }
    | 'i32.load8_u' memarg1          { I32Load8U $2 }
    | 'i32.load16_s' memarg2         { I32Load16S $2 }
    | 'i32.load16_u' memarg2         { I32Load16U $2 }
    | 'i64.load8_s' memarg1          { I64Load8S $2 }
    | 'i64.load8_u' memarg1          { I64Load8U $2 }
    | 'i64.load16_s' memarg2         { I64Load16S $2 }
    | 'i64.load16_u' memarg2         { I64Load16U $2 }
    | 'i64.load32_s' memarg4         { I64Load32S $2 }
    | 'i64.load32_u' memarg4         { I64Load32U $2 }
    | 'i32.store' memarg4            { I32Store $2 }
    | 'i64.store' memarg8            { I64Store $2 }
    | 'f32.store' memarg4            { F32Store $2 }
    | 'f64.store' memarg8            { F64Store $2 }

typedef :: { TypeDef }
    : '(' 'type' opt(ident) functype ')' { TypeDef $3 $4 }

typeuse :: { TypeUse }
    : '(' 'type' typeidx ')' { IndexedTypeUse $3 Nothing }
    | '(' 'type' typeidx paramtypes resulttypes ')' { IndexedTypeUse $3 (Just $ FuncType $4 $5) }
    | paramtypes resulttypes { AnonimousTypeUse $ FuncType $1 $2 }

memarg1 :: { MemArg }
    : opt(offset) opt(align) { MemArg (fromMaybe 0 $1) (fromMaybe 1 $2) }

memarg2 :: { MemArg }
    : opt(offset) opt(align) { MemArg (fromMaybe 0 $1) (fromMaybe 2 $2) }

memarg4 :: { MemArg }
    : opt(offset) opt(align) { MemArg (fromMaybe 0 $1) (fromMaybe 4 $2) }

memarg8 :: { MemArg }
    : opt(offset) opt(align) { MemArg (fromMaybe 0 $1) (fromMaybe 8 $2) }

-- utils

rev_list(p)
    : rev_list(p) p  { $2 : $1 }
    | {- empty -}    { [] }

rev_list1(p)
    : rev_list1(p) p { $2 : $1 }
    | p              { [$1] }

list(p)
    : rev_list(p)    { reverse $1 }

list1(p)
    : rev_list1(p)   { reverse $1 }

opt(p)
    : p { Just $1 }
    |   { Nothing }

{

asUInt32 :: Integer -> Maybe Natural
asUInt32 val
    | val >= 0, val < 2 ^ 32 = Just $ fromIntegral val
    | otherwise = Nothing

asOffset :: LBS.ByteString -> Maybe Natural
asOffset str = do
    num <- TL.stripPrefix "offset=" $ TLEncoding.decodeUtf8 str
    fromIntegral . fst <$> eitherToMaybe (TLRead.decimal num)

asAlign :: LBS.ByteString -> Maybe Natural
asAlign str = do
    num <- TL.stripPrefix "align=" $ TLEncoding.decodeUtf8 str
    fromIntegral . fst <$> eitherToMaybe (TLRead.decimal num)

eitherToMaybe :: Either left right -> Maybe right
eitherToMaybe = either (const Nothing) Just

data ValueType =
    I32
    | I64
    | F32
    | F64
    deriving (Show, Eq)

data FuncType = FuncType {
        params :: [ParamType],
        results :: [ValueType]
    } deriving (Show, Eq)

data ParamType = ParamType {
        ident :: Maybe Ident,
        paramType :: ValueType
    } deriving (Show, Eq)

newtype Ident = Ident T.Text deriving (Show, Eq)

data GlobalType = Const ValueType | Mut ValueType deriving (Show, Eq)

data Limit = Limit Natural (Maybe Natural) deriving (Show, Eq)

data ElemType = AnyFunc deriving (Show, Eq)

data TableType = TableType Limit ElemType deriving (Show, Eq)

type LabelIndex = Natural
type FuncIndex = Natural
type TypeIndex = Natural
type LocalIndex = Natural
type GlobalIndex = Natural

data PlainInstr =
    -- Control instructions
    Unreachable
    | Nop
    | Br LabelIndex
    | BrIf LabelIndex
    | BrTable [LabelIndex] LabelIndex
    | Return
    | Call FuncIndex
    | CallIndirect TypeUse
    -- Parametric instructions
    | Drop
    | Select
    -- Variable instructions
    | GetLocal LocalIndex
    | SetLocal LocalIndex
    | TeeLocal LocalIndex
    | GetGlobal GlobalIndex
    | SetGlobal GlobalIndex
    -- Memory instructions
    | I32Load MemArg
    | I64Load MemArg
    | F32Load MemArg
    | F64Load MemArg
    | I32Load8S MemArg
    | I32Load8U MemArg
    | I32Load16S MemArg
    | I32Load16U MemArg
    | I64Load8S MemArg
    | I64Load8U MemArg
    | I64Load16S MemArg
    | I64Load16U MemArg
    | I64Load32S MemArg
    | I64Load32U MemArg
    | I32Store MemArg
    | I64Store MemArg
    | F32Store MemArg
    | F64Store MemArg
    deriving (Show, Eq)

data TypeDef = TypeDef (Maybe Ident) FuncType deriving (Show, Eq)

data TypeUse =
    IndexedTypeUse TypeIndex (Maybe FuncType)
    | AnonimousTypeUse FuncType
    deriving (Show, Eq)

data MemArg = MemArg { offset :: Natural, align :: Natural } deriving (Show, Eq)

happyError tokens = error $ "Error occuried: " ++ show tokens 

}