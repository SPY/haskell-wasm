{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Wasm.Parser (
    
) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLEncoding
import Numeric.Natural

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
id                 { TId $$ }
u32                { TIntLit (asUInt32 -> Just $$) }

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

plaininstr :: { PlainInstr }
    : 'unreachable'                  { Unreachable }
    | 'nop'                          { Nop }
    | 'br' labelidx                  { Br $2 }
    | 'br_if' labelidx               { BrIf $2 }
    | 'br_table' rev_list1(labelidx) { BrTable (reverse $ tail $2) (head $2) }
    | 'return'                       { Return }
    | 'call' funcidx                 { Call $2 }
    | 'call_indirect' typeuse        { CallIndirect $2 }

typedef :: { TypeDef }
    : '(' 'type' opt(ident) functype ')' { TypeDef $3 $4 }

typeuse :: { TypeUse }
    : '(' 'type' typeidx ')' { IndexedTypeUse $3 Nothing }
    | '(' 'type' typeidx paramtypes resulttypes ')' { IndexedTypeUse $3 (Just $ FuncType $4 $5) }
    | paramtypes resulttypes { AnonimousTypeUse $ FuncType $1 $2 }

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

data PlainInstr =
    Unreachable
    | Nop
    | Br LabelIndex
    | BrIf LabelIndex
    | BrTable [LabelIndex] LabelIndex
    | Return
    | Call FuncIndex
    | CallIndirect TypeUse
    deriving (Show, Eq)

data TypeDef = TypeDef (Maybe Ident) FuncType deriving (Show, Eq)

data TypeUse =
    IndexedTypeUse TypeIndex (Maybe FuncType)
    | AnonimousTypeUse FuncType
    deriving (Show, Eq)

happyError tokens = error $ "Error occuried: " ++ show tokens 

}