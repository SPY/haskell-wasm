{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.Wasm.Parser (
    parseModule,
    parseModuleFields,
    parseScript,
    desugarize,
    ModuleField(..),
    DataSegment(..),
    ElemSegment(..),
    StartFunction(..),
    Export(..),
    ExportDesc(..),
    Table(..),
    Memory(..),
    Global(..),
    Function(..),
    LocalType(..),
    Import(..),
    ImportDesc(..),
    Instruction(..),
    TypeUse(..),
    TypeDef(..),
    PlainInstr(..),
    Index(..),
    Ident(..),
    ParamType(..),
    FuncType(..),
    -- script
    Script,
    ModuleDef(..),
    Command(..),
    Action(..),
    Assertion(..),
    Meta(..)
) where

import Language.Wasm.Structure (
        MemArg(..),
        IUnOp(..),
        IBinOp(..),
        IRelOp(..),
        FUnOp(..),
        FBinOp(..),
        FRelOp(..),
        BitSize(..),
        TableType(..),
        ElemType(..),
        Limit(..),
        GlobalType(..),
        ValueType(..)
    )

import qualified Language.Wasm.Structure as S

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLEncoding
import qualified Data.Text.Lazy.Read as TLRead

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSChar8
import Data.Maybe (fromMaybe, fromJust)
import Data.List (foldl', findIndex, find)
import Control.Monad (guard)

import Numeric.Natural (Natural)
import Data.Word (Word32, Word64)
import Data.Bits ((.|.))
import Numeric.IEEE (infinity, nan)
import Language.Wasm.FloatUtils (doubleToFloat)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

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
        ),
        Lexeme(..),
        AlexPosn(..)
    )

import Debug.Trace as Debug

}

%name parseModule mod
%name parseModuleFields modAsFields
%name parseScript script
%monad { Either String }
%tokentype { Lexeme }

%token

'('                   { Lexeme _ TOpenBracket }
')'                   { Lexeme _ TCloseBracket }
'func'                { Lexeme _ (TKeyword "func") }
'param'               { Lexeme _ (TKeyword "param") }
'result'              { Lexeme _ (TKeyword "result") }
'i32'                 { Lexeme _ (TKeyword "i32") }
'i64'                 { Lexeme _ (TKeyword "i64") }
'f32'                 { Lexeme _ (TKeyword "f32") }
'f64'                 { Lexeme _ (TKeyword "f64") }
'mut'                 { Lexeme _ (TKeyword "mut") }
'anyfunc'             { Lexeme _ (TKeyword "anyfunc") }
'type'                { Lexeme _ (TKeyword "type") }
'unreachable'         { Lexeme _ (TKeyword "unreachable") }
'nop'                 { Lexeme _ (TKeyword "nop") }
'br'                  { Lexeme _ (TKeyword "br") }
'br_if'               { Lexeme _ (TKeyword "br_if") }
'br_table'            { Lexeme _ (TKeyword "br_table") }
'return'              { Lexeme _ (TKeyword "return") }
'call'                { Lexeme _ (TKeyword "call") }
'call_indirect'       { Lexeme _ (TKeyword "call_indirect") }
'drop'                { Lexeme _ (TKeyword "drop") }
'select'              { Lexeme _ (TKeyword "select") }
'get_local'           { Lexeme _ (TKeyword "get_local") }
'set_local'           { Lexeme _ (TKeyword "set_local") }
'tee_local'           { Lexeme _ (TKeyword "tee_local") }
'get_global'          { Lexeme _ (TKeyword "get_global") }
'set_global'          { Lexeme _ (TKeyword "set_global") }
'i32.load'            { Lexeme _ (TKeyword "i32.load") }
'i64.load'            { Lexeme _ (TKeyword "i64.load") }
'f32.load'            { Lexeme _ (TKeyword "f32.load") }
'f64.load'            { Lexeme _ (TKeyword "f64.load") }
'i32.load8_s'         { Lexeme _ (TKeyword "i32.load8_s") }
'i32.load8_u'         { Lexeme _ (TKeyword "i32.load8_u") }
'i32.load16_s'        { Lexeme _ (TKeyword "i32.load16_s") }
'i32.load16_u'        { Lexeme _ (TKeyword "i32.load16_u") }
'i64.load8_s'         { Lexeme _ (TKeyword "i64.load8_s") }
'i64.load8_u'         { Lexeme _ (TKeyword "i64.load8_u") }
'i64.load16_s'        { Lexeme _ (TKeyword "i64.load16_s") }
'i64.load16_u'        { Lexeme _ (TKeyword "i64.load16_u") }
'i64.load32_s'        { Lexeme _ (TKeyword "i64.load32_s") }
'i64.load32_u'        { Lexeme _ (TKeyword "i64.load32_u") }
'i32.store'           { Lexeme _ (TKeyword "i32.store") }
'i64.store'           { Lexeme _ (TKeyword "i64.store") }
'f32.store'           { Lexeme _ (TKeyword "f32.store") }
'f64.store'           { Lexeme _ (TKeyword "f64.store") }
'i32.store8'          { Lexeme _ (TKeyword "i32.store8") }
'i32.store16'         { Lexeme _ (TKeyword "i32.store16") }
'i64.store8'          { Lexeme _ (TKeyword "i64.store8") }
'i64.store16'         { Lexeme _ (TKeyword "i64.store16") }
'i64.store32'         { Lexeme _ (TKeyword "i64.store32") }
'current_memory'      { Lexeme _ (TKeyword "current_memory") }
'grow_memory'         { Lexeme _ (TKeyword "grow_memory") }
'memory.size'         { Lexeme _ (TKeyword "memory.size") }
'memory.grow'         { Lexeme _ (TKeyword "memory.grow") }
'i32.const'           { Lexeme _ (TKeyword "i32.const") }
'i64.const'           { Lexeme _ (TKeyword "i64.const") }
'f32.const'           { Lexeme _ (TKeyword "f32.const") }
'f64.const'           { Lexeme _ (TKeyword "f64.const") }
'i32.clz'             { Lexeme _ (TKeyword "i32.clz") }
'i32.ctz'             { Lexeme _ (TKeyword "i32.ctz") }
'i32.popcnt'          { Lexeme _ (TKeyword "i32.popcnt") }
'i32.add'             { Lexeme _ (TKeyword "i32.add") }
'i32.sub'             { Lexeme _ (TKeyword "i32.sub") }
'i32.mul'             { Lexeme _ (TKeyword "i32.mul") }
'i32.div_s'           { Lexeme _ (TKeyword "i32.div_s") }
'i32.div_u'           { Lexeme _ (TKeyword "i32.div_u") }
'i32.rem_s'           { Lexeme _ (TKeyword "i32.rem_s") }
'i32.rem_u'           { Lexeme _ (TKeyword "i32.rem_u") }
'i32.and'             { Lexeme _ (TKeyword "i32.and") }
'i32.or'              { Lexeme _ (TKeyword "i32.or") }
'i32.xor'             { Lexeme _ (TKeyword "i32.xor") }
'i32.shl'             { Lexeme _ (TKeyword "i32.shl") }
'i32.shr_s'           { Lexeme _ (TKeyword "i32.shr_s") }
'i32.shr_u'           { Lexeme _ (TKeyword "i32.shr_u") }
'i32.rotl'            { Lexeme _ (TKeyword "i32.rotl") }
'i32.rotr'            { Lexeme _ (TKeyword "i32.rotr") }
'i64.clz'             { Lexeme _ (TKeyword "i64.clz") }
'i64.ctz'             { Lexeme _ (TKeyword "i64.ctz") }
'i64.popcnt'          { Lexeme _ (TKeyword "i64.popcnt") }
'i64.add'             { Lexeme _ (TKeyword "i64.add") }
'i64.sub'             { Lexeme _ (TKeyword "i64.sub") }
'i64.mul'             { Lexeme _ (TKeyword "i64.mul") }
'i64.div_s'           { Lexeme _ (TKeyword "i64.div_s") }
'i64.div_u'           { Lexeme _ (TKeyword "i64.div_u") }
'i64.rem_s'           { Lexeme _ (TKeyword "i64.rem_s") }
'i64.rem_u'           { Lexeme _ (TKeyword "i64.rem_u") }
'i64.and'             { Lexeme _ (TKeyword "i64.and") }
'i64.or'              { Lexeme _ (TKeyword "i64.or") }
'i64.xor'             { Lexeme _ (TKeyword "i64.xor") }
'i64.shl'             { Lexeme _ (TKeyword "i64.shl") }
'i64.shr_s'           { Lexeme _ (TKeyword "i64.shr_s") }
'i64.shr_u'           { Lexeme _ (TKeyword "i64.shr_u") }
'i64.rotl'            { Lexeme _ (TKeyword "i64.rotl") }
'i64.rotr'            { Lexeme _ (TKeyword "i64.rotr") }
'f32.abs'             { Lexeme _ (TKeyword "f32.abs") }
'f32.neg'             { Lexeme _ (TKeyword "f32.neg") }
'f32.ceil'            { Lexeme _ (TKeyword "f32.ceil") }
'f32.floor'           { Lexeme _ (TKeyword "f32.floor") }
'f32.trunc'           { Lexeme _ (TKeyword "f32.trunc") }
'f32.nearest'         { Lexeme _ (TKeyword "f32.nearest") }
'f32.sqrt'            { Lexeme _ (TKeyword "f32.sqrt") }
'f32.add'             { Lexeme _ (TKeyword "f32.add") }
'f32.sub'             { Lexeme _ (TKeyword "f32.sub") }
'f32.mul'             { Lexeme _ (TKeyword "f32.mul") }
'f32.div'             { Lexeme _ (TKeyword "f32.div") }
'f32.min'             { Lexeme _ (TKeyword "f32.min") }
'f32.max'             { Lexeme _ (TKeyword "f32.max") }
'f32.copysign'        { Lexeme _ (TKeyword "f32.copysign") }
'f64.abs'             { Lexeme _ (TKeyword "f64.abs") }
'f64.neg'             { Lexeme _ (TKeyword "f64.neg") }
'f64.ceil'            { Lexeme _ (TKeyword "f64.ceil") }
'f64.floor'           { Lexeme _ (TKeyword "f64.floor") }
'f64.trunc'           { Lexeme _ (TKeyword "f64.trunc") }
'f64.nearest'         { Lexeme _ (TKeyword "f64.nearest") }
'f64.sqrt'            { Lexeme _ (TKeyword "f64.sqrt") }
'f64.add'             { Lexeme _ (TKeyword "f64.add") }
'f64.sub'             { Lexeme _ (TKeyword "f64.sub") }
'f64.mul'             { Lexeme _ (TKeyword "f64.mul") }
'f64.div'             { Lexeme _ (TKeyword "f64.div") }
'f64.min'             { Lexeme _ (TKeyword "f64.min") }
'f64.max'             { Lexeme _ (TKeyword "f64.max") }
'f64.copysign'        { Lexeme _ (TKeyword "f64.copysign") }
'i32.eqz'             { Lexeme _ (TKeyword "i32.eqz") }
'i32.eq'              { Lexeme _ (TKeyword "i32.eq") }
'i32.ne'              { Lexeme _ (TKeyword "i32.ne") }
'i32.lt_s'            { Lexeme _ (TKeyword "i32.lt_s") }
'i32.lt_u'            { Lexeme _ (TKeyword "i32.lt_u") }
'i32.gt_s'            { Lexeme _ (TKeyword "i32.gt_s") }
'i32.gt_u'            { Lexeme _ (TKeyword "i32.gt_u") }
'i32.le_s'            { Lexeme _ (TKeyword "i32.le_s") }
'i32.le_u'            { Lexeme _ (TKeyword "i32.le_u") }
'i32.ge_s'            { Lexeme _ (TKeyword "i32.ge_s") }
'i32.ge_u'            { Lexeme _ (TKeyword "i32.ge_u") }
'i64.eqz'             { Lexeme _ (TKeyword "i64.eqz") }
'i64.eq'              { Lexeme _ (TKeyword "i64.eq") }
'i64.ne'              { Lexeme _ (TKeyword "i64.ne") }
'i64.lt_s'            { Lexeme _ (TKeyword "i64.lt_s") }
'i64.lt_u'            { Lexeme _ (TKeyword "i64.lt_u") }
'i64.gt_s'            { Lexeme _ (TKeyword "i64.gt_s") }
'i64.gt_u'            { Lexeme _ (TKeyword "i64.gt_u") }
'i64.le_s'            { Lexeme _ (TKeyword "i64.le_s") }
'i64.le_u'            { Lexeme _ (TKeyword "i64.le_u") }
'i64.ge_s'            { Lexeme _ (TKeyword "i64.ge_s") }
'i64.ge_u'            { Lexeme _ (TKeyword "i64.ge_u") }
'f32.eq'              { Lexeme _ (TKeyword "f32.eq") }
'f32.ne'              { Lexeme _ (TKeyword "f32.ne") }
'f32.lt'              { Lexeme _ (TKeyword "f32.lt") }
'f32.gt'              { Lexeme _ (TKeyword "f32.gt") }
'f32.le'              { Lexeme _ (TKeyword "f32.le") }
'f32.ge'              { Lexeme _ (TKeyword "f32.ge") }
'f64.eq'              { Lexeme _ (TKeyword "f64.eq") }
'f64.ne'              { Lexeme _ (TKeyword "f64.ne") }
'f64.lt'              { Lexeme _ (TKeyword "f64.lt") }
'f64.gt'              { Lexeme _ (TKeyword "f64.gt") }
'f64.le'              { Lexeme _ (TKeyword "f64.le") }
'f64.ge'              { Lexeme _ (TKeyword "f64.ge") }
'i32.wrap/i64'        { Lexeme _ (TKeyword "i32.wrap/i64") }
'i32.trunc_s/f32'     { Lexeme _ (TKeyword "i32.trunc_s/f32") }
'i32.trunc_u/f32'     { Lexeme _ (TKeyword "i32.trunc_u/f32") }
'i32.trunc_s/f64'     { Lexeme _ (TKeyword "i32.trunc_s/f64") }
'i32.trunc_u/f64'     { Lexeme _ (TKeyword "i32.trunc_u/f64") }
'i64.extend_s/i32'    { Lexeme _ (TKeyword "i64.extend_s/i32") }
'i64.extend_u/i32'    { Lexeme _ (TKeyword "i64.extend_u/i32") }
'i64.trunc_s/f32'     { Lexeme _ (TKeyword "i64.trunc_s/f32") }
'i64.trunc_u/f32'     { Lexeme _ (TKeyword "i64.trunc_u/f32") }
'i64.trunc_s/f64'     { Lexeme _ (TKeyword "i64.trunc_s/f64") }
'i64.trunc_u/f64'     { Lexeme _ (TKeyword "i64.trunc_u/f64") }
'f32.convert_s/i32'   { Lexeme _ (TKeyword "f32.convert_s/i32") }
'f32.convert_u/i32'   { Lexeme _ (TKeyword "f32.convert_u/i32") }
'f32.convert_s/i64'   { Lexeme _ (TKeyword "f32.convert_s/i64") }
'f32.convert_u/i64'   { Lexeme _ (TKeyword "f32.convert_u/i64") }
'f32.demote/f64'      { Lexeme _ (TKeyword "f32.demote/f64") }
'f64.convert_s/i32'   { Lexeme _ (TKeyword "f64.convert_s/i32") }
'f64.convert_u/i32'   { Lexeme _ (TKeyword "f64.convert_u/i32") }
'f64.convert_s/i64'   { Lexeme _ (TKeyword "f64.convert_s/i64") }
'f64.convert_u/i64'   { Lexeme _ (TKeyword "f64.convert_u/i64") }
'f64.promote/f32'     { Lexeme _ (TKeyword "f64.promote/f32") }
'i32.reinterpret/f32' { Lexeme _ (TKeyword "i32.reinterpret/f32") }
'i64.reinterpret/f64' { Lexeme _ (TKeyword "i64.reinterpret/f64") }
'f32.reinterpret/i32' { Lexeme _ (TKeyword "f32.reinterpret/i32") }
'f64.reinterpret/i64' { Lexeme _ (TKeyword "f64.reinterpret/i64") }
'block'               { Lexeme _ (TKeyword "block") }
'loop'                { Lexeme _ (TKeyword "loop") }
'if'                  { Lexeme _ (TKeyword "if") }
'else'                { Lexeme _ (TKeyword "else") }
'end'                 { Lexeme _ (TKeyword "end") }
'then'                { Lexeme _ (TKeyword "then") }
'table'               { Lexeme _ (TKeyword "table") }
'memory'              { Lexeme _ (TKeyword "memory") }
'global'              { Lexeme _ (TKeyword "global") }
'import'              { Lexeme _ (TKeyword "import") }
'export'              { Lexeme _ (TKeyword "export") }
'local'               { Lexeme _ (TKeyword "local") }
'elem'                { Lexeme _ (TKeyword "elem") }
'data'                { Lexeme _ (TKeyword "data") }
'offset'              { Lexeme _ (TKeyword "offset") }
'start'               { Lexeme _ (TKeyword "start") }
'module'              { Lexeme _ (TKeyword "module") }
-- script extension
'binary'              { Lexeme _ (TKeyword "binary") }
'quote'               { Lexeme _ (TKeyword "quote") }
'register'            { Lexeme _ (TKeyword "register") }
'invoke'              { Lexeme _ (TKeyword "invoke") }
'get'                 { Lexeme _ (TKeyword "get") }
'assert_return'       { Lexeme _ (TKeyword "assert_return") }
'assert_return_canonical_nan' { Lexeme _ (TKeyword "assert_return_canonical_nan") }
'assert_return_arithmetic_nan' { Lexeme _ (TKeyword "assert_return_arithmetic_nan") }
'assert_trap'         { Lexeme _ (TKeyword "assert_trap") }
'assert_malformed'    { Lexeme _ (TKeyword "assert_malformed") }
'assert_invalid'      { Lexeme _ (TKeyword "assert_invalid") }
'assert_unlinkable'   { Lexeme _ (TKeyword "assert_unlinkable") }
'assert_exhaustion'   { Lexeme _ (TKeyword "assert_exhaustion") }
'script'              { Lexeme _ (TKeyword "script") }
'input'               { Lexeme _ (TKeyword "input") }
'output'              { Lexeme _ (TKeyword "output") }
-- script extension end
id                    { Lexeme _ (TId $$) }
u32                   { Lexeme _ (TIntLit (asUInt32 -> Just $$)) }
i32                   { Lexeme _ (TIntLit (asInt32 -> Just $$)) }
i64                   { Lexeme _ (TIntLit (asInt64 -> Just $$)) }
unrestricted_int      { Lexeme _ (TIntLit $$) }
f64                   { Lexeme _ (TFloatLit $$) }
offset                { Lexeme _ (TKeyword (asOffset -> Just $$)) }
align                 { Lexeme _ (TKeyword (asAlign -> Just $$)) }
str                   { Lexeme _ (TStringLit $$) }
EOF                   { Lexeme _ EOF }

%%

string :: { TL.Text }
    : str {%
        case TLEncoding.decodeUtf8' $1 of
            Right t -> Right t
            Left err -> Left "invalid utf8 string"
    }

name :: { TL.Text }
    : string { $1 }

ident :: { Ident }
    : id { Ident (TLEncoding.decodeUtf8 $1) }

valtype :: { ValueType }
    : 'i32' { I32 }
    | 'i64' { I64 }
    | 'f32' { F32 }
    | 'f64' { F64 }

labelidx :: { LabelIndex }
    : u32 { Index $1 }
    | ident { Named $1 }

funcidx :: { FuncIndex }
    : u32 { Index $1 }
    | ident { Named $1 }

typeidx :: { TypeIndex }
    : u32 { Index $1 }
    | ident { Named $1 }

localidx :: { LocalIndex }
    : u32 { Index $1 }
    | ident { Named $1 }

globalidx :: { GlobalIndex }
    : u32 { Index $1 }
    | ident { Named $1 }

tableidx :: { TableIndex }
    : u32 { Index $1 }
    | ident { Named $1 }

memidx :: { MemoryIndex }
    : u32 { Index $1 }
    | ident { Named $1 }

int32 :: { Integer }
    : u32 { fromIntegral $1 }
    | i32 { $1 }

int64 :: { Integer }
    : u32 { fromIntegral $1 }
    | i32 { $1 }
    | i64 { $1 }

float32 :: { Float }
    : u32 { fromIntegral $1 }
    | i32 { fromIntegral $1 }
    | i64 { fromIntegral $1 }
    | unrestricted_int { fromIntegral $1 }
    | f64 { asFloat32 $1 }

float64 :: { Double }
    : u32 { fromIntegral $1 }
    | i32 { fromIntegral $1 }
    | i64 { fromIntegral $1 }
    | unrestricted_int { fromIntegral $1 }
    | f64 { $1 }

plaininstr :: { PlainInstr }
    -- control instructions
    : 'unreachable'                  { Unreachable }
    | 'nop'                          { Nop }
    | 'br' labelidx                  { Br $2 }
    | 'br_if' labelidx               { BrIf $2 }
    | 'br_table' rev_list1(labelidx) { BrTable (reverse $ tail $2) (head $2) }
    | 'return'                       { Return }
    | 'call' funcidx                 { Call $2 }
    -- | 'call_indirect' typeuse        { CallIndirect $2 }
    -- call_inderict has special case in folded form
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
    | 'i32.store8' memarg1           { I32Store8 $2 }
    | 'i32.store16' memarg2          { I32Store16 $2 }
    | 'i64.store8' memarg1           { I64Store8 $2 }
    | 'i64.store16' memarg2          { I64Store16 $2 }
    | 'i64.store32' memarg4          { I64Store32 $2 }
    | 'current_memory'               { CurrentMemory }
    | 'grow_memory'                  { GrowMemory }
    | 'memory.size'                  { CurrentMemory }
    | 'memory.grow'                  { GrowMemory }
    -- numeric instructions
    | 'i32.const' int32              { I32Const $2 }
    | 'i64.const' int64              { I64Const $2 }
    | 'f32.const' float32            { F32Const $2 }
    | 'f64.const' float64            { F64Const $2 }
    | 'i32.clz'                      { IUnOp BS32 IClz }
    | 'i32.ctz'                      { IUnOp BS32 ICtz }
    | 'i32.popcnt'                   { IUnOp BS32 IPopcnt }
    | 'i32.add'                      { IBinOp BS32 IAdd }
    | 'i32.sub'                      { IBinOp BS32 ISub }
    | 'i32.mul'                      { IBinOp BS32 IMul }
    | 'i32.div_s'                    { IBinOp BS32 IDivS }
    | 'i32.div_u'                    { IBinOp BS32 IDivU }
    | 'i32.rem_s'                    { IBinOp BS32 IRemS }
    | 'i32.rem_u'                    { IBinOp BS32 IRemU }
    | 'i32.and'                      { IBinOp BS32 IAnd }
    | 'i32.or'                       { IBinOp BS32 IOr }
    | 'i32.xor'                      { IBinOp BS32 IXor }
    | 'i32.shl'                      { IBinOp BS32 IShl }
    | 'i32.shr_s'                    { IBinOp BS32 IShrS }
    | 'i32.shr_u'                    { IBinOp BS32 IShrU }
    | 'i32.rotl'                     { IBinOp BS32 IRotl }
    | 'i32.rotr'                     { IBinOp BS32 IRotr }
    | 'i64.clz'                      { IUnOp BS64 IClz }
    | 'i64.ctz'                      { IUnOp BS64 ICtz }
    | 'i64.popcnt'                   { IUnOp BS64 IPopcnt }
    | 'i64.add'                      { IBinOp BS64 IAdd }
    | 'i64.sub'                      { IBinOp BS64 ISub }
    | 'i64.mul'                      { IBinOp BS64 IMul }
    | 'i64.div_s'                    { IBinOp BS64 IDivS }
    | 'i64.div_u'                    { IBinOp BS64 IDivU }
    | 'i64.rem_s'                    { IBinOp BS64 IRemS }
    | 'i64.rem_u'                    { IBinOp BS64 IRemU }
    | 'i64.and'                      { IBinOp BS64 IAnd }
    | 'i64.or'                       { IBinOp BS64 IOr }
    | 'i64.xor'                      { IBinOp BS64 IXor }
    | 'i64.shl'                      { IBinOp BS64 IShl }
    | 'i64.shr_s'                    { IBinOp BS64 IShrS }
    | 'i64.shr_u'                    { IBinOp BS64 IShrU }
    | 'i64.rotl'                     { IBinOp BS64 IRotl }
    | 'i64.rotr'                     { IBinOp BS64 IRotr }
    | 'f32.abs'                      { FUnOp BS32 FAbs }
    | 'f32.neg'                      { FUnOp BS32 FNeg }
    | 'f32.ceil'                     { FUnOp BS32 FCeil }
    | 'f32.floor'                    { FUnOp BS32 FFloor }
    | 'f32.trunc'                    { FUnOp BS32 FTrunc }
    | 'f32.nearest'                  { FUnOp BS32 FNearest }
    | 'f32.sqrt'                     { FUnOp BS32 FSqrt }
    | 'f32.add'                      { FBinOp BS32 FAdd }
    | 'f32.sub'                      { FBinOp BS32 FSub }
    | 'f32.mul'                      { FBinOp BS32 FMul }
    | 'f32.div'                      { FBinOp BS32 FDiv }
    | 'f32.min'                      { FBinOp BS32 FMin }
    | 'f32.max'                      { FBinOp BS32 FMax }
    | 'f32.copysign'                 { FBinOp BS32 FCopySign }
    | 'f64.abs'                      { FUnOp BS64 FAbs }
    | 'f64.neg'                      { FUnOp BS64 FNeg }
    | 'f64.ceil'                     { FUnOp BS64 FCeil }
    | 'f64.floor'                    { FUnOp BS64 FFloor }
    | 'f64.trunc'                    { FUnOp BS64 FTrunc }
    | 'f64.nearest'                  { FUnOp BS64 FNearest }
    | 'f64.sqrt'                     { FUnOp BS64 FSqrt }
    | 'f64.add'                      { FBinOp BS64 FAdd }
    | 'f64.sub'                      { FBinOp BS64 FSub }
    | 'f64.mul'                      { FBinOp BS64 FMul }
    | 'f64.div'                      { FBinOp BS64 FDiv }
    | 'f64.min'                      { FBinOp BS64 FMin }
    | 'f64.max'                      { FBinOp BS64 FMax }
    | 'f64.copysign'                 { FBinOp BS64 FCopySign }
    | 'i32.eqz'                      { I32Eqz }
    | 'i32.eq'                       { IRelOp BS32 IEq }
    | 'i32.ne'                       { IRelOp BS32 INe }
    | 'i32.lt_s'                     { IRelOp BS32 ILtS }
    | 'i32.lt_u'                     { IRelOp BS32 ILtU }
    | 'i32.gt_s'                     { IRelOp BS32 IGtS }
    | 'i32.gt_u'                     { IRelOp BS32 IGtU }
    | 'i32.le_s'                     { IRelOp BS32 ILeS }
    | 'i32.le_u'                     { IRelOp BS32 ILeU }
    | 'i32.ge_s'                     { IRelOp BS32 IGeS }
    | 'i32.ge_u'                     { IRelOp BS32 IGeU }
    | 'i64.eqz'                      { I64Eqz }
    | 'i64.eq'                       { IRelOp BS64 IEq }
    | 'i64.ne'                       { IRelOp BS64 INe }
    | 'i64.lt_s'                     { IRelOp BS64 ILtS }
    | 'i64.lt_u'                     { IRelOp BS64 ILtU }
    | 'i64.gt_s'                     { IRelOp BS64 IGtS }
    | 'i64.gt_u'                     { IRelOp BS64 IGtU }
    | 'i64.le_s'                     { IRelOp BS64 ILeS }
    | 'i64.le_u'                     { IRelOp BS64 ILeU }
    | 'i64.ge_s'                     { IRelOp BS64 IGeS }
    | 'i64.ge_u'                     { IRelOp BS64 IGeU }
    | 'f32.eq'                       { FRelOp BS32 FEq }
    | 'f32.ne'                       { FRelOp BS32 FNe }
    | 'f32.lt'                       { FRelOp BS32 FLt }
    | 'f32.gt'                       { FRelOp BS32 FGt }
    | 'f32.le'                       { FRelOp BS32 FLe }
    | 'f32.ge'                       { FRelOp BS32 FGe }
    | 'f64.eq'                       { FRelOp BS64 FEq }
    | 'f64.ne'                       { FRelOp BS64 FNe }
    | 'f64.lt'                       { FRelOp BS64 FLt }
    | 'f64.gt'                       { FRelOp BS64 FGt }
    | 'f64.le'                       { FRelOp BS64 FLe }
    | 'f64.ge'                       { FRelOp BS64 FGe }
    | 'i32.wrap/i64'                 { I32WrapI64 }
    | 'i32.trunc_s/f32'              { ITruncFS BS32 BS32 }
    | 'i32.trunc_u/f32'              { ITruncFU BS32 BS32 }
    | 'i32.trunc_s/f64'              { ITruncFS BS32 BS64 }
    | 'i32.trunc_u/f64'              { ITruncFU BS32 BS64 }
    | 'i64.extend_s/i32'             { I64ExtendSI32 }
    | 'i64.extend_u/i32'             { I64ExtendUI32 }
    | 'i64.trunc_s/f32'              { ITruncFS BS64 BS32 }
    | 'i64.trunc_u/f32'              { ITruncFU BS64 BS32 }
    | 'i64.trunc_s/f64'              { ITruncFS BS64 BS64 }
    | 'i64.trunc_u/f64'              { ITruncFU BS64 BS64 }
    | 'f32.convert_s/i32'            { FConvertIS BS32 BS32 }
    | 'f32.convert_u/i32'            { FConvertIU BS32 BS32 }
    | 'f32.convert_s/i64'            { FConvertIS BS32 BS64 }
    | 'f32.convert_u/i64'            { FConvertIU BS32 BS64 }
    | 'f32.demote/f64'               { F32DemoteF64 }
    | 'f64.convert_s/i32'            { FConvertIS BS64 BS32 }
    | 'f64.convert_u/i32'            { FConvertIU BS64 BS32 }
    | 'f64.convert_s/i64'            { FConvertIS BS64 BS64 }
    | 'f64.convert_u/i64'            { FConvertIU BS64 BS64 }
    | 'f64.promote/f32'              { F64PromoteF32 }
    | 'i32.reinterpret/f32'          { IReinterpretF BS32 }
    | 'i64.reinterpret/f64'          { IReinterpretF BS64 }
    | 'f32.reinterpret/i32'          { FReinterpretI BS32 }
    | 'f64.reinterpret/i64'          { FReinterpretI BS64 }

typeuse :: { TypeUse }
    : '(' typeuse1 { $2 }
    | {- empty -} { AnonimousTypeUse $ FuncType [] [] }

typeuse1 :: { TypeUse }
    : 'type' typeidx ')' typedtypeuse { IndexedTypeUse $2 $4 }
    | paramsresultstypeuse { AnonimousTypeUse $1 }

typedtypeuse :: { Maybe FuncType }
    : '(' paramsresultstypeuse { Just $2 }
    | {- empty -} { Nothing }

typedef :: { TypeDef }
    : 'type' opt(ident) functype ')' { TypeDef $2 $3 }

functype :: { FuncType }
    : '(' 'func' params_results { $3 }

params_results :: { FuncType }
    : ')' { emptyFuncType }
    | '(' params_results1 { $2 }

params_results1 :: { FuncType }
    : 'param' list(valtype) ')' params_results { mergeFuncType (FuncType (map (ParamType Nothing) $2) []) $4 }
    | 'param' ident valtype ')' params_results { mergeFuncType (FuncType [ParamType (Just $2) $3] []) $5 }
    | results1 { $1 }

results :: { FuncType }
    : ')' { emptyFuncType }
    | '(' results1 { $2 }

results1 :: { FuncType }
    : 'result' list(valtype) ')' results { mergeFuncType (FuncType [] $2) $4 }

paramsresultstypeuse :: { FuncType }
    : paramsresultstypeuse '(' paramsresulttypeuse { mergeFuncType $1 $3 }
    | paramsresulttypeuse { $1 }

paramsresulttypeuse :: { FuncType }
    : 'param' list(valtype) ')' { FuncType (map (ParamType Nothing) $2) [] }
    | 'param' ident valtype ')' { FuncType [ParamType (Just $2) $3] [] }
    | 'result' list(valtype) ')' { FuncType [] $2 }

memarg1 :: { MemArg }
    : opt(offset) opt(align) {% parseMemArg 1 $1 $2 }

memarg2 :: { MemArg }
    : opt(offset) opt(align) {% parseMemArg 2 $1 $2 }

memarg4 :: { MemArg }
    : opt(offset) opt(align) {% parseMemArg 4 $1 $2 }

memarg8 :: { MemArg }
    : opt(offset) opt(align) {% parseMemArg 8 $1 $2 }

instruction :: { [Instruction] }
    : raw_instr { $1 }
    | foldedinstr { $1 }

raw_instr :: { [Instruction] }
    : plaininstr { [PlainInstr $1] }
    | 'call_indirect' raw_call_indirect { $2 }
    | 'block' opt(ident) raw_block { [$3 $2] }
    | 'loop' opt(ident) raw_loop { [$3 $2] }
    | 'if' opt(ident) raw_if_result { $3 $2 }

raw_block :: { Maybe Ident -> Instruction }
    : 'end' opt(ident) { \ident -> BlockInstr ident [] [] }
    | raw_instr list(instruction) 'end' opt(ident) { \ident -> BlockInstr ident [] ($1 ++ concat $2) }
    | '(' raw_block1 { $2 }

raw_block1 :: { Maybe Ident -> Instruction }
    : 'result' valtype ')' list(instruction) 'end' opt(ident) {
        \ident -> BlockInstr ident [$2] (concat $4)
    }
    | foldedinstr1 list(instruction) 'end' opt(ident) {
        \ident -> BlockInstr ident [] ($1 ++ concat $2)
    }

raw_loop :: { Maybe Ident -> Instruction }
    : 'end' opt(ident) { \ident -> LoopInstr ident [] [] }
    | raw_instr list(instruction) 'end' opt(ident) {
        \ident -> LoopInstr ident [] ($1 ++ concat $2)
    }
    | '(' raw_loop1 { $2 }

raw_loop1 :: { Maybe Ident -> Instruction }
    : 'result' valtype ')' list(instruction) 'end' opt(ident) {
        \ident -> LoopInstr ident [$2] (concat $4)
    }
    | foldedinstr1 list(instruction) 'end' opt(ident) {
        \ident -> LoopInstr ident [] ($1 ++ concat $2)
    }

raw_if_result :: { Maybe Ident -> [Instruction] }
    : raw_else { \ident ->  [IfInstr ident [] [] $1] }
    | raw_instr list(instruction) raw_else {
        \ident ->  [IfInstr ident [] ($1 ++ concat $2) $3]
    }
    | '(' raw_if_result1 { $2 }

raw_if_result1 :: { Maybe Ident -> [Instruction] }
    : 'result' valtype ')' list(instruction) raw_else {
        \ident -> [IfInstr ident [$2] (concat $4) $5]
    }
    | foldedinstr1 list(instruction) raw_else {
        \ident -> [IfInstr ident [] ($1 ++ concat $2) $3]
    }

raw_else :: { [Instruction] }
    : 'end' opt(ident) { [] }
    | 'else' opt(ident) list(instruction) 'end' opt(ident) { concat $3 }

raw_call_indirect :: { [Instruction] }
    : '(' raw_call_indirect_typeuse { (PlainInstr $ CallIndirect $ fst $2) : snd $2 }
    | {- empty -} { [PlainInstr $ CallIndirect $ AnonimousTypeUse $ FuncType [] []] }

raw_call_indirect_typeuse :: { (TypeUse, [Instruction]) }
    : 'type' typeidx ')' raw_call_indirect_functype {
        (IndexedTypeUse $2 $ fst $4, snd $4)
    }
    | raw_call_indirect_functype1 {
        (AnonimousTypeUse $ fromMaybe (FuncType [] []) $ fst $1, snd $1)
    }

raw_call_indirect_functype :: { (Maybe FuncType, [Instruction]) }
    : '(' raw_call_indirect_functype1 { $2 }
    | {- empty -} { (Nothing, []) }

raw_call_indirect_functype1 :: { (Maybe FuncType, [Instruction]) }
    : 'param' list(valtype) ')' raw_call_indirect_functype {
        let ft = fromMaybe emptyFuncType $ fst $4 in
        (Just $ ft { params = map (ParamType Nothing) $2 ++ params ft }, snd $4)
    }
    | 'param' ident valtype ')' raw_call_indirect_functype {
        let ft = fromMaybe emptyFuncType $ fst $5 in
        (Just $ ft { params = (ParamType (Just $2) $3) : params ft }, snd $5)
    }
    | raw_call_indirect_return_functype1 { $1 }

raw_call_indirect_return_functype :: { (Maybe FuncType, [Instruction]) }
    : '(' raw_call_indirect_return_functype1 { $2 }
    | {- empty -} { (Nothing, []) }

raw_call_indirect_return_functype1 :: { (Maybe FuncType, [Instruction]) }
    : 'result' list(valtype) ')' raw_call_indirect_return_functype {
        let ft = fromMaybe emptyFuncType $ fst $4 in
        (Just $ ft { results = $2 ++ results ft }, snd $4)
    }
    | foldedinstr1 { (Nothing, $1) }

foldedinstr :: { [Instruction] }
    : '(' foldedinstr1 { $2 }

foldedinstr1 :: { [Instruction] }
    : plaininstr list(foldedinstr) ')' { concat $2 ++ [PlainInstr $1] }
    | 'call_indirect' folded_call_indirect { $2 }
    | 'block' opt(ident) folded_block { [$3 $2] }
    | 'loop' opt(ident) folded_loop { [$3 $2] }
    | 'if' opt(ident) '(' folded_if_result { $4 $2 }

folded_block :: { Maybe Ident -> Instruction }
    : ')' { \ident -> BlockInstr ident [] [] }
    | '(' folded_block1 { $2 }
    | raw_instr list(instruction) ')' { \ident -> BlockInstr ident [] ($1 ++ concat $2) }

folded_block1 :: { Maybe Ident -> Instruction }
    : 'result' valtype ')' list(instruction) ')' { \ident -> BlockInstr ident [$2] (concat $4) }
    | foldedinstr1 list(instruction) ')' { \ident -> BlockInstr ident [] ($1 ++ concat $2) }

folded_loop :: { Maybe Ident -> Instruction }
    : ')' { \ident -> LoopInstr ident [] [] }
    | '(' folded_loop1 { $2 }
    | raw_instr list(instruction) ')' { \ident -> LoopInstr ident [] ($1 ++ concat $2) }

folded_loop1 :: { Maybe Ident -> Instruction }
    : 'result' valtype ')' list(instruction) ')' { \ident -> LoopInstr ident [$2] (concat $4) }
    | foldedinstr1 list(instruction) ')' { \ident -> LoopInstr ident [] ($1 ++ concat $2) }

folded_if_result :: { Maybe Ident -> [Instruction] }
    : 'result' valtype ')' '(' folded_then_else {
        \ident -> [IfInstr ident [$2] (fst $5) (snd $5)]
    }
    | 'result' valtype ')' '(' foldedinstr1 '(' folded_then_else {
        \ident -> $5 ++ [IfInstr ident [$2] (fst $7) (snd $7)]
    }
    | folded_if { $1 }

folded_if :: { Maybe Ident -> [Instruction] }
    : folded_then_else { \ident -> [IfInstr ident [] (fst $1) (snd $1)] }
    | foldedinstr1 '(' folded_then_else { \ident -> $1 ++ [IfInstr ident [] (fst $3) (snd $3)] }

folded_then_else :: { ([Instruction], [Instruction]) }
    : 'then' list(instruction) ')' folded_else { (concat $2, $4)}

folded_else :: { [Instruction] }
    : ')' { [] }
    | '(' 'else' list(instruction) ')' ')' { concat $3 }

folded_call_indirect :: { [Instruction] }
    : ')' { [PlainInstr $ CallIndirect $ AnonimousTypeUse $ FuncType [] []] }
    | '(' folded_call_indirect_typeuse { snd $2 ++ [PlainInstr $ CallIndirect $ fst $2] }

folded_call_indirect_typeuse :: { (TypeUse, [Instruction]) }
    : 'type' typeidx ')' folded_call_indirect_functype {
        (IndexedTypeUse $2 $ fst $4, snd $4)
    }
    | folded_call_indirect_functype1 {
        (AnonimousTypeUse $ fromMaybe (FuncType [] []) $ fst $1, snd $1)
    }

folded_call_indirect_functype :: { (Maybe FuncType, [Instruction]) }
    : '(' folded_call_indirect_functype1 { $2 }
    | ')' { (Nothing, []) }

folded_call_indirect_functype1 :: { (Maybe FuncType, [Instruction]) }
    : paramsresulttypeuse folded_call_indirect_functype {
        (Just $ mergeFuncType $1 $ fromMaybe emptyFuncType $ fst $2, snd $2)
    }
    | foldedinstr1 list(foldedinstr) ')' { (Nothing, $1 ++ concat $2) }

importdesc :: { ImportDesc }
    : 'func' opt(ident) typeuse ')' { ImportFunc $2 $3 }
    | 'table' opt(ident) tabletype ')' { ImportTable $2 $3 }
    | 'memory' opt(ident) limits ')' { ImportMemory $2 $3 }
    | 'global' opt(ident) globaltype ')' { ImportGlobal $2 $3 }

import :: { Import }
    : 'import' name name '(' importdesc ')' { Import $2 $3 $5 }

-- FUNCTION --
function :: { [ModuleField] }
    : 'func' opt(ident) export_import_typeuse_locals_body { $3 $2 }

export_import_typeuse_locals_body :: { Maybe Ident -> [ModuleField] }
    : ')' { \i -> [MFFunc Nothing $ emptyFunction { ident = i }] }
    | raw_instr list(instruction) ')' {
        \i -> [MFFunc Nothing $ emptyFunction { ident = i, body = $1 ++ concat $2 }]
    }
    | '(' export_import_typeuse_locals_body1 { $2 }

export_import_typeuse_locals_body1 :: { Maybe Ident -> [ModuleField] }
    : 'export' name ')' export_import_typeuse_locals_body {
        \ident -> (MFExport $ Export $2 $ ExportFunc (Named `fmap` ident)) : ($4 ident)
    }
    | import_typeuse_locals_body1 { \ident -> [$1 ident] }

import_typeuse_locals_body1 :: { Maybe Ident -> ModuleField }
    : 'import' name name ')' typeuse ')' {
        \ident -> MFImport $ Import $2 $3 $ ImportFunc ident $5
    }
    | typeuse_locals_body1 { MFFunc Nothing . $1 }

typeuse_locals_body1 :: { Maybe Ident -> Function }
    : 'type' typeidx ')' signature_locals_body {
        \i ->
            let (AnonimousTypeUse signature) = funcType $4 in
            let typeSign = if signature == emptyFuncType then Nothing else Just signature in
            $4 { funcType = IndexedTypeUse $2 typeSign, ident = i }
    }
    | signature_locals_body1 { \i -> $1 { ident = i } }

signature_locals_body :: { Function }
    : ')' { emptyFunction }
    | '(' signature_locals_body1 { $2 }

signature_locals_body1 :: { Function }
    : 'param' list(valtype) ')' signature_locals_body {
        prependFuncParams (map (ParamType Nothing) $2) $4
    }
    | 'param' ident valtype ')' signature_locals_body {
        prependFuncParams [ParamType (Just $2) $3] $5
    }
    | result_locals_body1 { $1 }

result_locals_body :: { Function }
    : ')' { emptyFunction }
    | '(' result_locals_body1 { $2 }
    | raw_instr list(instruction) ')' { emptyFunction { body = $1 ++ concat $2 } }

result_locals_body1 :: { Function }
    : 'result' list(valtype) ')' result_locals_body {
        prependFuncResults $2 $4
    }
    | locals_body1 {
        emptyFunction { locals = fst $1, body = snd $1 }
    }

locals_body :: { ([LocalType], [Instruction]) }
    : ')' { ([], []) }
    | raw_instr list(instruction) ')' { ([], $1 ++ concat $2)}
    | '(' locals_body1 { $2 }

locals_body1 :: { ([LocalType], [Instruction]) }
    : 'local' list(valtype) ')' locals_body { (map (LocalType Nothing) $2 ++ fst $4, snd $4) }
    | 'local' ident valtype ')' locals_body { (LocalType (Just $2) $3 : fst $5, snd $5) }
    | foldedinstr1 list(instruction) ')' { ([], $1 ++ concat $2) }

-- FUNCTION END --

-- GLOBAL --

global :: { [ModuleField] }
    : 'global' opt(ident) global_type_export_import { $3 $2 }

globaltype :: { GlobalType }
    : valtype { Const $1 }
    | '(' 'mut' valtype ')' { Mut $3 }

global_type_export_import :: { Maybe Ident -> [ModuleField] }
    : valtype list(instruction) ')' { \ident -> [MFGlobal $ Global ident (Const $1) $ concat $2] }
    | '(' global_mut_export_import { $2 }

global_mut_export_import :: { Maybe Ident -> [ModuleField] }
    : 'mut' valtype ')' list(instruction) ')' { \ident -> [MFGlobal $ Global ident (Mut $2) $ concat $4] }
    | 'export' name ')' global_type_export_import {
        \ident -> (MFExport $ Export $2 $ ExportGlobal $ Named `fmap` ident) : ($4 ident)
    }
    | 'import' name name ')' globaltype ')' {
        \ident -> [MFImport $ Import $2 $3 $ ImportGlobal ident $5]
    }

-- GLOBAL END --

-- MEMORY --

memory :: { [ModuleField] }
    : 'memory' opt(ident) memory_limits_export_import { $3 $2 }

memory_limits_export_import :: { Maybe Ident -> [ModuleField] }
    : memory_limits { $1 }
    | '(' memory_limits_export_import1 { $2 }

datastring :: { LBS.ByteString }
    : list(str) { LBS.concat $1 }

memory_limits_export_import1 :: { Maybe Ident -> [ModuleField] }
    : 'export' name ')' memory_limits_export_import {
        \ident -> (MFExport $ Export $2 $ ExportMemory $ Named `fmap` ident) : $4 ident
    }
    | 'import' name name ')' limits ')' {
        \ident -> [MFImport $ Import $2 $3 $ ImportMemory ident $5]
    }
    | 'data' datastring ')' ')' {
        \ident ->
            let m = fromIntegral $ LBS.length $2 in
            [
                MFMem $ Memory ident $ Limit m $ Just m,
                MFData $ DataSegment (fromMaybe (Index 0) $ Named `fmap` ident) [PlainInstr $ I32Const 0] $2
            ]
    }

memory_limits :: { Maybe Ident -> [ModuleField] }
    : limits ')' { \ident -> [MFMem $ Memory ident $1] }

-- MEMOTY END --

-- TABLE --
limits :: { Limit }
    : u32 opt(u32) { Limit (fromIntegral $1) (fromIntegral `fmap` $2) }

elemtype :: { ElemType }
    : 'anyfunc' { AnyFunc }

tabletype :: { TableType }
    : limits elemtype { TableType $1 $2 }

table :: { [ModuleField] }
    : 'table' opt(ident) limits_elemtype_elem { $3 $2 }

limits_elemtype_elem :: { Maybe Ident -> [ModuleField] }
    : tabletype ')' { \ident -> [MFTable $ Table ident $1] }
    | elemtype '(' 'elem' list(funcidx) ')' ')' {
        \ident ->
            let funcsLen = fromIntegral $ length $4 in [
                MFTable $ Table ident $ TableType (Limit funcsLen (Just funcsLen)) $1,
                MFElem $ ElemSegment (fromMaybe (Index 0) $ Named `fmap` ident) [PlainInstr $ I32Const 0] $4
            ]
    }
    | '(' import_export_table { $2 }

import_export_table :: { Maybe Ident -> [ModuleField] }
    : 'import' name name ')' tabletype ')' {
        \ident -> [MFImport $ Import $2 $3 $ ImportTable ident $5]
    }
    | 'export' name ')' limits_elemtype_elem {
        \ident -> (MFExport $ Export $2 $ ExportTable $ Named `fmap` ident) : ($4 ident)
    }

-- TABLE END --

exportdesc :: { ExportDesc }
    : 'func' funcidx ')' { ExportFunc (Just $2) }
    | 'table' tableidx ')' { ExportTable (Just $2) }
    | 'memory' memidx ')' { ExportMemory (Just $2) }
    | 'global' globalidx ')' { ExportGlobal (Just $2) }

export :: { Export }
    : 'export' name '(' exportdesc ')' { Export $2 $4 }

start :: { StartFunction }
    : 'start' funcidx ')' { StartFunction $2 }

-- TODO: Spec from 09 Jan 2018 declares 'offset' keyword as mandatory,
-- but collection of testcases omits 'offset' in this position
-- I am going to support both options for now, but maybe it has to be updated in future.
offsetexpr :: { [Instruction] }
    : 'offset' list(foldedinstr) ')' { concat $2 }
    | foldedinstr1 { $1 }

elemsegment :: { ElemSegment }
    : 'elem' opt(tableidx) '(' offsetexpr list(funcidx) ')' { ElemSegment (fromMaybe (Index 0) $2) $4 $5 }

datasegment :: { DataSegment }
    : 'data' opt(memidx) '(' offsetexpr datastring ')' { DataSegment (fromMaybe (Index 0) $2) $4 $5 }

modulefield1_single :: { ModuleField }
    : typedef { MFType $1 }
    | import { MFImport $1 }
    | export { MFExport $1 }
    | start { MFStart $1 }
    | elemsegment { MFElem $1 }
    | datasegment { MFData $1 }

modulefield1_multi :: { [ModuleField] }
    : function { $1 }
    | table { $1 }
    | memory { $1 }
    | global { $1 }

modulefield1 :: { [ModuleField] }
    : modulefield1_single { [$1] }
    | modulefield1_multi { $1 }

modulefield :: { [ModuleField] }
    : '(' modulefield1 { $2 }

modAsFields :: { [ModuleField] }
    : '(' 'module' list(modulefield) ')' EOF { concat $3 }
    | '(' modulefield1 list(modulefield) EOF { $2 ++ concat $3}

mod :: { S.Module }
    : modAsFields { desugarize $1 }

-- Wasm Script Extended Grammar
script :: { Script }
    : list(command) EOF { $1 }

command :: { Command }
    : '(' command1 { $2 }

command1 :: { Command }
    : module1 { ModuleDef $1 }
    | 'register' string opt(ident) ')' { Register $2 $3 }
    | action1 { Action $1 }
    | assertion1 { Assertion $1 }
    | meta1 { Meta $1 }

module1 :: { ModuleDef }
    : 'module' opt(ident) 'binary' datastring ')' { BinaryModDef $2 $4 }
    | 'module' opt(ident) 'quote' list(string) ')' { TextModDef $2 (TL.concat $4) }
    | 'module' opt(ident) list(modulefield) ')' { RawModDef $2 (desugarize $ concat $3) }
    | modulefield1 list(modulefield) { RawModDef Nothing (desugarize $ $1 ++ concat $2) }

action1 :: { Action }
    : 'invoke' opt(ident) string list(foldedinstr) ')' { Invoke $2 $3 (map (map constInstructionToValue) $4) }
    | 'get' opt(ident) string ')' { Get $2 $3 }

assertion1 :: { Assertion }
    : 'assert_return' '(' action1 list(foldedinstr) ')' { AssertReturn $3 (map (map constInstructionToValue) $4) }
    | 'assert_return_canonical_nan' '(' action1 ')' { AssertReturnCanonicalNaN $3 }
    | 'assert_return_arithmetic_nan' '(' action1 ')' { AssertReturnArithmeticNaN $3 }
    | 'assert_trap' '(' assertion_trap string ')' { AssertTrap $3 $4 }
    | 'assert_malformed' '(' module1 string ')' { AssertMalformed $3 $4 }
    | 'assert_invalid' '(' module1 string ')' { AssertInvalid $3 $4 }
    | 'assert_unlinkable' '(' module1 string ')' { AssertUnlinkable $3 $4 }
    | 'assert_exhaustion' '(' action1 string ')' { AssertExhaustion $3 $4 }

assertion_trap :: { Either Action ModuleDef }
    : action1 { Left $1 }
    | module1 { Right $1 }

meta1 :: { Meta }
    : 'script' opt(ident) script ')' { Script $2 $3 }
    | 'input' opt(ident) string ')' { Input $2 $3 }
    | 'output' opt(ident) string ')' { Output $2 $3 }

-- utils

rev_list(p)
    : rev_list(p) p  { $2 : $1 }
    | {- empty -}    { [] }

rev_list1(p)
    : rev_list1(p) p { $2 : $1 }
    | p              { [$1] }

list(p)
    : rev_list(p)    { reverse $1 }

opt(p)
    : p { Just $1 }
    | {- empty -} { Nothing }

{

-- partial function by intention
prependFuncParams :: [ParamType] -> Function -> Function
prependFuncParams prep f@(Function { funcType = AnonimousTypeUse ft }) =
    f { funcType = AnonimousTypeUse $ ft { params = prep ++ params ft } }

prependFuncResults :: [ValueType] -> Function -> Function
prependFuncResults prep f@(Function { funcType = AnonimousTypeUse ft }) =
    f { funcType = AnonimousTypeUse $ ft { results = prep ++ results ft } }

mergeFuncType :: FuncType -> FuncType -> FuncType
mergeFuncType (FuncType lps lrs) (FuncType rps rrs) = FuncType (lps ++ rps) (lrs ++ rrs)

asUInt32 :: Integer -> Maybe Natural
asUInt32 val
    | val >= 0, val < 2 ^ 32 = Just $ fromIntegral val
    | otherwise = Nothing

asInt32 :: Integer -> Maybe Integer
asInt32 val
    | val >= -2 ^ 31, val < 2 ^ 32 = Just $ fromIntegral val
    | otherwise = Nothing

asInt64 :: Integer -> Maybe Integer
asInt64 val
    | val >= -2 ^ 63, val < 2 ^ 64 = Just $ fromIntegral val
    | otherwise = Nothing

asFloat32 :: Double -> Float
asFloat32 v = doubleToFloat v

asOffset :: LBS.ByteString -> Maybe Natural
asOffset str = do
    num <- TL.stripPrefix "offset=" $ TLEncoding.decodeUtf8 str
    fromIntegral . fst <$> eitherToMaybe (TLRead.decimal num)

asAlign :: LBS.ByteString -> Maybe Natural
asAlign str = do
    num <- TL.stripPrefix "align=" $ TLEncoding.decodeUtf8 str
    fromIntegral . fst <$> eitherToMaybe (TLRead.decimal num)

parseMemArg :: Natural -> Maybe Natural -> Maybe Natural -> Either String MemArg
parseMemArg defAlign optOffset optAlign =
    let offset = fromMaybe 0 optOffset in
    let align = fromIntegral $ round $ logBase 2 $ fromIntegral $ fromMaybe defAlign optAlign in
    if offset >= 2 ^ 32 || align >= 2 ^ 32
    then Left "u32 is out of boundaries"
    else return $ MemArg offset align

-- TODO: check name conditions.
-- Presuming the source text is itself encoded correctly,
-- strings that do not contain any uses of hexadecimal byte escapes are always valid names.
asName :: LBS.ByteString -> Maybe TL.Text
asName = Just . TLEncoding.decodeUtf8

asString :: LBS.ByteString -> Maybe TL.Text
asString bs =
    case TLEncoding.decodeUtf8' bs of
        Right t ->  Just t
        Left err -> Nothing

eitherToMaybe :: Either left right -> Maybe right
eitherToMaybe = either (const Nothing) Just

integerToWord32 :: Integer -> Word32
integerToWord32 i
    | i >= 0 && i <= 2 ^ 32 = fromIntegral i
    | i < 0 && i >= -(2 ^ 31) = 0xFFFFFFFF - (fromIntegral (abs i)) + 1
    | otherwise = error "I32 is out of bounds."

integerToWord64 :: Integer -> Word64
integerToWord64 i
    | i >= 0 && i <= 2 ^ 64 = fromIntegral i
    | i < 0 && i >= -(2 ^ 63) = 0xFFFFFFFFFFFFFFFF - (fromIntegral (abs i)) + 1
    | otherwise = error "I64 is out of bounds."

data FuncType = FuncType { params :: [ParamType], results :: [ValueType] } deriving (Show, Eq, Generic, NFData)

emptyFuncType :: FuncType
emptyFuncType = FuncType [] []

data ParamType = ParamType {
        ident :: Maybe Ident,
        paramType :: ValueType
    } deriving (Show, Eq, Generic, NFData)

newtype Ident = Ident TL.Text deriving (Show, Eq, Generic, NFData)

data Index = Named Ident | Index Natural deriving (Show, Eq, Generic, NFData)

type LabelIndex = Index
type FuncIndex = Index
type TypeIndex = Index
type LocalIndex = Index
type GlobalIndex = Index
type TableIndex = Index
type MemoryIndex = Index

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
    | I32Store8 MemArg
    | I32Store16 MemArg
    | I64Store8 MemArg
    | I64Store16 MemArg
    | I64Store32 MemArg
    | CurrentMemory
    | GrowMemory
    -- Numeric instructions
    | I32Const Integer
    | I64Const Integer
    | F32Const Float
    | F64Const Double
    | IUnOp BitSize IUnOp
    | IBinOp BitSize IBinOp
    | I32Eqz
    | I64Eqz
    | IRelOp BitSize IRelOp
    | FUnOp BitSize FUnOp
    | FBinOp BitSize FBinOp
    | FRelOp BitSize FRelOp
    | I32WrapI64
    | ITruncFU {- Int Size -} BitSize {- Float Size -} BitSize
    | ITruncFS {- Int Size -} BitSize {- Float Size -} BitSize
    | I64ExtendSI32
    | I64ExtendUI32
    | FConvertIU {- Float Size -} BitSize {- Int Size -} BitSize
    | FConvertIS {- Float Size -} BitSize {- Int Size -} BitSize
    | F32DemoteF64
    | F64PromoteF32
    | IReinterpretF BitSize
    | FReinterpretI BitSize
    deriving (Show, Eq, Generic, NFData)

data TypeDef = TypeDef (Maybe Ident) FuncType deriving (Show, Eq, Generic, NFData)

data TypeUse =
    IndexedTypeUse TypeIndex (Maybe FuncType)
    | AnonimousTypeUse FuncType
    deriving (Show, Eq, Generic, NFData)

data Instruction =
    PlainInstr PlainInstr
    | BlockInstr {
        label :: Maybe Ident,
        resultType :: [ValueType],
        body :: [Instruction]
    }
    | LoopInstr {
        label :: Maybe Ident,
        resultType :: [ValueType],
        body :: [Instruction]
    }
    | IfInstr {
        label :: Maybe Ident,
        resultType :: [ValueType],
        trueBranch :: [Instruction],
        falseBranch :: [Instruction]
    }
    deriving (Show, Eq, Generic, NFData)

data Import = Import {
        sourceModule :: TL.Text,
        name :: TL.Text,
        desc :: ImportDesc
    } deriving (Show, Eq, Generic, NFData)

data ImportDesc =
    ImportFunc (Maybe Ident) TypeUse
    | ImportTable (Maybe Ident) TableType
    | ImportMemory (Maybe Ident) Limit
    | ImportGlobal (Maybe Ident) GlobalType
    deriving (Show, Eq, Generic, NFData)

data LocalType = LocalType {
        ident :: Maybe Ident,
        localType :: ValueType
    } deriving (Show, Eq, Generic, NFData)

data Function = Function {
        ident :: Maybe Ident,
        funcType :: TypeUse,
        locals :: [LocalType],
        body :: [Instruction]
    }
    deriving (Show, Eq, Generic, NFData)

emptyFunction :: Function
emptyFunction =
    Function {
        ident = Nothing,
        funcType = AnonimousTypeUse emptyFuncType,
        locals = [],
        body = []
    }

data Global = Global {
        ident :: Maybe Ident,
        globalType :: GlobalType,
        initializer :: [Instruction]
    }
    deriving (Show, Eq, Generic, NFData)

data Memory = Memory (Maybe Ident) Limit deriving (Show, Eq, Generic, NFData)

data Table = Table (Maybe Ident) TableType deriving (Show, Eq, Generic, NFData)

data ExportDesc =
    ExportFunc (Maybe FuncIndex)
    | ExportTable (Maybe TableIndex)
    | ExportMemory (Maybe MemoryIndex)
    | ExportGlobal (Maybe GlobalIndex)
    deriving (Show, Eq, Generic, NFData)

data Export = Export {
        name :: TL.Text,
        desc :: ExportDesc
    }
    deriving (Show, Eq, Generic, NFData)

data StartFunction = StartFunction FuncIndex deriving (Show, Eq, Generic, NFData)

data ElemSegment = ElemSegment {
        tableIndex :: TableIndex,
        offset :: [Instruction],
        funcIndexes :: [FuncIndex]
    }
    deriving (Show, Eq, Generic, NFData)

data DataSegment = DataSegment {
        memIndex :: MemoryIndex,
        offset :: [Instruction],
        datastring :: LBS.ByteString
    }
    deriving (Show, Eq, Generic, NFData)

data ModuleField =
    MFType TypeDef
    | MFImport Import
    | MFFunc (Maybe Int) Function
    | MFTable Table
    | MFMem Memory
    | MFGlobal Global
    | MFExport Export
    | MFStart StartFunction
    | MFElem ElemSegment
    | MFData DataSegment
    deriving(Show, Eq, Generic, NFData)

happyError (Lexeme _ EOF : []) = Left $ "Error occuried during parsing phase at the end of file"
happyError (Lexeme (AlexPn abs line col) tok : tokens) = Left $
    "Error occuried during parsing phase. " ++
    "Line " ++ show line ++ ", " ++
    "Column " ++ show col ++ ", " ++
    "Token " ++ show tok ++ ". " ++
    "Token lookahed: " ++ show (take 3 tokens)

data Module = Module {
    types :: [TypeDef],
    functions :: [Function],
    tables :: [Table],
    mems :: [Memory],
    globals :: [Global],
    elems :: [ElemSegment],
    datas :: [DataSegment],
    start :: Maybe StartFunction,
    imports :: [Import],
    exports :: [Export]
} deriving (Show, Eq)

type Script = [Command]

type Expression = [Instruction]

data ModuleDef
    = RawModDef (Maybe Ident) S.Module
    | TextModDef (Maybe Ident) TL.Text
    | BinaryModDef (Maybe Ident) LBS.ByteString
    deriving (Show, Eq)

data Command
    = ModuleDef ModuleDef
    | Register TL.Text (Maybe Ident)
    | Action Action
    | Assertion Assertion
    | Meta Meta
    deriving (Show, Eq)

data Action
    = Invoke (Maybe Ident) TL.Text [[S.Instruction]]
    | Get (Maybe Ident) TL.Text
    deriving (Show, Eq)

type FailureString = TL.Text

data Assertion
    = AssertReturn Action [[S.Instruction]]
    | AssertReturnCanonicalNaN Action
    | AssertReturnArithmeticNaN Action
    | AssertTrap (Either Action ModuleDef) FailureString
    | AssertMalformed ModuleDef FailureString
    | AssertInvalid ModuleDef FailureString
    | AssertUnlinkable ModuleDef FailureString
    | AssertExhaustion Action FailureString
    deriving (Show, Eq)

data Meta
    = Script (Maybe Ident) Script
    | Input (Maybe Ident) TL.Text
    | Output (Maybe Ident) TL.Text
    deriving (Show, Eq)

type Labels = [Maybe Ident]

data FunCtx = FunCtx {
    ctxMod :: Module,
    ctxLabels :: Labels,
    ctxLocals :: [LocalType],
    ctxParams :: [ParamType]
} deriving (Eq, Show)

constInstructionToValue :: Instruction -> S.Instruction
constInstructionToValue (PlainInstr (I32Const v)) = S.I32Const $ integerToWord32 v
constInstructionToValue (PlainInstr (F32Const v)) = S.F32Const v
constInstructionToValue (PlainInstr (I64Const v)) = S.I64Const $ integerToWord64 v
constInstructionToValue (PlainInstr (F64Const v)) = S.F64Const v
constInstructionToValue _ = error "Only const instructions supported as arguments for actions"

desugarize :: [ModuleField] -> S.Module
desugarize fields =
    let mod = Module {
        types = reverse $ foldl' extractTypeDef (reverse $ explicitTypeDefs fields) fields,
        functions = extract extractFunction fields,
        tables = extract extractTable fields,
        imports = extract extractImport fields,
        mems = extract extractMemory fields,
        globals = extract extractGlobal fields,
        elems = extract extractElemSegment fields,
        datas = extract extractDataSegment fields,
        start = extractStart fields,
        exports = []
    } in
    S.Module {
        S.types = map synTypeDefToStruct $ types mod,
        S.functions = map (synFunctionToStruct mod) $ functions mod,
        S.tables = map synTableToStruct $ tables mod,
        S.imports = map (synImportToStruct $ types mod) $ imports mod,
        S.elems = map (synElemToStruct mod) $ elems mod,
        S.datas = map (synDataToStruct mod) $ datas mod,
        S.mems = map synMemoryToStruct $ mems mod,
        S.globals = map (synGlobalToStruct mod) $ globals mod,
        S.start = fmap (synStartToStruct mod) $ start mod,
        S.exports = synExportsToStruct mod $ appendIndexToFuncs fields
    }
    where
        -- utils
        extract :: ([a] -> ModuleField -> [a]) -> [ModuleField] -> [a]
        extract extractor = reverse . foldl' extractor []

        findWithIndex :: (a -> Bool) -> [a] -> Maybe (a, Int)
        findWithIndex pred l = find (pred . fst) $ zip l [0..]

        -- types
        synTypeDefToStruct :: TypeDef -> S.FuncType
        synTypeDefToStruct (TypeDef _ FuncType { params, results }) =
            S.FuncType (map paramType params) results

        explicitTypeDefs :: [ModuleField] -> [TypeDef]
        explicitTypeDefs = map (\(MFType def) -> def) . filter isTypeDef
            where
                isTypeDef (MFType _) = True
                isTypeDef _ = False
        
        extractTypeDef :: [TypeDef] -> ModuleField -> [TypeDef]
        extractTypeDef defs (MFType _) = defs -- should be extracted before implicit defs
        extractTypeDef defs (MFImport Import { desc = ImportFunc _ typeUse }) =
            matchTypeUse defs typeUse
        extractTypeDef defs (MFFunc _ Function { funcType, body }) =
            extractTypeDefFromInstructions (matchTypeUse defs funcType) body
        extractTypeDef defs (MFGlobal Global { initializer }) =
            extractTypeDefFromInstructions defs initializer
        extractTypeDef defs (MFElem ElemSegment { offset }) =
            extractTypeDefFromInstructions defs offset
        extractTypeDef defs (MFData DataSegment { offset }) =
            extractTypeDefFromInstructions defs offset
        extractTypeDef defs _ = defs

        extractTypeDefFromInstructions :: [TypeDef] -> [Instruction] -> [TypeDef]
        extractTypeDefFromInstructions = foldl' extractTypeDefFromInstruction

        extractTypeDefFromInstruction :: [TypeDef] -> Instruction -> [TypeDef]
        extractTypeDefFromInstruction defs (PlainInstr (CallIndirect typeUse)) =
            matchTypeUse defs typeUse
        extractTypeDefFromInstruction defs (BlockInstr { body }) =
            extractTypeDefFromInstructions defs body
        extractTypeDefFromInstruction defs (LoopInstr { body }) =
            extractTypeDefFromInstructions defs body
        extractTypeDefFromInstruction defs (IfInstr { trueBranch, falseBranch }) =
            extractTypeDefFromInstructions defs $ trueBranch ++ falseBranch
        extractTypeDefFromInstruction defs _ = defs

        funcTypesEq :: FuncType -> FuncType -> Bool
        funcTypesEq l r =
            let paramTypes = map paramType . params in
            paramTypes l == paramTypes r && results l == results r

        matchTypeFunc :: FuncType -> TypeDef -> Bool
        matchTypeFunc funcType (TypeDef _ ft) = funcTypesEq ft funcType

        matchTypeUse :: [TypeDef] -> TypeUse -> [TypeDef]
        matchTypeUse defs (AnonimousTypeUse funcType) =
            if any (matchTypeFunc funcType) defs
            then defs
            else (TypeDef Nothing funcType) : defs
        matchTypeUse defs _ = defs

        getTypeIndex :: [TypeDef] -> TypeUse -> Maybe Natural
        getTypeIndex defs (AnonimousTypeUse funcType) =
            fromIntegral <$> findIndex (matchTypeFunc funcType) defs
        getTypeIndex defs (IndexedTypeUse (Named ident) (Just funcType)) = do
            (def, idx) <- findWithIndex (\(TypeDef i _) -> i == Just ident) defs
            guard $ matchTypeFunc funcType def
            return $ fromIntegral idx
        getTypeIndex defs (IndexedTypeUse (Named ident) Nothing) =
            fromIntegral <$> findIndex (\(TypeDef i _) -> i == Just ident) defs
        getTypeIndex defs (IndexedTypeUse (Index n) (Just funcType)) = do
            guard $ matchTypeFunc funcType $ defs !! fromIntegral n
            return n
        getTypeIndex defs (IndexedTypeUse (Index n) Nothing) = return n
        
        -- imports
        synImportToStruct :: [TypeDef] -> Import -> S.Import
        synImportToStruct defs (Import mod name (ImportFunc _ typeUse)) =
            case getTypeIndex defs typeUse of
                Just idx -> S.Import mod name $ S.ImportFunc idx
                Nothing -> error $ "cannot find type index for function import: " ++ show typeUse
        synImportToStruct _ (Import mod name (ImportTable _ tableType)) =
            S.Import mod name $ S.ImportTable tableType
        synImportToStruct _ (Import mod name (ImportMemory _ limit)) =
            S.Import mod name $ S.ImportMemory limit
        synImportToStruct _ (Import mod name (ImportGlobal _ globalType)) =
            S.Import mod name $ S.ImportGlobal globalType

        extractImport :: [Import] -> ModuleField -> [Import]
        extractImport imports (MFImport imp) = imp : imports
        extractImport imports _ = imports

        -- functions
        synInstrToStruct :: FunCtx -> Instruction -> S.Instruction
        synInstrToStruct _ (PlainInstr Unreachable) = S.Unreachable
        synInstrToStruct _ (PlainInstr Nop) = S.Nop
        synInstrToStruct ctx (PlainInstr (Br labelIdx)) =
            fromJust $ S.Br <$> getLabelIdx ctx labelIdx
        synInstrToStruct ctx (PlainInstr (BrIf labelIdx)) =
            fromJust $ S.BrIf <$> getLabelIdx ctx labelIdx
        synInstrToStruct ctx (PlainInstr (BrTable lbls lbl)) =
            S.BrTable (map (fromJust . getLabelIdx ctx) lbls) $ fromJust $ getLabelIdx ctx lbl
        synInstrToStruct _ (PlainInstr Return) = S.Return
        synInstrToStruct FunCtx { ctxMod } (PlainInstr (Call funIdx)) =
            S.Call $ fromJust $ getFuncIndex ctxMod funIdx
        synInstrToStruct FunCtx { ctxMod = Module { types } } (PlainInstr (CallIndirect typeUse)) =
            fromJust $ S.CallIndirect <$> getTypeIndex types typeUse
        synInstrToStruct _ (PlainInstr Drop) = S.Drop
        synInstrToStruct _ (PlainInstr Select) = S.Select
        synInstrToStruct ctx (PlainInstr (GetLocal localIdx)) =
            S.GetLocal $ fromJust $ getLocalIndex ctx localIdx
        synInstrToStruct ctx (PlainInstr (SetLocal localIdx)) =
            S.SetLocal $ fromJust $ getLocalIndex ctx localIdx
        synInstrToStruct ctx (PlainInstr (TeeLocal localIdx)) =
            S.TeeLocal $ fromJust $ getLocalIndex ctx localIdx
        synInstrToStruct FunCtx { ctxMod } (PlainInstr (GetGlobal globalIdx)) =
            S.GetGlobal $ fromJust $ getGlobalIndex ctxMod globalIdx
        synInstrToStruct FunCtx { ctxMod } (PlainInstr (SetGlobal globalIdx)) =
            S.SetGlobal $ fromJust $ getGlobalIndex ctxMod globalIdx
        synInstrToStruct _ (PlainInstr (I32Load memArg)) = S.I32Load memArg
        synInstrToStruct _ (PlainInstr (I64Load memArg)) = S.I64Load memArg
        synInstrToStruct _ (PlainInstr (F32Load memArg)) = S.F32Load memArg
        synInstrToStruct _ (PlainInstr (F64Load memArg)) = S.F64Load memArg
        synInstrToStruct _ (PlainInstr (I32Load8S memArg)) = S.I32Load8S memArg
        synInstrToStruct _ (PlainInstr (I32Load8U memArg)) = S.I32Load8U memArg
        synInstrToStruct _ (PlainInstr (I32Load16S memArg)) = S.I32Load16S memArg
        synInstrToStruct _ (PlainInstr (I32Load16U memArg)) = S.I32Load16U memArg
        synInstrToStruct _ (PlainInstr (I64Load8S memArg)) = S.I64Load8S memArg
        synInstrToStruct _ (PlainInstr (I64Load8U memArg)) = S.I64Load8U memArg
        synInstrToStruct _ (PlainInstr (I64Load16S memArg)) = S.I64Load16S memArg
        synInstrToStruct _ (PlainInstr (I64Load16U memArg)) = S.I64Load16U memArg
        synInstrToStruct _ (PlainInstr (I64Load32S memArg)) = S.I64Load32S memArg
        synInstrToStruct _ (PlainInstr (I64Load32U memArg)) = S.I64Load32U memArg
        synInstrToStruct _ (PlainInstr (I32Store memArg)) = S.I32Store memArg
        synInstrToStruct _ (PlainInstr (I64Store memArg)) = S.I64Store memArg
        synInstrToStruct _ (PlainInstr (F32Store memArg)) = S.F32Store memArg
        synInstrToStruct _ (PlainInstr (F64Store memArg)) = S.F64Store memArg
        synInstrToStruct _ (PlainInstr (I32Store8 memArg)) = S.I32Store8 memArg
        synInstrToStruct _ (PlainInstr (I32Store16 memArg)) = S.I32Store16 memArg
        synInstrToStruct _ (PlainInstr (I64Store8 memArg)) = S.I64Store8 memArg
        synInstrToStruct _ (PlainInstr (I64Store16 memArg)) = S.I64Store16 memArg
        synInstrToStruct _ (PlainInstr (I64Store32 memArg)) = S.I64Store32 memArg
        synInstrToStruct _ (PlainInstr CurrentMemory) = S.CurrentMemory
        synInstrToStruct _ (PlainInstr GrowMemory) = S.GrowMemory
        synInstrToStruct _ (PlainInstr (I32Const val)) = S.I32Const $ integerToWord32 val
        synInstrToStruct _ (PlainInstr (I64Const val)) = S.I64Const $ integerToWord64 val
        synInstrToStruct _ (PlainInstr (F32Const val)) = S.F32Const val
        synInstrToStruct _ (PlainInstr (F64Const val)) = S.F64Const val
        synInstrToStruct _ (PlainInstr (IUnOp sz op)) = S.IUnOp sz op
        synInstrToStruct _ (PlainInstr (IBinOp sz op)) = S.IBinOp sz op
        synInstrToStruct _ (PlainInstr I32Eqz) = S.I32Eqz
        synInstrToStruct _ (PlainInstr I64Eqz) = S.I64Eqz
        synInstrToStruct _ (PlainInstr (IRelOp sz op)) = S.IRelOp sz op
        synInstrToStruct _ (PlainInstr (FUnOp sz op)) = S.FUnOp sz op
        synInstrToStruct _ (PlainInstr (FBinOp sz op)) = S.FBinOp sz op
        synInstrToStruct _ (PlainInstr (FRelOp sz op)) = S.FRelOp sz op
        synInstrToStruct _ (PlainInstr I32WrapI64) = S.I32WrapI64
        synInstrToStruct _ (PlainInstr (ITruncFU sz sz')) = S.ITruncFU sz sz'
        synInstrToStruct _ (PlainInstr (ITruncFS sz sz')) = S.ITruncFS sz sz'
        synInstrToStruct _ (PlainInstr I64ExtendSI32) = S.I64ExtendSI32
        synInstrToStruct _ (PlainInstr I64ExtendUI32) = S.I64ExtendUI32
        synInstrToStruct _ (PlainInstr (FConvertIU sz sz')) = S.FConvertIU sz sz'
        synInstrToStruct _ (PlainInstr (FConvertIS sz sz')) = S.FConvertIS sz sz'
        synInstrToStruct _ (PlainInstr F32DemoteF64) = S.F32DemoteF64
        synInstrToStruct _ (PlainInstr F64PromoteF32) = S.F64PromoteF32
        synInstrToStruct _ (PlainInstr (IReinterpretF sz)) = S.IReinterpretF sz
        synInstrToStruct _ (PlainInstr (FReinterpretI sz)) = S.FReinterpretI sz
        synInstrToStruct ctx BlockInstr {label, resultType, body} =
            let ctx' = ctx { ctxLabels = label : ctxLabels ctx } in
            S.Block resultType $ map (synInstrToStruct ctx') body
        synInstrToStruct ctx LoopInstr {label, resultType, body} =
            let ctx' = ctx { ctxLabels = label : ctxLabels ctx } in
            S.Loop resultType $ map (synInstrToStruct ctx') body
        synInstrToStruct ctx IfInstr {label, resultType, trueBranch, falseBranch} =
            let ctx' = ctx { ctxLabels = label : ctxLabels ctx } in
            let trueBranch' = map (synInstrToStruct ctx') trueBranch in
            let falseBranch' = map (synInstrToStruct ctx') falseBranch in
            S.If resultType trueBranch' falseBranch'
        
        synFunctionToStruct :: Module -> Function -> S.Function
        synFunctionToStruct mod Function { funcType, locals, body } =
            let typeIdx = fromJust $ getTypeIndex (types mod) funcType in
            -- we have to use local func params declaration,
            -- coz it can contain own names for them
            let
                params = case funcType of
                    IndexedTypeUse _ (Just FuncType { params }) -> params
                    AnonimousTypeUse FuncType { params } -> params
                    _ ->
                        if fromIntegral typeIdx < length (types mod)
                        then let TypeDef _ FuncType { params } = types mod !! fromIntegral typeIdx in params
                        else []
            in
            let ctx = FunCtx mod [] locals params in
            S.Function {
                S.funcType = typeIdx,
                S.localTypes = map localType locals,
                S.body = map (synInstrToStruct ctx) body
            }

        extractFunction :: [Function] -> ModuleField -> [Function]
        extractFunction funcs (MFFunc _ fun) = fun : funcs
        extractFunction funcs _ = funcs

        getLabelIdx :: FunCtx -> LabelIndex -> Maybe Natural
        getLabelIdx FunCtx { ctxLabels } (Named id) =
            fromIntegral <$> findIndex (\ident -> ident == Just id) ctxLabels
        getLabelIdx FunCtx { ctxLabels } (Index idx) =
            Just idx
        
        getLocalIndex :: FunCtx -> LabelIndex -> Maybe Natural
        getLocalIndex FunCtx {ctxParams, ctxLocals} (Named id) =
            case findIndex (\(ParamType ident _) -> ident == Just id) ctxParams of
                Just idx -> return $ fromIntegral idx
                Nothing ->
                    let isIdent (LocalType ident _) = ident == Just id in
                    fromIntegral . (+ length ctxParams) <$> findIndex isIdent ctxLocals
        getLocalIndex FunCtx {ctxParams, ctxLocals} (Index idx) = Just idx
        
        isFuncImport :: Import -> Bool
        isFuncImport Import { desc = ImportFunc _ _ } = True
        isFuncImport _ = False

        getFuncIndex :: Module -> FuncIndex -> Maybe Natural
        getFuncIndex Module { imports, functions } (Named id) =
            let funImports = filter isFuncImport imports in
            case findIndex (\(Import { desc = ImportFunc ident _ }) -> ident == Just id) funImports of
                Just idx -> return $ fromIntegral idx
                Nothing ->
                    let isIdent (Function { ident }) = ident == Just id in
                    fromIntegral . (+ length funImports) <$> findIndex isIdent functions
        getFuncIndex Module { imports, functions } (Index idx) = Just idx

        -- tables
        synTableToStruct :: Table -> S.Table
        synTableToStruct (Table _ tableType) = S.Table tableType

        extractTable :: [Table] -> ModuleField -> [Table]
        extractTable tables (MFTable table) = table : tables
        extractTable tables _ = tables

        isTableImport :: Import -> Bool
        isTableImport Import { desc = ImportTable _ _ } = True
        isTableImport _ = False

        getTableIndex :: Module -> TableIndex -> Maybe Natural
        getTableIndex Module { imports, tables } (Named id) =
            let tableImports = filter isTableImport imports in
            case findIndex (\(Import { desc = ImportTable ident _ }) -> ident == Just id) tableImports of
                Just idx -> return $ fromIntegral idx
                Nothing ->
                    let isIdent (Table (Just id) _) = True in
                    fromIntegral . (+ length tableImports) <$> findIndex isIdent tables
        getTableIndex Module { imports, tables } (Index idx) = Just idx

        -- memory
        synMemoryToStruct :: Memory -> S.Memory
        synMemoryToStruct (Memory _ limits) = S.Memory limits

        extractMemory :: [Memory] -> ModuleField -> [Memory]
        extractMemory mems (MFMem mem) = mem : mems
        extractMemory mems _ = mems

        isMemImport :: Import -> Bool
        isMemImport Import { desc = ImportMemory _ _ } = True
        isMemImport _ = False

        getMemIndex :: Module -> MemoryIndex -> Maybe Natural
        getMemIndex Module { imports, mems } (Named id) =
            let memImports = filter isMemImport imports in
            case findIndex (\(Import { desc = ImportMemory ident _ }) -> ident == Just id) memImports of
                Just idx -> return $ fromIntegral idx
                Nothing ->
                    let isIdent (Memory (Just id) _) = True in
                    fromIntegral . (+ length memImports) <$> findIndex isIdent mems
        getMemIndex Module { imports, mems } (Index idx) = Just idx

        -- global
        synGlobalToStruct :: Module -> Global -> S.Global
        synGlobalToStruct mod Global { globalType, initializer } =
            let ctx = FunCtx mod [] [] [] in
            S.Global globalType $ map (synInstrToStruct ctx) initializer

        extractGlobal :: [Global] -> ModuleField -> [Global]
        extractGlobal globals (MFGlobal global) = global : globals
        extractGlobal globals _ = globals

        isGlobalImport :: Import -> Bool
        isGlobalImport Import { desc = ImportGlobal _ _ } = True
        isGlobalImport _ = False

        getGlobalIndex :: Module -> GlobalIndex -> Maybe Natural
        getGlobalIndex Module { imports, globals } (Named id) =
            let globalImports = filter isGlobalImport imports in
            case findIndex (\(Import { desc = ImportGlobal ident _ }) -> ident == Just id) globalImports of
                Just idx -> return $ fromIntegral idx
                Nothing ->
                    let isIdent (Global { ident }) = ident == Just id in
                    fromIntegral . (+ length globalImports) <$> findIndex isIdent globals
        getGlobalIndex Module { imports, globals } (Index idx) = Just idx

        -- elem segment
        synElemToStruct :: Module -> ElemSegment -> S.ElemSegment
        synElemToStruct mod ElemSegment { tableIndex, offset, funcIndexes } =
            let ctx = FunCtx mod [] [] [] in
            let offsetInstrs = map (synInstrToStruct ctx) offset in
            let idx = fromJust $ getTableIndex mod tableIndex in
            let indexes = map (fromJust . getFuncIndex mod) funcIndexes in
            S.ElemSegment idx offsetInstrs indexes

        extractElemSegment :: [ElemSegment] -> ModuleField -> [ElemSegment]
        extractElemSegment elems (MFElem elem) = elem : elems
        extractElemSegment elems _ = elems

        -- data segment
        synDataToStruct :: Module -> DataSegment -> S.DataSegment
        synDataToStruct mod DataSegment { memIndex, offset, datastring } =
            let ctx = FunCtx mod [] [] [] in
            let offsetInstrs = map (synInstrToStruct ctx) offset in
            let idx = fromJust $ getMemIndex mod memIndex in
            S.DataSegment idx offsetInstrs datastring

        extractDataSegment :: [DataSegment] -> ModuleField -> [DataSegment]
        extractDataSegment datas (MFData dataSegment) = dataSegment : datas
        extractDataSegment datas _ = datas

        -- start
        synStartToStruct :: Module -> StartFunction -> S.StartFunction
        synStartToStruct mod (StartFunction funIdx) =
            S.StartFunction $ fromJust $ getFuncIndex mod funIdx
        
        extractStart :: [ModuleField] -> Maybe StartFunction
        extractStart = foldl' extractStart' Nothing

        extractStart' :: Maybe StartFunction -> ModuleField -> Maybe StartFunction
        extractStart' _ (MFStart start) = Just start
        extractStart' start _ = start

        -- exports
        appendIndexToFuncs :: [ModuleField] -> [ModuleField]
        appendIndexToFuncs mf = reverse $ snd $ foldl' appendIndexToFunc (0, []) mf
            where
                appendIndexToFunc :: (Int, [ModuleField]) -> ModuleField -> (Int, [ModuleField])
                appendIndexToFunc (idx, mf) (MFFunc _ fun) = (idx + 1, (MFFunc (Just idx) fun):mf)
                appendIndexToFunc (idx, mf) f = (idx, f:mf)
 
        synExportsToStruct :: Module -> [ModuleField] -> [S.Export]
        synExportsToStruct mod (MFExport Export { name, desc = ExportFunc Nothing } : rest) =
            let
                isFuncExport (MFExport Export { desc = ExportFunc Nothing }) = True
                isFuncExport _ = False
            in
            let getName (MFExport Export { name }) = name in
            let names = name : (map getName $ takeWhile isFuncExport rest) in
            let funImports = filter isFuncImport $ imports mod in
            let rest' =  dropWhile isFuncExport rest in
            let 
                idx = fromIntegral $ case head rest' of
                    MFImport imp -> fromJust $ findIndex (== imp) funImports
                    MFFunc (Just idx) fun -> length funImports + idx
                    _ -> error "export statement without index has to be followed with import or function"
            in
            map (\name -> S.Export name $ S.ExportFunc idx) names ++ synExportsToStruct mod rest'
        synExportsToStruct mod (MFExport Export { name, desc = ExportFunc (Just idx) } : rest) =
            let exp = S.Export name $ S.ExportFunc $ fromJust $ getFuncIndex mod idx in
            exp : synExportsToStruct mod rest
        synExportsToStruct mod (MFExport Export { name, desc = ExportTable Nothing } : rest) =
            let
                isTableExport (MFExport Export { desc = ExportTable Nothing }) = True
                isTableExport _ = False
            in
            let getName (MFExport Export { name }) = name in
            let names = name : (map getName $ takeWhile isTableExport rest) in
            let tableImports = filter isTableImport $ imports mod in
            let rest' =  dropWhile isTableExport rest in
            let 
                idx = fromIntegral $ case head rest' of
                    MFImport imp -> fromJust $ findIndex (== imp) tableImports
                    MFTable tab -> length tableImports + (fromJust $ findIndex (== tab) $ tables mod)
                    _ -> error "export statement without index has to be followed with import or table"
            in
            map (\name -> S.Export name $ S.ExportTable idx) names ++ synExportsToStruct mod rest'
        synExportsToStruct mod (MFExport Export { name, desc = ExportTable (Just idx) } : rest) =
            let exp = S.Export name $ S.ExportTable $ fromJust $ getTableIndex mod idx in
            exp : synExportsToStruct mod rest
        synExportsToStruct mod (MFExport Export { name, desc = ExportMemory Nothing } : rest) =
            let
                isMemExport (MFExport Export { desc = ExportMemory Nothing }) = True
                isMemExport _ = False
            in
            let getName (MFExport Export { name }) = name in
            let names = name : (map getName $ takeWhile isMemExport rest) in
            let memImports = filter isMemImport $ imports mod in
            let rest' =  dropWhile isMemExport rest in
            let 
                idx = fromIntegral $ case head rest' of
                    MFImport imp -> fromJust $ findIndex (== imp) memImports
                    MFMem mem -> length memImports + (fromJust $ findIndex (== mem) $ mems mod)
                    _ -> error "export statement without index has to be followed with import or memory"
            in
            map (\name -> S.Export name $ S.ExportMemory idx) names ++ synExportsToStruct mod rest'
        synExportsToStruct mod (MFExport Export { name, desc = ExportMemory (Just idx) } : rest) =
            let exp = S.Export name $ S.ExportMemory $ fromJust $ getMemIndex mod idx in
            exp : synExportsToStruct mod rest
        synExportsToStruct mod (MFExport Export { name, desc = ExportGlobal Nothing } : rest) =
            let
                isGlobalExport (MFExport Export { desc = ExportGlobal Nothing }) = True
                isGlobalExport _ = False
            in
            let getName (MFExport Export { name }) = name in
            let names = name : (map getName $ takeWhile isGlobalExport rest) in
            let globalImports = filter isGlobalImport $ imports mod in
            let rest' =  dropWhile isGlobalExport rest in
            let 
                idx = fromIntegral $ case head rest' of
                    MFImport imp -> fromJust $ findIndex (== imp) globalImports
                    MFGlobal global -> length globalImports + (fromJust $ findIndex (== global) $ globals mod)
                    _ -> error "export statement without index has to be followed with import or memory"
            in
            map (\name -> S.Export name $ S.ExportGlobal idx) names ++ synExportsToStruct mod rest'
        synExportsToStruct mod (MFExport Export { name, desc = ExportGlobal (Just idx) } : rest) =
            let exp = S.Export name $ S.ExportGlobal $ fromJust $ getGlobalIndex mod idx in
            exp : synExportsToStruct mod rest
        synExportsToStruct mod (_ : rest) = synExportsToStruct mod rest
        synExportsToStruct _ [] = []
}