{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.Wasm.Parser (
    parseModule
) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLEncoding
import qualified Data.Text.Lazy.Read as TLRead

import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.List (foldl')

import Numeric.Natural (Natural)

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
'i64.store8'          { Lexeme _ (TKeyword "i64.store") }
'i64.store16'         { Lexeme _ (TKeyword "i64.store") }
'i64.store32'         { Lexeme _ (TKeyword "i64.store") }
'current_memory'      { Lexeme _ (TKeyword "current_memory") }
'grow_memory'         { Lexeme _ (TKeyword "grow_memory") }
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
id                    { Lexeme _ (TId $$) }
u32                   { Lexeme _ (TIntLit (asUInt32 -> Just $$)) }
i32                   { Lexeme _ (TIntLit (asInt32 -> Just $$)) }
i64                   { Lexeme _ (TIntLit (asInt64 -> Just $$)) }
f32                   { Lexeme _ (TFloatLit (asFloat32 -> $$)) }
f64                   { Lexeme _ (TFloatLit (asFloat64 -> $$)) }
offset                { Lexeme _ (TKeyword (asOffset -> Just $$)) }
align                 { Lexeme _ (TKeyword (asAlign -> Just $$)) }
name                  { Lexeme _ (TStringLit (asName -> Just $$)) }
string                { Lexeme _ (TStringLit (asString -> Just $$)) }
EOF                   { Lexeme _ EOF }

%%

functype :: { FuncType }
    : '(' 'func' paramtypes resulttypes ')' { FuncType $3 $4 }

paramtypes :: { [ParamType] }
    : list(paramtype) { concat $1 }

paramtype :: { [ParamType] }
    : '(' 'param' ident valtype ')' { [ParamType (Just $3) $4] }
    | '(' 'param' list(valtype) ')' { map (ParamType Nothing) $3 }

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
    : '(' 'result' list(valtype) ')' { $3 }

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
    | 'i32.store8' memarg1           { I32Store8 $2 }
    | 'i32.store16' memarg2          { I32Store16 $2 }
    | 'i64.store8' memarg1           { I64Store8 $2 }
    | 'i64.store16' memarg2          { I64Store16 $2 }
    | 'i64.store32' memarg4          { I64Store32 $2 }
    | 'current_memory'               { CurrentMemory }
    | 'grow_memory'                  { GrowMemory }
    -- numeric instructions
    | 'i32.const' int32              { I32Const $2 }
    | 'i64.const' i64                { I64Const $2 }
    | 'f32.const' f32                { F32Const $2 }
    | 'f64.const' f64                { F64Const $2 }
    | 'i32.clz'                      { I32Clz }
    | 'i32.ctz'                      { I32Ctz }
    | 'i32.popcnt'                   { I32Popcnt }
    | 'i32.add'                      { I32Add }
    | 'i32.sub'                      { I32Sub }
    | 'i32.mul'                      { I32Mul }
    | 'i32.div_s'                    { I32DivS }
    | 'i32.div_u'                    { I32DivU }
    | 'i32.rem_s'                    { I32RemS }
    | 'i32.rem_u'                    { I32RemU }
    | 'i32.and'                      { I32And }
    | 'i32.or'                       { I32Or }
    | 'i32.xor'                      { I32Xor }
    | 'i32.shl'                      { I32Shl }
    | 'i32.shr_s'                    { I32ShrS }
    | 'i32.shr_u'                    { I32ShrU }
    | 'i32.rotl'                     { I32Rotl }
    | 'i32.rotr'                     { I32Rotr }
    | 'i64.clz'                      { I64Clz }
    | 'i64.ctz'                      { I64Ctz }
    | 'i64.popcnt'                   { I64Popcnt }
    | 'i64.add'                      { I64Add }
    | 'i64.sub'                      { I64Sub }
    | 'i64.mul'                      { I64Mul }
    | 'i64.div_s'                    { I64DivS }
    | 'i64.div_u'                    { I64DivU }
    | 'i64.rem_s'                    { I64RemS }
    | 'i64.rem_u'                    { I64RemU }
    | 'i64.and'                      { I64And }
    | 'i64.or'                       { I64Or }
    | 'i64.xor'                      { I64Xor }
    | 'i64.shl'                      { I64Shl }
    | 'i64.shr_s'                    { I64ShrS }
    | 'i64.shr_u'                    { I64ShrU }
    | 'i64.rotl'                     { I64Rotl }
    | 'i64.rotr'                     { I64Rotr }
    | 'f32.abs'                      { F32Abs }
    | 'f32.neg'                      { F32Neg }
    | 'f32.ceil'                     { F32Ceil }
    | 'f32.floor'                    { F32Floor }
    | 'f32.trunc'                    { F32Trunc }
    | 'f32.nearest'                  { F32Nearest }
    | 'f32.sqrt'                     { F32Sqrt }
    | 'f32.add'                      { F32Add }
    | 'f32.sub'                      { F32Sub }
    | 'f32.mul'                      { F32Mul }
    | 'f32.div'                      { F32Div }
    | 'f32.min'                      { F32Min }
    | 'f32.max'                      { F32Max }
    | 'f32.copysign'                 { F32Copysign }
    | 'f64.abs'                      { F64Abs }
    | 'f64.neg'                      { F64Neg }
    | 'f64.ceil'                     { F64Ceil }
    | 'f64.floor'                    { F64Floor }
    | 'f64.trunc'                    { F64Trunc }
    | 'f64.nearest'                  { F64Nearest }
    | 'f64.sqrt'                     { F64Sqrt }
    | 'f64.add'                      { F64Add }
    | 'f64.sub'                      { F64Sub }
    | 'f64.mul'                      { F64Mul }
    | 'f64.div'                      { F64Div }
    | 'f64.min'                      { F64Min }
    | 'f64.max'                      { F64Max }
    | 'f64.copysign'                 { F64Copysign }
    | 'i32.eqz'                      { I32Eqz }
    | 'i32.eq'                       { I32Eq }
    | 'i32.ne'                       { I32Ne }
    | 'i32.lt_s'                     { I32LtS }
    | 'i32.lt_u'                     { I32LtU }
    | 'i32.gt_s'                     { I32GtS }
    | 'i32.gt_u'                     { I32GtU }
    | 'i32.le_s'                     { I32LeS }
    | 'i32.le_u'                     { I32LeU }
    | 'i32.ge_s'                     { I32GeS }
    | 'i32.ge_u'                     { I32GeU }
    | 'i64.eqz'                      { I64Eqz }
    | 'i64.eq'                       { I64Eq }
    | 'i64.ne'                       { I64Ne }
    | 'i64.lt_s'                     { I64LtS }
    | 'i64.lt_u'                     { I64LtU }
    | 'i64.gt_s'                     { I64GtS }
    | 'i64.gt_u'                     { I64GtU }
    | 'i64.le_s'                     { I64LeS }
    | 'i64.le_u'                     { I64LeU }
    | 'i64.ge_s'                     { I64GeS }
    | 'i64.ge_u'                     { I64GeU }
    | 'f32.eq'                       { F32Eq }
    | 'f32.ne'                       { F32Ne }
    | 'f32.lt'                       { F32Lt }
    | 'f32.gt'                       { F32Gt }
    | 'f32.le'                       { F32Le }
    | 'f32.ge'                       { F32Ge }
    | 'f64.eq'                       { F64Eq }
    | 'f64.ne'                       { F64Ne }
    | 'f64.lt'                       { F64Lt }
    | 'f64.gt'                       { F64Gt }
    | 'f64.le'                       { F64Le }
    | 'f64.ge'                       { F64Ge }
    | 'i32.wrap/i64'                 { I32WrapI64 }
    | 'i32.trunc_s/f32'              { I32TruncSF32 }
    | 'i32.trunc_u/f32'              { I32TruncUF32 }
    | 'i32.trunc_s/f64'              { I32TruncSF64 }
    | 'i32.trunc_u/f64'              { I32TruncUF64 }
    | 'i64.extend_s/i32'             { I64ExtendSI32 }
    | 'i64.extend_u/i32'             { I64ExtendUI32 }
    | 'i64.trunc_s/f32'              { I64TruncSF32 }
    | 'i64.trunc_u/f32'              { I64TruncUF32 }
    | 'i64.trunc_s/f64'              { I64TruncSF64 }
    | 'i64.trunc_u/f64'              { I64TruncUF64 }
    | 'f32.convert_s/i32'            { F32ConvertSI32 }
    | 'f32.convert_u/i32'            { F32ConvertUI32 }
    | 'f32.convert_s/i64'            { F32ConvertSI64 }
    | 'f32.convert_u/i64'            { F32ConvertUI64 }
    | 'f32.demote/f64'               { F32DemoteF64 }
    | 'f64.convert_s/i32'            { F64ConvertSI32 }
    | 'f64.convert_u/i32'            { F64ConvertUI32 }
    | 'f64.convert_s/i64'            { F64ConvertSI64 }
    | 'f64.convert_u/i64'            { F64ConvertUI64 }
    | 'f64.promote/f32'              { F64PromoteF32 }
    | 'i32.reinterpret/f32'          { I32ReinterpretF32 }
    | 'i64.reinterpret/f64'          { I64ReinterpretF64 }
    | 'f32.reinterpret/i32'          { F32ReinterpretI32 }
    | 'f64.reinterpret/i64'          { F64ReinterpretI64 }

typedef :: { TypeDef }
    : 'type' opt(ident) functype ')' { TypeDef $2 $3 }

-- TODO: it does not properly handle call_indirect instruction use. it expects to be last expression before common ')'
typeuse :: { TypeUse }
    : '(' typeuse1 { $2 }
    | {- empty -} { AnonimousTypeUse $ FuncType [] [] }

typeuse1 :: { TypeUse }
    : 'type' typeidx ')' typedtypeuse { IndexedTypeUse $2 $4 }
    | paramsresultstypeuse { AnonimousTypeUse $1 }

typedtypeuse :: { Maybe FuncType }
    : '(' paramsresultstypeuse { Just $2 }
    | {- empty -} { Nothing }

paramsresultstypeuse :: { FuncType }
    : paramsresultstypeuse '(' paramsresulttypeuse { mergeFuncType $1 $3 }
    | paramsresulttypeuse { $1 }

paramsresulttypeuse :: { FuncType }
    : 'param' list(valtype) ')' { FuncType (map (ParamType Nothing) $2) [] }
    | 'param' ident valtype ')' { FuncType [ParamType (Just $2) $3] [] }
    | 'result' list(valtype) ')' { FuncType [] $2 }

memarg1 :: { MemArg }
    : opt(offset) opt(align) { MemArg (fromMaybe 0 $1) (fromMaybe 1 $2) }

memarg2 :: { MemArg }
    : opt(offset) opt(align) { MemArg (fromMaybe 0 $1) (fromMaybe 2 $2) }

memarg4 :: { MemArg }
    : opt(offset) opt(align) { MemArg (fromMaybe 0 $1) (fromMaybe 4 $2) }

memarg8 :: { MemArg }
    : opt(offset) opt(align) { MemArg (fromMaybe 0 $1) (fromMaybe 8 $2) }

instr :: { Instruction }
    : plaininstr { PlainInstr $1 }
    -- TODO: check if optional labels are equal if they exist
    | 'block' opt(ident) opt(resulttype) list(instr) 'end' opt(ident) { BlockInstr $2 (fromMaybe [] $3) $4 }
    -- TODO: check if optional labels are equal if they exist
    | 'loop' opt(ident) opt(resulttype) list(instr) 'end' opt(ident) { LoopInstr $2 (fromMaybe [] $3) $4 }
    -- TODO: check if optional labels are equal if they exist
    | 'if' opt(ident) opt(resulttype) list(instr)
        'else' opt(ident) list(instr)
        'end' opt(ident) { IfInstr $2 (fromMaybe [] $3) $4 $7 }

foldedinstr :: { [Instruction] }
    : '(' foldedinst1 { $2 }

foldedinst1 :: { [Instruction] }
    : plaininstr list(foldedinstr) ')' { concat $2 ++ [PlainInstr $1] }
    | 'block' opt(ident) opt(resulttype) list(instr) ')' { [BlockInstr $2 (fromMaybe [] $3) $4] }
    | 'loop' opt(ident) opt(resulttype) list(instr) ')' { [LoopInstr $2 (fromMaybe [] $3) $4] }
    | 'if' opt(ident) opt(resulttype) list(foldedinstr)
        '(' 'then' list(instr) ')'
        '(' 'else' list(instr) opt(')') ')' { concat $4 ++ [IfInstr $2 (fromMaybe [] $3) $7 $11] }

importdesc :: { ImportDesc }
    : '(' 'func' opt(ident) typeuse ')' { ImportFunc $3 $4 }
    | '(' 'table' opt(ident) tabletype ')' { ImportTable $3 $4 }
    | '(' 'memory' opt(ident) limits ')' { ImportMemory $3 $4 }
    | '(' 'global' opt(ident) globaltype ')' { ImportGlobal $3 $4 }

import :: { Import }
    : 'import' name name importdesc ')' { Import $2 $3 $4 }
    --| '(' 'func' opt(ident) '(' 'import' name name ')' typeuse ')' { Import $6 $7 $ ImportFunc $3 $9 }
    --| '(' 'global' opt(ident) '(' 'import' name name ')' globaltype ')' { Import $6 $7 $ ImportGlobal $3 $9 }
    --| '(' 'memory' opt(ident) '(' 'import' name name ')' limits ')' { Import $6 $7 $ ImportMemory $3 $9 }

localtypes :: { [LocalType] }
    : list(localtype) { concat $1 }

localtype :: { [LocalType] }
    : '(' 'local' ident valtype ')' { [LocalType (Just $3) $4] }
    | '(' 'local' list(valtype) ')' { map (LocalType Nothing) $3 }

-- FUNCTION --
function :: { [ModuleField] }
    : 'func' opt(ident) export_import_typeuse_locals_body { map (appendIdent $2) $3 }

export_import_typeuse_locals_body :: { [ModuleField] }
    : ')' { [MFFunc $ Function Nothing (AnonimousTypeUse $ FuncType [] []) [] []] }
    | '(' export_import_typeuse_locals_body1 { $2 }

export_import_typeuse_locals_body1 :: { [ModuleField] }
    : 'export' name ')' export_import_typeuse_locals_body { (MFExport $ Export $2 $ ExportFunc Nothing) : $4 }
    | import_typeuse_locals_body1 { [$1] }

import_typeuse_locals_body :: { ModuleField }
    : '(' import_typeuse_locals_body1 { $2 }
    | ')' { MFFunc $ Function Nothing (AnonimousTypeUse $ FuncType [] []) [] [] }

import_typeuse_locals_body1 :: { ModuleField }
    : 'import' name name ')' typeuse ')' { MFImport $ Import $2 $3 $ ImportFunc Nothing $5 }
    | typeuse_locals_body1 { MFFunc $ Function Nothing (t3fst $1) (t3snd $1) (t3thd $1) }

typeuse_locals_body :: { (TypeUse, [LocalType], [Instruction]) }
    : '(' typeuse_locals_body1 { $2 }
    | ')' { (AnonimousTypeUse $ FuncType [] [], [], []) }

typeuse_locals_body1 :: { (TypeUse, [LocalType], [Instruction]) }
    : 'type' typeidx ')' signature_locals_body { (IndexedTypeUse $2 (t3fst $4), t3snd $4, t3thd $4) }
    | signature_locals_body1 { (AnonimousTypeUse (fromMaybe emptyFuncType $ t3fst $1), t3snd $1, t3thd $1) }

signature_locals_body :: { (Maybe FuncType, [LocalType], [Instruction]) }
    : ')' { (Nothing, [], []) }
    | '(' signature_locals_body1 { $2 }

signature_locals_body1 :: { (Maybe FuncType, [LocalType], [Instruction]) }
    : 'param' list(valtype) ')' signature_locals_body
        { (Just $ prependFuncParams (map (ParamType Nothing) $2) $ fromMaybe emptyFuncType $ t3fst $4, t3snd $4, t3thd $4) }
    | 'param' ident valtype ')' signature_locals_body
        { (Just $ prependFuncParams [ParamType (Just $2) $3] $ fromMaybe emptyFuncType $ t3fst $5, t3snd $5, t3thd $5) }
    | 'result' list(valtype) ')' signature_locals_body
        { (Just $ prependFuncResults $2 $ fromMaybe emptyFuncType $ t3fst $4, t3snd $4, t3thd $4) }
    | locals_body1 { (Nothing, fst $1, snd $1) }

locals_body :: { ([LocalType], [Instruction]) }
    : ')' { ([], []) }
    | '(' locals_body1 { $2 }

locals_body1 :: { ([LocalType], [Instruction]) }
    : 'local' list(valtype) ')' locals_body { (map (LocalType Nothing) $2 ++ fst $4, snd $4) }
    | 'local' ident valtype ')' locals_body { (LocalType (Just $2) $3 : fst $5, snd $5) }
    | foldedinst1 ')' { ([], $1) }

-- FUNCTION END --

global :: { Global }
    : 'global' opt(ident) globaltype list(foldedinstr) ')' { Global $2 $3 (concat $4) }

memory :: { Memory }
    : 'memory' opt(ident) limits ')' { Memory $2 $3 }

table :: { Table }
    : 'table' opt(ident) tabletype ')' { Table $2 $3 }

exportdesc :: { ExportDesc }
    : '(' 'func' funcidx ')' { ExportFunc (Just $3) }
    | '(' 'table' tableidx ')' { ExportTable $3 }
    | '(' 'memory' memidx ')' { ExportMemory $3 }
    | '(' 'global' globalidx ')' { ExportGlobal $3 }

export :: { Export }
    : 'export' name exportdesc ')' { Export $2 $3 }

start :: { StartFunction }
    : 'start' funcidx ')' { StartFunction $2 }

-- TODO: Spec from 09 Jan 2018 declares 'offset' keyword as mandatory,
-- but collection of testcases omits 'offset' in this position
-- I am going to support both options for now, but maybe it has to be updated in future.
offsetexpr :: { [Instruction] }
    : '(' 'offset' foldedinstr ')' { $3 }
    | foldedinstr { $1 } 

elemsegment :: { ElemSegment }
    : 'elem' opt(tableidx) offsetexpr list(funcidx) ')' { ElemSegment (fromMaybe (Index 0) $2) $3 $4 }

datasegment :: { DataSegment }
    : 'data' opt(memidx) offsetexpr list(string) ')' { DataSegment (fromMaybe (Index 0) $2) $3 (TL.concat $4) }

modulefield1_single :: { ModuleField }
    : typedef { MFType $1 }
    | import { MFImport $1 }
    | table { MFTable $1 }
    | memory { MFMem $1 }
    | global { MFGlobal $1 }
    | export { MFExport $1 }
    | start { MFStart $1 }
    | elemsegment { MFElem $1 }
    | datasegment { MFData $1 }

modulefield1_multi :: { [ModuleField] }
    : function { $1 }

modulefield1 :: { [ModuleField] }
    : modulefield1_single { [$1] }
    | modulefield1_multi { $1 }

modulefield :: { [ModuleField] }
    : '(' modulefield1 { $2 }

modulefields :: { Module }
    : modulefields modulefield { foldl' (flip appendModuleField) $1 $2 }
    | {- empty -} { emptyModule }

mod :: { Module }
    : '(' 'module' modulefields ')' EOF { reverseModuleFields $3 }
    | modulefields EOF { reverseModuleFields $1 }

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

t3fst :: (a, b, c) -> a
t3fst (a, _, _) = a

t3snd :: (a, b, c) -> b
t3snd (_, a, _) = a

t3thd :: (a, b, c) -> c
t3thd (_, _, a) = a

appendIdent :: Maybe Ident -> ModuleField -> ModuleField
appendIdent i (MFFunc fun) = MFFunc $ fun { ident = i }
appendIdent i (MFImport (Import sm name (ImportFunc _ typeUse))) = MFImport $ Import sm name $ ImportFunc i typeUse
appendIdent i (MFExport (Export name (ExportFunc _))) = MFExport $ Export name $ ExportFunc $ Named <$> i
appendIdent _ mf = mf

prependFuncParams :: [ParamType] -> FuncType -> FuncType
prependFuncParams prep (FuncType params results) = FuncType (prep ++ params) results

prependFuncResults :: [ValueType] -> FuncType -> FuncType
prependFuncResults prep (FuncType params results) = FuncType params (prep ++ results)

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
asFloat32 = realToFrac

asFloat64 :: Double -> Double
asFloat64 = id

asOffset :: LBS.ByteString -> Maybe Natural
asOffset str = do
    num <- TL.stripPrefix "offset=" $ TLEncoding.decodeUtf8 str
    fromIntegral . fst <$> eitherToMaybe (TLRead.decimal num)

asAlign :: LBS.ByteString -> Maybe Natural
asAlign str = do
    num <- TL.stripPrefix "align=" $ TLEncoding.decodeUtf8 str
    fromIntegral . fst <$> eitherToMaybe (TLRead.decimal num)

-- TODO: check name conditions.
-- Presuming the source text is itself encoded correctly,
-- strings that do not contain any uses of hexadecimal byte escapes are always valid names.
asName :: LBS.ByteString -> Maybe TL.Text
asName = Just . TLEncoding.decodeUtf8

asString :: LBS.ByteString -> Maybe TL.Text
asString = Just . TLEncoding.decodeUtf8

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

emptyFuncType :: FuncType
emptyFuncType = FuncType [] []

data ParamType = ParamType {
        ident :: Maybe Ident,
        paramType :: ValueType
    } deriving (Show, Eq)

newtype Ident = Ident T.Text deriving (Show, Eq)

data GlobalType = Const ValueType | Mut ValueType deriving (Show, Eq)

data Limit = Limit Natural (Maybe Natural) deriving (Show, Eq)

data ElemType = AnyFunc deriving (Show, Eq)

data TableType = TableType Limit ElemType deriving (Show, Eq)

data Index = Named Ident | Index Natural deriving (Show, Eq)

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
    | I32Clz
    | I32Ctz
    | I32Popcnt
    | I32Add
    | I32Sub
    | I32Mul
    | I32DivS
    | I32DivU
    | I32RemS
    | I32RemU
    | I32And
    | I32Or
    | I32Xor
    | I32Shl
    | I32ShrS
    | I32ShrU
    | I32Rotl
    | I32Rotr
    | I64Clz
    | I64Ctz
    | I64Popcnt
    | I64Add
    | I64Sub
    | I64Mul
    | I64DivS
    | I64DivU
    | I64RemS
    | I64RemU
    | I64And
    | I64Or
    | I64Xor
    | I64Shl
    | I64ShrS
    | I64ShrU
    | I64Rotl
    | I64Rotr
    | F32Abs
    | F32Neg
    | F32Ceil
    | F32Floor
    | F32Trunc
    | F32Nearest
    | F32Sqrt
    | F32Add
    | F32Sub
    | F32Mul
    | F32Div
    | F32Min
    | F32Max
    | F32Copysign
    | F64Abs
    | F64Neg
    | F64Ceil
    | F64Floor
    | F64Trunc
    | F64Nearest
    | F64Sqrt
    | F64Add
    | F64Sub
    | F64Mul
    | F64Div
    | F64Min
    | F64Max
    | F64Copysign
    | I32Eqz
    | I32Eq
    | I32Ne
    | I32LtS
    | I32LtU
    | I32GtS
    | I32GtU
    | I32LeS
    | I32LeU
    | I32GeS
    | I32GeU
    | I64Eqz
    | I64Eq
    | I64Ne
    | I64LtS
    | I64LtU
    | I64GtS
    | I64GtU
    | I64LeS
    | I64LeU
    | I64GeS
    | I64GeU
    | F32Eq
    | F32Ne
    | F32Lt
    | F32Gt
    | F32Le
    | F32Ge
    | F64Eq
    | F64Ne
    | F64Lt
    | F64Gt
    | F64Le
    | F64Ge
    | I32WrapI64
    | I32TruncSF32
    | I32TruncUF32
    | I32TruncSF64
    | I32TruncUF64
    | I64ExtendSI32
    | I64ExtendUI32
    | I64TruncSF32
    | I64TruncUF32
    | I64TruncSF64
    | I64TruncUF64
    | F32ConvertSI32
    | F32ConvertUI32
    | F32ConvertSI64
    | F32ConvertUI64
    | F32DemoteF64
    | F64ConvertSI32
    | F64ConvertUI32
    | F64ConvertSI64
    | F64ConvertUI64
    | F64PromoteF32
    | I32ReinterpretF32
    | I64ReinterpretF64
    | F32ReinterpretI32
    | F64ReinterpretI64
    deriving (Show, Eq)

data TypeDef = TypeDef (Maybe Ident) FuncType deriving (Show, Eq)

data TypeUse =
    IndexedTypeUse TypeIndex (Maybe FuncType)
    | AnonimousTypeUse FuncType
    deriving (Show, Eq)

data MemArg = MemArg { offset :: Natural, align :: Natural } deriving (Show, Eq)

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
    deriving (Show, Eq)

data Import = Import {
        sourceModule :: TL.Text,
        name :: TL.Text,
        desc :: ImportDesc
    } deriving (Show, Eq)

data ImportDesc =
    ImportFunc (Maybe Ident) TypeUse
    | ImportTable (Maybe Ident) TableType
    | ImportMemory (Maybe Ident) Limit
    | ImportGlobal (Maybe Ident) GlobalType
    deriving (Show, Eq)

data LocalType = LocalType {
        ident :: Maybe Ident,
        localType :: ValueType
    } deriving (Show, Eq)

data Function = Function {
        ident :: Maybe Ident,
        funType :: TypeUse,
        locals :: [LocalType],
        body :: [Instruction]
    }
    deriving (Show, Eq)

data Global = Global {
        ident :: Maybe Ident,
        globalType :: GlobalType,
        initializer :: [Instruction]
    }
    deriving (Show, Eq)

data Memory = Memory (Maybe Ident) Limit deriving (Show, Eq)

data Table = Table (Maybe Ident) TableType deriving (Show, Eq)

data ExportDesc =
    ExportFunc (Maybe FuncIndex)
    | ExportTable TableIndex
    | ExportMemory MemoryIndex
    | ExportGlobal GlobalIndex
    deriving (Show, Eq)

data Export = Export {
        name :: TL.Text,
        desc :: ExportDesc
    }
    deriving (Show, Eq)

data StartFunction = StartFunction FuncIndex deriving (Show, Eq)

data ElemSegment = ElemSegment {
        tableIndex :: TableIndex,
        offset :: [Instruction],
        funcIndexes :: [FuncIndex]
    }
    deriving (Show, Eq)

data DataSegment = DataSegment {
        memIndex :: MemoryIndex,
        offset :: [Instruction],
        datastring :: TL.Text
    }
    deriving (Show, Eq)

data ModuleField =
    MFType TypeDef
    | MFImport Import
    | MFFunc Function
    | MFTable Table
    | MFMem Memory
    | MFGlobal Global
    | MFExport Export
    | MFStart StartFunction
    | MFElem ElemSegment
    | MFData DataSegment
    deriving(Show, Eq)

data Module = Module {
        types     :: [TypeDef],
        imports   :: [Import],
        functions :: [Function],
        tables    :: [Table],
        memories  :: [Memory],
        globals   :: [Global],
        exports   :: [Export],
        start     :: Maybe StartFunction,
        elems     :: [ElemSegment],
        datas     :: [DataSegment]
    }
    deriving (Show, Eq)

emptyModule :: Module
emptyModule =
    Module {
        types = [],
        imports = [],
        functions = [],
        tables = [],
        memories = [],
        globals = [],
        exports = [],
        start = Nothing,
        elems = [],
        datas = []
    }

appendModuleField :: ModuleField -> Module -> Module
appendModuleField field mod =
    case field of
        MFType typeDef -> mod { types = typeDef : types mod }
        MFImport imp -> mod { imports = imp : imports mod }
        MFFunc func -> mod { functions = func : functions mod }
        MFTable table -> mod { tables = table : tables mod }
        MFMem mem -> mod { memories = mem : memories mod }
        MFGlobal global -> mod { globals = global : globals mod }
        MFExport exp -> mod { exports = exp : exports mod }
        MFStart startFunc -> mod { start = Just startFunc }
        MFElem elem -> mod { elems = elem : elems mod }
        MFData dataSeg -> mod { datas = dataSeg : datas mod }

reverseModuleFields :: Module -> Module
reverseModuleFields mod =
    Module {
        types = reverse $ types mod,
        imports = reverse $ imports mod,
        functions = reverse $ functions mod,
        tables = reverse $ tables mod,
        memories = reverse $ memories mod,
        globals = reverse $ globals mod,
        exports = reverse $ exports mod,
        start = start mod,
        elems = reverse $ elems mod,
        datas = reverse $ datas mod
    }

happyError (Lexeme _ EOF : []) = error $ "Error occuried during parsing phase at the end of file"
happyError (Lexeme (AlexPn abs line col) tok : tokens) = error $
    "Error occuried during parsing phase. " ++
    "Line " ++ show line ++ ", " ++
    "Column " ++ show col ++ ", " ++
    "Token " ++ show tok ++ ". " ++
    "Token lookahed: " ++ show (take 3 tokens)

}