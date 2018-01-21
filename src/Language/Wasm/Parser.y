{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.Wasm.Parser (
    foldedinstr
) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLEncoding
import qualified Data.Text.Lazy.Read as TLRead

import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)

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
        )
    )

}

%name foldedinstr foldedinstr
%tokentype { Token }

%token

'('                { TOpenBracket }
')'                { TCloseBracket }
'func'                { TKeyword "func" }
'param'               { TKeyword "param" }
'result'              { TKeyword "result" }
'i32'                 { TKeyword "i32" }
'i64'                 { TKeyword "i64" }
'f32'                 { TKeyword "f32" }
'f64'                 { TKeyword "f64" }
'mut'                 { TKeyword "mut" }
'anyfunc'             { TKeyword "anyfunc" }
'type'                { TKeyword "type" }
'unreachable'         { TKeyword "unreachable" }
'nop'                 { TKeyword "nop" }
'br'                  { TKeyword "br" }
'br_if'               { TKeyword "br_if" }
'br_table'            { TKeyword "br_table" }
'return'              { TKeyword "return" }
'call'                { TKeyword "call" }
'call_indirect'       { TKeyword "call_indirect" }
'drop'                { TKeyword "drop" }
'select'              { TKeyword "select" }
'get_local'           { TKeyword "get_local" }
'set_local'           { TKeyword "set_local" }
'tee_local'           { TKeyword "tee_local" }
'get_global'          { TKeyword "get_global" }
'set_global'          { TKeyword "set_global" }
'i32.load'            { TKeyword "i32.load" }
'i64.load'            { TKeyword "i64.load" }
'f32.load'            { TKeyword "f32.load" }
'f64.load'            { TKeyword "f64.load" }
'i32.load8_s'         { TKeyword "i32.load8_s" }
'i32.load8_u'         { TKeyword "i32.load8_u" }
'i32.load16_s'        { TKeyword "i32.load16_s" }
'i32.load16_u'        { TKeyword "i32.load16_u" }
'i64.load8_s'         { TKeyword "i64.load8_s" }
'i64.load8_u'         { TKeyword "i64.load8_u" }
'i64.load16_s'        { TKeyword "i64.load16_s" }
'i64.load16_u'        { TKeyword "i64.load16_u" }
'i64.load32_s'        { TKeyword "i64.load32_s" }
'i64.load32_u'        { TKeyword "i64.load32_u" }
'i32.store'           { TKeyword "i32.store" }
'i64.store'           { TKeyword "i64.store" }
'f32.store'           { TKeyword "f32.store" }
'f64.store'           { TKeyword "f64.store" }
'i32.store8'          { TKeyword "i32.store8" }
'i32.store16'         { TKeyword "i32.store16" }
'i64.store8'          { TKeyword "i64.store" }
'i64.store16'         { TKeyword "i64.store" }
'i64.store32'         { TKeyword "i64.store" }
'current_memory'      { TKeyword "current_memory" }
'grow_memory'         { TKeyword "grow_memory" }
'i32.const'           { TKeyword "i32.const" }
'i64.const'           { TKeyword "i64.const" }
'f32.const'           { TKeyword "f32.const" }
'f64.const'           { TKeyword "f64.const" }
'i32.clz'             { TKeyword "i32.clz" }
'i32.ctz'             { TKeyword "i32.ctz" }
'i32.popcnt'          { TKeyword "i32.popcnt" }
'i32.add'             { TKeyword "i32.add" }
'i32.sub'             { TKeyword "i32.sub" }
'i32.mul'             { TKeyword "i32.mul" }
'i32.div_s'           { TKeyword "i32.div_s" }
'i32.div_u'           { TKeyword "i32.div_u" }
'i32.rem_s'           { TKeyword "i32.rem_s" }
'i32.rem_u'           { TKeyword "i32.rem_u" }
'i32.and'             { TKeyword "i32.and" }
'i32.or'              { TKeyword "i32.or" }
'i32.xor'             { TKeyword "i32.xor" }
'i32.shl'             { TKeyword "i32.shl" }
'i32.shr_s'           { TKeyword "i32.shr_s" }
'i32.shr_u'           { TKeyword "i32.shr_u" }
'i32.rotl'            { TKeyword "i32.rotl" }
'i32.rotr'            { TKeyword "i32.rotr" }
'i64.clz'             { TKeyword "i64.clz" }
'i64.ctz'             { TKeyword "i64.ctz" }
'i64.popcnt'          { TKeyword "i64.popcnt" }
'i64.add'             { TKeyword "i64.add" }
'i64.sub'             { TKeyword "i64.sub" }
'i64.mul'             { TKeyword "i64.mul" }
'i64.div_s'           { TKeyword "i64.div_s" }
'i64.div_u'           { TKeyword "i64.div_u" }
'i64.rem_s'           { TKeyword "i64.rem_s" }
'i64.rem_u'           { TKeyword "i64.rem_u" }
'i64.and'             { TKeyword "i64.and" }
'i64.or'              { TKeyword "i64.or" }
'i64.xor'             { TKeyword "i64.xor" }
'i64.shl'             { TKeyword "i64.shl" }
'i64.shr_s'           { TKeyword "i64.shr_s" }
'i64.shr_u'           { TKeyword "i64.shr_u" }
'i64.rotl'            { TKeyword "i64.rotl" }
'i64.rotr'            { TKeyword "i64.rotr" }
'f32.abs'             { TKeyword "f32.abs" }
'f32.neg'             { TKeyword "f32.neg" }
'f32.ceil'            { TKeyword "f32.ceil" }
'f32.floor'           { TKeyword "f32.floor" }
'f32.trunc'           { TKeyword "f32.trunc" }
'f32.nearest'         { TKeyword "f32.nearest" }
'f32.sqrt'            { TKeyword "f32.sqrt" }
'f32.add'             { TKeyword "f32.add" }
'f32.sub'             { TKeyword "f32.sub" }
'f32.mul'             { TKeyword "f32.mul" }
'f32.div'             { TKeyword "f32.div" }
'f32.min'             { TKeyword "f32.min" }
'f32.max'             { TKeyword "f32.max" }
'f32.copysign'        { TKeyword "f32.copysign" }
'f64.abs'             { TKeyword "f64.abs" }
'f64.neg'             { TKeyword "f64.neg" }
'f64.ceil'            { TKeyword "f64.ceil" }
'f64.floor'           { TKeyword "f64.floor" }
'f64.trunc'           { TKeyword "f64.trunc" }
'f64.nearest'         { TKeyword "f64.nearest" }
'f64.sqrt'            { TKeyword "f64.sqrt" }
'f64.add'             { TKeyword "f64.add" }
'f64.sub'             { TKeyword "f64.sub" }
'f64.mul'             { TKeyword "f64.mul" }
'f64.div'             { TKeyword "f64.div" }
'f64.min'             { TKeyword "f64.min" }
'f64.max'             { TKeyword "f64.max" }
'f64.copysign'        { TKeyword "f64.copysign" }
'i32.eqz'             { TKeyword "i32.eqz" }
'i32.eq'              { TKeyword "i32.eq" }
'i32.ne'              { TKeyword "i32.ne" }
'i32.lt_s'            { TKeyword "i32.lt_s" }
'i32.lt_u'            { TKeyword "i32.lt_u" }
'i32.gt_s'            { TKeyword "i32.gt_s" }
'i32.gt_u'            { TKeyword "i32.gt_u" }
'i32.le_s'            { TKeyword "i32.le_s" }
'i32.le_u'            { TKeyword "i32.le_u" }
'i32.ge_s'            { TKeyword "i32.ge_s" }
'i32.ge_u'            { TKeyword "i32.ge_u" }
'i64.eqz'             { TKeyword "i64.eqz" }
'i64.eq'              { TKeyword "i64.eq" }
'i64.ne'              { TKeyword "i64.ne" }
'i64.lt_s'            { TKeyword "i64.lt_s" }
'i64.lt_u'            { TKeyword "i64.lt_u" }
'i64.gt_s'            { TKeyword "i64.gt_s" }
'i64.gt_u'            { TKeyword "i64.gt_u" }
'i64.le_s'            { TKeyword "i64.le_s" }
'i64.le_u'            { TKeyword "i64.le_u" }
'i64.ge_s'            { TKeyword "i64.ge_s" }
'i64.ge_u'            { TKeyword "i64.ge_u" }
'f32.eq'              { TKeyword "f32.eq" }
'f32.ne'              { TKeyword "f32.ne" }
'f32.lt'              { TKeyword "f32.lt" }
'f32.gt'              { TKeyword "f32.gt" }
'f32.le'              { TKeyword "f32.le" }
'f32.ge'              { TKeyword "f32.ge" }
'f64.eq'              { TKeyword "f64.eq" }
'f64.ne'              { TKeyword "f64.ne" }
'f64.lt'              { TKeyword "f64.lt" }
'f64.gt'              { TKeyword "f64.gt" }
'f64.le'              { TKeyword "f64.le" }
'f64.ge'              { TKeyword "f64.ge" }
'i32.wrap/i64'        { TKeyword "i32.wrap/i64" }
'i32.trunc_s/f32'     { TKeyword "i32.trunc_s/f32" }
'i32.trunc_u/f32'     { TKeyword "i32.trunc_u/f32" }
'i32.trunc_s/f64'     { TKeyword "i32.trunc_s/f64" }
'i32.trunc_u/f64'     { TKeyword "i32.trunc_u/f64" }
'i64.extend_s/i32'    { TKeyword "i64.extend_s/i32" }
'i64.extend_u/i32'    { TKeyword "i64.extend_u/i32" }
'i64.trunc_s/f32'     { TKeyword "i64.trunc_s/f32" }
'i64.trunc_u/f32'     { TKeyword "i64.trunc_u/f32" }
'i64.trunc_s/f64'     { TKeyword "i64.trunc_s/f64" }
'i64.trunc_u/f64'     { TKeyword "i64.trunc_u/f64" }
'f32.convert_s/i32'   { TKeyword "f32.convert_s/i32" }
'f32.convert_u/i32'   { TKeyword "f32.convert_u/i32" }
'f32.convert_s/i64'   { TKeyword "f32.convert_s/i64" }
'f32.convert_u/i64'   { TKeyword "f32.convert_u/i64" }
'f32.demote/f64'      { TKeyword "f32.demote/f64" }
'f64.convert_s/i32'   { TKeyword "f64.convert_s/i32" }
'f64.convert_u/i32'   { TKeyword "f64.convert_u/i32" }
'f64.convert_s/i64'   { TKeyword "f64.convert_s/i64" }
'f64.convert_u/i64'   { TKeyword "f64.convert_u/i64" }
'f64.promote/f32'     { TKeyword "f64.promote/f32" }
'i32.reinterpret/f32' { TKeyword "i32.reinterpret/f32" }
'i64.reinterpret/f64' { TKeyword "i64.reinterpret/f64" }
'f32.reinterpret/i32' { TKeyword "f32.reinterpret/i32" }
'f64.reinterpret/i64' { TKeyword "f64.reinterpret/i64" }
'block'               { TKeyword "block" }
'loop'                { TKeyword "loop" }
'if'                  { TKeyword "if" }
'else'                { TKeyword "else" }
'end'                 { TKeyword "end" }
'then'                { TKeyword "then" }
'table'               { TKeyword "table" }
'memory'              { TKeyword "memory" }
'global'              { TKeyword "global" }
'import'              { TKeyword "import" }
'local'               { TKeyword "local" }
id                    { TId $$ }
u32                   { TIntLit (asUInt32 -> Just $$) }
i32                   { TIntLit (asInt32 -> Just $$) }
i64                   { TIntLit (asInt64 -> Just $$) }
f32                   { TFloatLit (asFloat32 -> $$) }
f64                   { TFloatLit (asFloat64 -> $$) }
offset                { TKeyword (asOffset -> Just $$) }
align                 { TKeyword (asAlign -> Just $$) }
name                  { TStringLit (asName -> Just $$) }

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
    | 'i32.const' i32                { I32Const $2 }
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
    : '(' plaininstr list(foldedinstr) ')' { concat $3 ++ [PlainInstr $2] }
    | '(' 'block' opt(ident) opt(resulttype) list(instr) ')' { [BlockInstr $3 (fromMaybe [] $4) $5] }
    | '(' 'loop' opt(ident) opt(resulttype) list(instr) ')' { [LoopInstr $3 (fromMaybe [] $4) $5] }
    | '(' 'if' opt(ident) opt(resulttype) list(foldedinstr)
        '(' 'then' list(instr) ')'
        '(' 'else' list(instr) opt(')') ')' { concat $5 ++ [IfInstr $3 (fromMaybe [] $4) $8 $12] }

importdesc :: { ImportDesc }
    : '(' 'func' opt(ident) typeuse ')' { ImportFunc $3 $4 }
    | '(' 'table' opt(ident) tabletype ')' { ImportTable $3 $4 }
    | '(' 'memory' opt(ident) limits ')' { ImportMemory $3 $4 }
    | '(' 'global' opt(ident) globaltype ')' { ImportGlobal $3 $4 }

import :: { Import }
    : '(' 'import' name name importdesc ')' { Import $3 $4 $5 }

localtypes :: { [LocalType] }
    : list(localtype) { concat $1 }

localtype :: { [LocalType] }
    : '(' 'local' ident valtype ')' { [LocalType (Just $3) $4] }
    | '(' 'local' list(valtype) ')' { map (LocalType Nothing) $3 }

function :: { Function }
    : '(' 'func' opt(ident) typeuse localtypes list(foldedinstr) ')' { Function $3 $4 $5 (concat $6) }

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

data Index = Named Ident | Index Natural deriving (Show, Eq)

type LabelIndex = Index
type FuncIndex = Index
type TypeIndex = Index
type LocalIndex = Index
type GlobalIndex = Index

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

data Import = Import { sourceModule :: TL.Text, name :: TL.Text, desc :: ImportDesc } deriving (Show, Eq)

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

happyError tokens = error $ "Error occuried: " ++ show tokens 

}