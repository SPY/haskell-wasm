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
    Meta(..),
    ValuePattern(..)
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
        ValueType(..),
        SimdShape(..)
    )

import qualified Language.Wasm.Structure as S

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLEncoding
import qualified Data.Text.Lazy.Read as TLRead

import qualified Data.Primitive.ByteArray as ByteArray
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSChar8
import Data.Maybe (fromMaybe, fromJust, isNothing, catMaybes)
import Data.List (foldl', findIndex, find, nub)
import Control.Monad (guard, foldM)
import Control.Monad.Except (throwError)

import Numeric.Natural (Natural)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Bits ((.|.))
import Numeric.IEEE (infinity, nan, maxFinite)
import Language.Wasm.FloatUtils (doubleToFloat, floatToWord, doubleToWord)
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
        AlexPosn(..),
        FloatRep(..),
        NaN(..),
        asFloat,
        asDouble,
        doubleFromInteger
    )

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
'v128'                { Lexeme _ (TKeyword "v128") }
'mut'                 { Lexeme _ (TKeyword "mut") }
'funcref'             { Lexeme _ (TKeyword "funcref") }
'externref'           { Lexeme _ (TKeyword "externref") }
'extern'              { Lexeme _ (TKeyword "extern") }
'type'                { Lexeme _ (TKeyword "type") }
'unreachable'         { Lexeme _ (TKeyword "unreachable") }
'nop'                 { Lexeme _ (TKeyword "nop") }
'br'                  { Lexeme _ (TKeyword "br") }
'br_if'               { Lexeme _ (TKeyword "br_if") }
'br_table'            { Lexeme _ (TKeyword "br_table") }
'return'              { Lexeme _ (TKeyword "return") }
'call'                { Lexeme _ (TKeyword "call") }
'call_indirect'       { Lexeme _ (TKeyword "call_indirect") }
'ref.null'            { Lexeme _ (TKeyword "ref.null") }
'ref.is_null'         { Lexeme _ (TKeyword "ref.is_null") }
'ref.func'            { Lexeme _ (TKeyword "ref.func") }
'ref.extern'          { Lexeme _ (TKeyword "ref.extern") }
'drop'                { Lexeme _ (TKeyword "drop") }
'select'              { Lexeme _ (TKeyword "select") }
'get_local'           { Lexeme _ (TKeyword "local.get") }
'set_local'           { Lexeme _ (TKeyword "local.set") }
'tee_local'           { Lexeme _ (TKeyword "local.tee") }
'get_global'          { Lexeme _ (TKeyword "global.get") }
'set_global'          { Lexeme _ (TKeyword "global.set") }
'i32.load'            { Lexeme _ (TKeyword "i32.load") }
'i64.load'            { Lexeme _ (TKeyword "i64.load") }
'f32.load'            { Lexeme _ (TKeyword "f32.load") }
'f64.load'            { Lexeme _ (TKeyword "f64.load") }
'v128.load'           { Lexeme _ (TKeyword "v128.load") }
'v128.load8_lane'     { Lexeme _ (TKeyword "v128.load8_lane") }
'v128.load16_lane'    { Lexeme _ (TKeyword "v128.load16_lane") }
'v128.load32_lane'    { Lexeme _ (TKeyword "v128.load32_lane") }
'v128.load64_lane'    { Lexeme _ (TKeyword "v128.load64_lane") }
'v128.load8_splat'    { Lexeme _ (TKeyword "v128.load8_splat") }
'v128.load16_splat'   { Lexeme _ (TKeyword "v128.load16_splat") }
'v128.load32_splat'   { Lexeme _ (TKeyword "v128.load32_splat") }
'v128.load64_splat'   { Lexeme _ (TKeyword "v128.load64_splat") }
'v128.load32_zero'    { Lexeme _ (TKeyword "v128.load32_zero") }
'v128.load64_zero'    { Lexeme _ (TKeyword "v128.load64_zero") }
'v128.load8x8_s'      { Lexeme _ (TKeyword "v128.load8x8_s") }
'v128.load8x8_u'      { Lexeme _ (TKeyword "v128.load8x8_u") }
'v128.load16x4_s'     { Lexeme _ (TKeyword "v128.load16x4_s") }
'v128.load16x4_u'     { Lexeme _ (TKeyword "v128.load16x4_u") }
'v128.load32x2_s'     { Lexeme _ (TKeyword "v128.load32x2_s") }
'v128.load32x2_u'     { Lexeme _ (TKeyword "v128.load32x2_u") }
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
'v128.store'          { Lexeme _ (TKeyword "v128.store") }
'v128.store8_lane'    { Lexeme _ (TKeyword "v128.store8_lane") }
'v128.store16_lane'   { Lexeme _ (TKeyword "v128.store16_lane") }
'v128.store32_lane'   { Lexeme _ (TKeyword "v128.store32_lane") }
'v128.store64_lane'   { Lexeme _ (TKeyword "v128.store64_lane") }
'i32.store8'          { Lexeme _ (TKeyword "i32.store8") }
'i32.store16'         { Lexeme _ (TKeyword "i32.store16") }
'i64.store8'          { Lexeme _ (TKeyword "i64.store8") }
'i64.store16'         { Lexeme _ (TKeyword "i64.store16") }
'i64.store32'         { Lexeme _ (TKeyword "i64.store32") }
'memory.size'         { Lexeme _ (TKeyword "memory.size") }
'memory.grow'         { Lexeme _ (TKeyword "memory.grow") }
'memory.fill'         { Lexeme _ (TKeyword "memory.fill") }
'memory.copy'         { Lexeme _ (TKeyword "memory.copy") }
'memory.init'         { Lexeme _ (TKeyword "memory.init") }
'data.drop'           { Lexeme _ (TKeyword "data.drop") }
'table.init'          { Lexeme _ (TKeyword "table.init") }
'table.copy'          { Lexeme _ (TKeyword "table.copy") }
'table.fill'          { Lexeme _ (TKeyword "table.fill") }
'table.size'          { Lexeme _ (TKeyword "table.size") }
'table.grow'          { Lexeme _ (TKeyword "table.grow") }
'table.get'           { Lexeme _ (TKeyword "table.get") }
'table.set'           { Lexeme _ (TKeyword "table.set") }
'elem.drop'           { Lexeme _ (TKeyword "elem.drop") }
'i32.const'           { Lexeme _ (TKeyword "i32.const") }
'i64.const'           { Lexeme _ (TKeyword "i64.const") }
'f32.const'           { Lexeme _ (TKeyword "f32.const") }
'f64.const'           { Lexeme _ (TKeyword "f64.const") }
'v128.const'          { Lexeme _ (TKeyword "v128.const") }
'i32.clz'             { Lexeme _ (TKeyword "i32.clz") }
'i32.ctz'             { Lexeme _ (TKeyword "i32.ctz") }
'i32.popcnt'          { Lexeme _ (TKeyword "i32.popcnt") }
'i32.extend8_s'       { Lexeme _ (TKeyword "i32.extend8_s") }
'i32.extend16_s'      { Lexeme _ (TKeyword "i32.extend16_s") }
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
'i64.extend8_s'       { Lexeme _ (TKeyword "i64.extend8_s") }
'i64.extend16_s'      { Lexeme _ (TKeyword "i64.extend16_s") }
'i64.extend32_s'      { Lexeme _ (TKeyword "i64.extend32_s") }
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
'i32.wrap_i64'        { Lexeme _ (TKeyword "i32.wrap_i64") }
'i32.trunc_f32_s'     { Lexeme _ (TKeyword "i32.trunc_f32_s") }
'i32.trunc_f32_u'     { Lexeme _ (TKeyword "i32.trunc_f32_u") }
'i32.trunc_f64_s'     { Lexeme _ (TKeyword "i32.trunc_f64_s") }
'i32.trunc_f64_u'     { Lexeme _ (TKeyword "i32.trunc_f64_u") }
'i32.trunc_sat_f32_s' { Lexeme _ (TKeyword "i32.trunc_sat_f32_s") }
'i32.trunc_sat_f32_u' { Lexeme _ (TKeyword "i32.trunc_sat_f32_u") }
'i32.trunc_sat_f64_s' { Lexeme _ (TKeyword "i32.trunc_sat_f64_s") }
'i32.trunc_sat_f64_u' { Lexeme _ (TKeyword "i32.trunc_sat_f64_u") }
'i64.extend_i32_s'    { Lexeme _ (TKeyword "i64.extend_i32_s") }
'i64.extend_i32_u'    { Lexeme _ (TKeyword "i64.extend_i32_u") }
'i64.trunc_f32_s'     { Lexeme _ (TKeyword "i64.trunc_f32_s") }
'i64.trunc_f32_u'     { Lexeme _ (TKeyword "i64.trunc_f32_u") }
'i64.trunc_f64_s'     { Lexeme _ (TKeyword "i64.trunc_f64_s") }
'i64.trunc_f64_u'     { Lexeme _ (TKeyword "i64.trunc_f64_u") }
'i64.trunc_sat_f32_s' { Lexeme _ (TKeyword "i64.trunc_sat_f32_s") }
'i64.trunc_sat_f32_u' { Lexeme _ (TKeyword "i64.trunc_sat_f32_u") }
'i64.trunc_sat_f64_s' { Lexeme _ (TKeyword "i64.trunc_sat_f64_s") }
'i64.trunc_sat_f64_u' { Lexeme _ (TKeyword "i64.trunc_sat_f64_u") }
'f32.convert_i32_s'   { Lexeme _ (TKeyword "f32.convert_i32_s") }
'f32.convert_i32_u'   { Lexeme _ (TKeyword "f32.convert_i32_u") }
'f32.convert_i64_s'   { Lexeme _ (TKeyword "f32.convert_i64_s") }
'f32.convert_i64_u'   { Lexeme _ (TKeyword "f32.convert_i64_u") }
'f32x4.convert_i32x4_s' { Lexeme _ (TKeyword "f32x4.convert_i32x4_s") }
'f32x4.convert_i32x4_u' { Lexeme _ (TKeyword "f32x4.convert_i32x4_u") }
'f64x2.convert_low_i32x4_s' { Lexeme _ (TKeyword "f64x2.convert_low_i32x4_s") }
'f64x2.convert_low_i32x4_u' { Lexeme _ (TKeyword "f64x2.convert_low_i32x4_u") }
'i8x16.narrow_i16x8_s'{ Lexeme _ (TKeyword "i8x16.narrow_i16x8_s") }
'i8x16.narrow_i16x8_u'{ Lexeme _ (TKeyword "i8x16.narrow_i16x8_u") }
'i16x8.narrow_i32x4_s'{ Lexeme _ (TKeyword "i16x8.narrow_i32x4_s") }
'i16x8.narrow_i32x4_u'{ Lexeme _ (TKeyword "i16x8.narrow_i32x4_u") }
'f64x2.promote_low_f32x4'{ Lexeme _ (TKeyword "f64x2.promote_low_f32x4") }
'f32x4.demote_f64x2_zero'{ Lexeme _ (TKeyword "f32x4.demote_f64x2_zero") }
'f32.demote_f64'      { Lexeme _ (TKeyword "f32.demote_f64") }
'f64.convert_i32_s'   { Lexeme _ (TKeyword "f64.convert_i32_s") }
'f64.convert_i32_u'   { Lexeme _ (TKeyword "f64.convert_i32_u") }
'f64.convert_i64_s'   { Lexeme _ (TKeyword "f64.convert_i64_s") }
'f64.convert_i64_u'   { Lexeme _ (TKeyword "f64.convert_i64_u") }
'f64.promote_f32'     { Lexeme _ (TKeyword "f64.promote_f32") }
'i32.reinterpret_f32' { Lexeme _ (TKeyword "i32.reinterpret_f32") }
'i64.reinterpret_f64' { Lexeme _ (TKeyword "i64.reinterpret_f64") }
'f32.reinterpret_i32' { Lexeme _ (TKeyword "f32.reinterpret_i32") }
'f64.reinterpret_i64' { Lexeme _ (TKeyword "f64.reinterpret_i64") }
'i8x16'               { Lexeme _ (TKeyword "i8x16") }
'i16x8'               { Lexeme _ (TKeyword "i16x8") }
'i32x4'               { Lexeme _ (TKeyword "i32x4") }
'i64x2'               { Lexeme _ (TKeyword "i64x2") }
'f32x4'               { Lexeme _ (TKeyword "f32x4") }
'f64x2'               { Lexeme _ (TKeyword "f64x2") }
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
'item'                { Lexeme _ (TKeyword "item") }
'declare'             { Lexeme _ (TKeyword "declare") }
'data'                { Lexeme _ (TKeyword "data") }
'offset'              { Lexeme _ (TKeyword "offset") }
'start'               { Lexeme _ (TKeyword "start") }
'module'              { Lexeme _ (TKeyword "module") }
-- simd
'i8x16.shuffle'       { Lexeme _ (TKeyword "i8x16.shuffle") }
'i8x16.swizzle'       { Lexeme _ (TKeyword "i8x16.swizzle") }
'i8x16.splat'         { Lexeme _ (TKeyword "i8x16.splat") }
'i16x8.splat'         { Lexeme _ (TKeyword "i16x8.splat") }
'i32x4.splat'         { Lexeme _ (TKeyword "i32x4.splat") }
'i64x2.splat'         { Lexeme _ (TKeyword "i64x2.splat") }
'f32x4.splat'         { Lexeme _ (TKeyword "f32x4.splat") }
'f64x2.splat'         { Lexeme _ (TKeyword "f64x2.splat") }
'i8x16.extract_lane_u'{ Lexeme _ (TKeyword "i8x16.extract_lane_u") }
'i16x8.extract_lane_u'{ Lexeme _ (TKeyword "i16x8.extract_lane_u") }
'i8x16.extract_lane_s'{ Lexeme _ (TKeyword "i8x16.extract_lane_s") }
'i16x8.extract_lane_s'{ Lexeme _ (TKeyword "i16x8.extract_lane_s") }
'i32x4.extract_lane'  { Lexeme _ (TKeyword "i32x4.extract_lane") }
'i64x2.extract_lane'  { Lexeme _ (TKeyword "i64x2.extract_lane") }
'f32x4.extract_lane'  { Lexeme _ (TKeyword "f32x4.extract_lane") }
'f64x2.extract_lane'  { Lexeme _ (TKeyword "f64x2.extract_lane") }
'i8x16.replace_lane'  { Lexeme _ (TKeyword "i8x16.replace_lane") }
'i16x8.replace_lane'  { Lexeme _ (TKeyword "i16x8.replace_lane") }
'i32x4.replace_lane'  { Lexeme _ (TKeyword "i32x4.replace_lane") }
'i64x2.replace_lane'  { Lexeme _ (TKeyword "i64x2.replace_lane") }
'f32x4.replace_lane'  { Lexeme _ (TKeyword "f32x4.replace_lane") }
'f64x2.replace_lane'  { Lexeme _ (TKeyword "f64x2.replace_lane") }
'i8x16.all_true'      { Lexeme _ (TKeyword "i8x16.all_true") }
'i16x8.all_true'      { Lexeme _ (TKeyword "i16x8.all_true") }
'i32x4.all_true'      { Lexeme _ (TKeyword "i32x4.all_true") }
'i64x2.all_true'      { Lexeme _ (TKeyword "i64x2.all_true") }
'i8x16.popcnt'        { Lexeme _ (TKeyword "i8x16.popcnt") }
'v128.not'            { Lexeme _ (TKeyword "v128.not") }
'v128.and'            { Lexeme _ (TKeyword "v128.and") }
'v128.andnot'         { Lexeme _ (TKeyword "v128.andnot") }
'v128.or'             { Lexeme _ (TKeyword "v128.or") }
'v128.xor'            { Lexeme _ (TKeyword "v128.xor") }
'v128.any_true'       { Lexeme _ (TKeyword "v128.any_true") }
'v128.bitselect'      { Lexeme _ (TKeyword "v128.bitselect") }
'i8x16.add'           { Lexeme _ (TKeyword "i8x16.add") }
'i16x8.add'           { Lexeme _ (TKeyword "i16x8.add") }
'i32x4.add'           { Lexeme _ (TKeyword "i32x4.add") }
'i64x2.add'           { Lexeme _ (TKeyword "i64x2.add") }
'i8x16.sub'           { Lexeme _ (TKeyword "i8x16.sub") }
'i16x8.sub'           { Lexeme _ (TKeyword "i16x8.sub") }
'i32x4.sub'           { Lexeme _ (TKeyword "i32x4.sub") }
'i64x2.sub'           { Lexeme _ (TKeyword "i64x2.sub") }
'i8x16.add_sat_s'     { Lexeme _ (TKeyword "i8x16.add_sat_s") }
'i16x8.add_sat_s'     { Lexeme _ (TKeyword "i16x8.add_sat_s") }
'i8x16.sub_sat_s'     { Lexeme _ (TKeyword "i8x16.sub_sat_s") }
'i16x8.sub_sat_s'     { Lexeme _ (TKeyword "i16x8.sub_sat_s") }
'i8x16.add_sat_u'     { Lexeme _ (TKeyword "i8x16.add_sat_u") }
'i16x8.add_sat_u'     { Lexeme _ (TKeyword "i16x8.add_sat_u") }
'i8x16.sub_sat_u'     { Lexeme _ (TKeyword "i8x16.sub_sat_u") }
'i16x8.sub_sat_u'     { Lexeme _ (TKeyword "i16x8.sub_sat_u") }
'i8x16.avgr_u'        { Lexeme _ (TKeyword "i8x16.avgr_u") }
'i16x8.avgr_u'        { Lexeme _ (TKeyword "i16x8.avgr_u") }
'i16x8.extadd_pairwise_i8x16_s' { Lexeme _ (TKeyword "i16x8.extadd_pairwise_i8x16_s") }
'i32x4.extadd_pairwise_i16x8_s' { Lexeme _ (TKeyword "i32x4.extadd_pairwise_i16x8_s") }
'i16x8.extadd_pairwise_i8x16_u' { Lexeme _ (TKeyword "i16x8.extadd_pairwise_i8x16_u") }
'i32x4.extadd_pairwise_i16x8_u' { Lexeme _ (TKeyword "i32x4.extadd_pairwise_i16x8_u") }
'i16x8.extmul_low_i8x16_s' { Lexeme _ (TKeyword "i16x8.extmul_low_i8x16_s") }
'i32x4.extmul_low_i16x8_s' { Lexeme _ (TKeyword "i32x4.extmul_low_i16x8_s") }
'i64x2.extmul_low_i32x4_s' { Lexeme _ (TKeyword "i64x2.extmul_low_i32x4_s") }
'i16x8.extmul_low_i8x16_u' { Lexeme _ (TKeyword "i16x8.extmul_low_i8x16_u") }
'i32x4.extmul_low_i16x8_u' { Lexeme _ (TKeyword "i32x4.extmul_low_i16x8_u") }
'i64x2.extmul_low_i32x4_u' { Lexeme _ (TKeyword "i64x2.extmul_low_i32x4_u") }
'i16x8.extmul_high_i8x16_s' { Lexeme _ (TKeyword "i16x8.extmul_high_i8x16_s") }
'i32x4.extmul_high_i16x8_s' { Lexeme _ (TKeyword "i32x4.extmul_high_i16x8_s") }
'i64x2.extmul_high_i32x4_s' { Lexeme _ (TKeyword "i64x2.extmul_high_i32x4_s") }
'i16x8.extmul_high_i8x16_u' { Lexeme _ (TKeyword "i16x8.extmul_high_i8x16_u") }
'i32x4.extmul_high_i16x8_u' { Lexeme _ (TKeyword "i32x4.extmul_high_i16x8_u") }
'i64x2.extmul_high_i32x4_u' { Lexeme _ (TKeyword "i64x2.extmul_high_i32x4_u") }
'i8x16.min_s'         { Lexeme _ (TKeyword "i8x16.min_s") }
'i16x8.min_s'         { Lexeme _ (TKeyword "i16x8.min_s") }
'i32x4.min_s'         { Lexeme _ (TKeyword "i32x4.min_s") }
'i8x16.min_u'         { Lexeme _ (TKeyword "i8x16.min_u") }
'i16x8.min_u'         { Lexeme _ (TKeyword "i16x8.min_u") }
'i32x4.min_u'         { Lexeme _ (TKeyword "i32x4.min_u") }
'i8x16.max_s'         { Lexeme _ (TKeyword "i8x16.max_s") }
'i16x8.max_s'         { Lexeme _ (TKeyword "i16x8.max_s") }
'i32x4.max_s'         { Lexeme _ (TKeyword "i32x4.max_s") }
'i8x16.max_u'         { Lexeme _ (TKeyword "i8x16.max_u") }
'i16x8.max_u'         { Lexeme _ (TKeyword "i16x8.max_u") }
'i32x4.max_u'         { Lexeme _ (TKeyword "i32x4.max_u") }
'i16x8.mul'           { Lexeme _ (TKeyword "i16x8.mul") }
'i32x4.mul'           { Lexeme _ (TKeyword "i32x4.mul") }
'i64x2.mul'           { Lexeme _ (TKeyword "i64x2.mul") }
'i8x16.abs'           { Lexeme _ (TKeyword "i8x16.abs") }
'i16x8.abs'           { Lexeme _ (TKeyword "i16x8.abs") }
'i32x4.abs'           { Lexeme _ (TKeyword "i32x4.abs") }
'i64x2.abs'           { Lexeme _ (TKeyword "i64x2.abs") }
'i8x16.neg'           { Lexeme _ (TKeyword "i8x16.neg") }
'i16x8.neg'           { Lexeme _ (TKeyword "i16x8.neg") }
'i32x4.neg'           { Lexeme _ (TKeyword "i32x4.neg") }
'i64x2.neg'           { Lexeme _ (TKeyword "i64x2.neg") }
'i8x16.shl'           { Lexeme _ (TKeyword "i8x16.shl") }
'i16x8.shl'           { Lexeme _ (TKeyword "i16x8.shl") }
'i32x4.shl'           { Lexeme _ (TKeyword "i32x4.shl") }
'i64x2.shl'           { Lexeme _ (TKeyword "i64x2.shl") }
'i8x16.shr_u'         { Lexeme _ (TKeyword "i8x16.shr_u") }
'i16x8.shr_u'         { Lexeme _ (TKeyword "i16x8.shr_u") }
'i32x4.shr_u'         { Lexeme _ (TKeyword "i32x4.shr_u") }
'i64x2.shr_u'         { Lexeme _ (TKeyword "i64x2.shr_u") }
'i8x16.shr_s'         { Lexeme _ (TKeyword "i8x16.shr_s") }
'i16x8.shr_s'         { Lexeme _ (TKeyword "i16x8.shr_s") }
'i32x4.shr_s'         { Lexeme _ (TKeyword "i32x4.shr_s") }
'i64x2.shr_s'         { Lexeme _ (TKeyword "i64x2.shr_s") }
'i8x16.bitmask'       { Lexeme _ (TKeyword "i8x16.bitmask") }
'i16x8.bitmask'       { Lexeme _ (TKeyword "i16x8.bitmask") }
'i32x4.bitmask'       { Lexeme _ (TKeyword "i32x4.bitmask") }
'i64x2.bitmask'       { Lexeme _ (TKeyword "i64x2.bitmask") }
'i8x16.eq'            { Lexeme _ (TKeyword "i8x16.eq") }
'i16x8.eq'            { Lexeme _ (TKeyword "i16x8.eq") }
'i32x4.eq'            { Lexeme _ (TKeyword "i32x4.eq") }
'i64x2.eq'            { Lexeme _ (TKeyword "i64x2.eq") }
'i8x16.ne'            { Lexeme _ (TKeyword "i8x16.ne") }
'i16x8.ne'            { Lexeme _ (TKeyword "i16x8.ne") }
'i32x4.ne'            { Lexeme _ (TKeyword "i32x4.ne") }
'i64x2.ne'            { Lexeme _ (TKeyword "i64x2.ne") }
'i8x16.lt_s'          { Lexeme _ (TKeyword "i8x16.lt_s") }
'i16x8.lt_s'          { Lexeme _ (TKeyword "i16x8.lt_s") }
'i32x4.lt_s'          { Lexeme _ (TKeyword "i32x4.lt_s") }
'i64x2.lt_s'          { Lexeme _ (TKeyword "i64x2.lt_s") }
'i8x16.lt_u'          { Lexeme _ (TKeyword "i8x16.lt_u") }
'i16x8.lt_u'          { Lexeme _ (TKeyword "i16x8.lt_u") }
'i32x4.lt_u'          { Lexeme _ (TKeyword "i32x4.lt_u") }
'i64x2.lt_u'          { Lexeme _ (TKeyword "i64x2.lt_u") }
'i8x16.le_s'          { Lexeme _ (TKeyword "i8x16.le_s") }
'i16x8.le_s'          { Lexeme _ (TKeyword "i16x8.le_s") }
'i32x4.le_s'          { Lexeme _ (TKeyword "i32x4.le_s") }
'i64x2.le_s'          { Lexeme _ (TKeyword "i64x2.le_s") }
'i8x16.le_u'          { Lexeme _ (TKeyword "i8x16.le_u") }
'i16x8.le_u'          { Lexeme _ (TKeyword "i16x8.le_u") }
'i32x4.le_u'          { Lexeme _ (TKeyword "i32x4.le_u") }
'i64x2.le_u'          { Lexeme _ (TKeyword "i64x2.le_u") }
'i8x16.gt_s'          { Lexeme _ (TKeyword "i8x16.gt_s") }
'i16x8.gt_s'          { Lexeme _ (TKeyword "i16x8.gt_s") }
'i32x4.gt_s'          { Lexeme _ (TKeyword "i32x4.gt_s") }
'i64x2.gt_s'          { Lexeme _ (TKeyword "i64x2.gt_s") }
'i8x16.gt_u'          { Lexeme _ (TKeyword "i8x16.gt_u") }
'i16x8.gt_u'          { Lexeme _ (TKeyword "i16x8.gt_u") }
'i32x4.gt_u'          { Lexeme _ (TKeyword "i32x4.gt_u") }
'i64x2.gt_u'          { Lexeme _ (TKeyword "i64x2.gt_u") }
'i8x16.ge_s'          { Lexeme _ (TKeyword "i8x16.ge_s") }
'i16x8.ge_s'          { Lexeme _ (TKeyword "i16x8.ge_s") }
'i32x4.ge_s'          { Lexeme _ (TKeyword "i32x4.ge_s") }
'i64x2.ge_s'          { Lexeme _ (TKeyword "i64x2.ge_s") }
'i8x16.ge_u'          { Lexeme _ (TKeyword "i8x16.ge_u") }
'i16x8.ge_u'          { Lexeme _ (TKeyword "i16x8.ge_u") }
'i32x4.ge_u'          { Lexeme _ (TKeyword "i32x4.ge_u") }
'i64x2.ge_u'          { Lexeme _ (TKeyword "i64x2.ge_u") }
'f32x4.add'           { Lexeme _ (TKeyword "f32x4.add") }
'f64x2.add'           { Lexeme _ (TKeyword "f64x2.add") }
'f32x4.sub'           { Lexeme _ (TKeyword "f32x4.sub") }
'f64x2.sub'           { Lexeme _ (TKeyword "f64x2.sub") }
'f32x4.mul'           { Lexeme _ (TKeyword "f32x4.mul") }
'f64x2.mul'           { Lexeme _ (TKeyword "f64x2.mul") }
'f32x4.div'           { Lexeme _ (TKeyword "f32x4.div") }
'f64x2.div'           { Lexeme _ (TKeyword "f64x2.div") }
'f32x4.min'           { Lexeme _ (TKeyword "f32x4.min") }
'f64x2.min'           { Lexeme _ (TKeyword "f64x2.min") }
'f32x4.max'           { Lexeme _ (TKeyword "f32x4.max") }
'f64x2.max'           { Lexeme _ (TKeyword "f64x2.max") }
'f32x4.pmin'          { Lexeme _ (TKeyword "f32x4.pmin") }
'f64x2.pmin'          { Lexeme _ (TKeyword "f64x2.pmin") }
'f32x4.pmax'          { Lexeme _ (TKeyword "f32x4.pmax") }
'f64x2.pmax'          { Lexeme _ (TKeyword "f64x2.pmax") }
'f32x4.abs'           { Lexeme _ (TKeyword "f32x4.abs") }
'f64x2.abs'           { Lexeme _ (TKeyword "f64x2.abs") }
'f32x4.neg'           { Lexeme _ (TKeyword "f32x4.neg") }
'f64x2.neg'           { Lexeme _ (TKeyword "f64x2.neg") }
'f32x4.ceil'          { Lexeme _ (TKeyword "f32x4.ceil") }
'f64x2.ceil'          { Lexeme _ (TKeyword "f64x2.ceil") }
'f32x4.floor'         { Lexeme _ (TKeyword "f32x4.floor") }
'f64x2.floor'         { Lexeme _ (TKeyword "f64x2.floor") }
'f32x4.trunc'         { Lexeme _ (TKeyword "f32x4.trunc") }
'f64x2.trunc'         { Lexeme _ (TKeyword "f64x2.trunc") }
'f32x4.nearest'       { Lexeme _ (TKeyword "f32x4.nearest") }
'f64x2.nearest'       { Lexeme _ (TKeyword "f64x2.nearest") }
'f32x4.sqrt'          { Lexeme _ (TKeyword "f32x4.sqrt") }
'f64x2.sqrt'          { Lexeme _ (TKeyword "f64x2.sqrt") }
'f32x4.eq'            { Lexeme _ (TKeyword "f32x4.eq") }
'f64x2.eq'            { Lexeme _ (TKeyword "f64x2.eq") }
'f32x4.ne'            { Lexeme _ (TKeyword "f32x4.ne") }
'f64x2.ne'            { Lexeme _ (TKeyword "f64x2.ne") }
'f32x4.lt'            { Lexeme _ (TKeyword "f32x4.lt") }
'f64x2.lt'            { Lexeme _ (TKeyword "f64x2.lt") }
'f32x4.le'            { Lexeme _ (TKeyword "f32x4.le") }
'f64x2.le'            { Lexeme _ (TKeyword "f64x2.le") }
'f32x4.gt'            { Lexeme _ (TKeyword "f32x4.gt") }
'f64x2.gt'            { Lexeme _ (TKeyword "f64x2.gt") }
'f32x4.ge'            { Lexeme _ (TKeyword "f32x4.ge") }
'f64x2.ge'            { Lexeme _ (TKeyword "f64x2.ge") }
'i16x8.extend_high_i8x16_s' { Lexeme _ (TKeyword "i16x8.extend_high_i8x16_s") }
'i16x8.extend_high_i8x16_u' { Lexeme _ (TKeyword "i16x8.extend_high_i8x16_u") }
'i16x8.extend_low_i8x16_s'  { Lexeme _ (TKeyword "i16x8.extend_low_i8x16_s") }
'i16x8.extend_low_i8x16_u'  { Lexeme _ (TKeyword "i16x8.extend_low_i8x16_u") }
'i32x4.extend_high_i16x8_s' { Lexeme _ (TKeyword "i32x4.extend_high_i16x8_s") }
'i32x4.extend_high_i16x8_u' { Lexeme _ (TKeyword "i32x4.extend_high_i16x8_u") }
'i32x4.extend_low_i16x8_s'  { Lexeme _ (TKeyword "i32x4.extend_low_i16x8_s") }
'i32x4.extend_low_i16x8_u'  { Lexeme _ (TKeyword "i32x4.extend_low_i16x8_u") }
'i64x2.extend_high_i32x4_s' { Lexeme _ (TKeyword "i64x2.extend_high_i32x4_s") }
'i64x2.extend_high_i32x4_u' { Lexeme _ (TKeyword "i64x2.extend_high_i32x4_u") }
'i64x2.extend_low_i32x4_s'  { Lexeme _ (TKeyword "i64x2.extend_low_i32x4_s") }
'i64x2.extend_low_i32x4_u'  { Lexeme _ (TKeyword "i64x2.extend_low_i32x4_u") }
-- script extension
'binary'              { Lexeme _ (TKeyword "binary") }
'quote'               { Lexeme _ (TKeyword "quote") }
'register'            { Lexeme _ (TKeyword "register") }
'invoke'              { Lexeme _ (TKeyword "invoke") }
'get'                 { Lexeme _ (TKeyword "get") }
'assert_return'       { Lexeme $$ (TKeyword "assert_return") }
'assert_return_canonical_nan' { Lexeme $$ (TKeyword "assert_return_canonical_nan") }
'assert_return_arithmetic_nan' { Lexeme $$ (TKeyword "assert_return_arithmetic_nan") }
'assert_trap'         { Lexeme $$ (TKeyword "assert_trap") }
'assert_malformed'    { Lexeme $$ (TKeyword "assert_malformed") }
'assert_invalid'      { Lexeme $$ (TKeyword "assert_invalid") }
'assert_unlinkable'   { Lexeme $$ (TKeyword "assert_unlinkable") }
'assert_exhaustion'   { Lexeme $$ (TKeyword "assert_exhaustion") }
'script'              { Lexeme _ (TKeyword "script") }
'input'               { Lexeme _ (TKeyword "input") }
'output'              { Lexeme _ (TKeyword "output") }
-- script extension end
id                    { Lexeme _ (TId $$) }
signed_pos            { Lexeme _ (TIntLit (Just False) $$) }
signed_neg            { Lexeme _ (TIntLit (Just True) $$) }
nat                   { Lexeme _ (TIntLit Nothing $$) }
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
    | 'v128' { V128 }
    | 'funcref' { Func }
    | 'externref' { Extern }

int :: {Integer}
    : signed_neg { $1 }
    | signed_pos { $1 }
    | nat { $1 }

index :: { Index }
    : u32 { Index $1 }
    | ident { Named $1 }

i8 :: { Integer }
    : int {%
        if $1 >= -(2^7) && $1 < 2^8
        then Right $ fromIntegral $ if $1 >= 0 then $1 else 2^8 + $1
        else Left ("I8 literal value is out of signed i8 boundaries: " ++ show $1)
    }

i16 :: { Integer }
    : int {%
        if $1 >= -(2^15) && $1 < 2^16
        then Right $ fromIntegral $ if $1 >= 0 then $1 else 2^16 + $1
        else Left ("I16 literal value is out of signed i16 boundaries: " ++ show $1)
    }

int32 :: { Integer }
    : int {%
        if $1 >= -(2^31) && $1 < 2^32
        then Right $1
        else Left ("Int literal value is out of signed int32 boundaries: " ++ show $1)
    }

u32 :: { Natural }
    : int {%
        if $1 >= 0 && $1 < 2^32
        then Right (fromIntegral $1)
        else Left ("Int literal value is out of unsigned int32 boundaries: " ++ show $1)
    }

int64 :: { Integer }
    : int {%
        if $1 >= -(2^63) && $1 < 2^64
        then Right $1
        else Left ("Int literal value is out of signed int64 boundaries: " ++ show $1)
    }

float32 :: { FloatRep }
    : signed_neg {%
        -- it is stupid, but to preserve minus bit of "-0" we have to do it
        let maxInt = 340282356779733623858607532500980858880 in
        if $1 <= maxInt && $1 >= -maxInt
        then return $ BinRep $ if $1 == 0 then negate $ fromIntegral $1 else fromIntegral $1
        else Left "constant out of range"
    }
    | signed_pos {%
        let maxInt = 340282356779733623858607532500980858880 in
        if $1 <= maxInt && $1 >= -maxInt
        then return $ BinRep $ fromIntegral $1
        else Left "constant out of range"
    }
    | nat {%
        let maxInt = 340282356779733623858607532500980858880 in
        if $1 <= maxInt && $1 >= -maxInt
        then return $ BinRep $ fromIntegral $1
        else Left "constant out of range"
    }
    | f64 { $1 }

float64 :: { FloatRep }
    : signed_neg {%
        -- it is stupid, but to preserve minus bit of "-0" we have to do it
        let maxInt = round (maxFinite :: Double) in
        if $1 <= maxInt && $1 >= -maxInt
        then fmap (BinRep . if $1 == 0 then negate else id) $ doubleFromInteger $1
        else Left "constant out of range"
    }
    | signed_pos {%
        let maxInt = round (maxFinite :: Double) in
        if $1 <= maxInt && $1 >= -maxInt
        then fmap BinRep $ doubleFromInteger $1
        else Left "constant out of range"
    }
    | nat {%
        let maxInt = round (maxFinite :: Double) in
        if $1 <= maxInt && $1 >= -maxInt
        then fmap BinRep $ doubleFromInteger $1
        else Left "constant out of range"
    }
    | f64 { $1 }

simd_shape :: { SimdShape }
    : 'i8x16' { I8x16 }
    | 'i16x8' { I16x8 }
    | 'i32x4' { I32x4 }
    | 'i64x2' { I64x2 }
    | 'f32x4' { F32x4 }
    | 'f64x2' { F64x2 }

v128_const :: { V128Rep }
    : 'i8x16' i8 i8 i8 i8 i8 i8 i8 i8 i8 i8 i8 i8 i8 i8 i8 i8 {
        I8x16Const [$2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17]
    }
    | 'i16x8' i16 i16 i16 i16 i16 i16 i16 i16 {
        I16x8Const [$2, $3, $4, $5, $6, $7, $8, $9]
    }
    | 'i32x4' int32 int32 int32 int32 {
        I32x4Const [$2, $3, $4, $5]
    }
    | 'i64x2' int64 int64 {
        I64x2Const [$2, $3]
    }
    | 'f32x4' float32 float32 float32 float32 {
        F32x4Const [$2, $3, $4, $5]
    }
    | 'f64x2' float64 float64 {
        F64x2Const [$2, $3]
    }

lane_index :: { Natural }
    : nat {% if $1 < 256 then Right $ fromIntegral $1 else Left "malformed lane index"}

plaininstr :: { PlainInstr }
    -- control instructions
    : 'unreachable'                  { Unreachable }
    | 'nop'                          { Nop }
    | 'br' index                     { Br $2 }
    | 'br_if' index                  { BrIf $2 }
    | 'br_table' rev_list1(index)    { BrTable (reverse $ tail $2) (head $2) }
    | 'return'                       { Return }
    | 'call' index                   { Call $2 }
    | 'drop'                         { Drop }
    -- reference instructions
    | 'ref.null' heaptype            { RefNull $2 }
    | 'ref.is_null'                  { RefIsNull }
    | 'ref.func' index               { RefFunc $2 }
    | 'ref.extern' u32               { RefExtern $2 }
    -- variable instructions
    | 'get_local' index              { GetLocal $2 }
    | 'set_local' index              { SetLocal $2 }
    | 'tee_local' index              { TeeLocal $2 }
    | 'get_global' index             { GetGlobal $2 }
    | 'set_global' index             { SetGlobal $2 }
    -- memory instructions
    | 'i32.load' memarg4             { I32Load $2 }
    | 'i64.load' memarg8             { I64Load $2 }
    | 'f32.load' memarg4             { F32Load $2 }
    | 'f64.load' memarg8             { F64Load $2 }
    | 'v128.load' memarg16           { V128Load $2 }
    | 'v128.load8_lane' memarg1 lane_index  { V128Load8Lane $2 $3 }
    | 'v128.load16_lane' memarg2 lane_index { V128Load16Lane $2 $3 }
    | 'v128.load32_lane' memarg4 lane_index { V128Load32Lane $2 $3 }
    | 'v128.load64_lane' memarg8 lane_index { V128Load64Lane $2 $3 }
    | 'v128.load8_splat' memarg1     { V128Load8Splat $2 }
    | 'v128.load16_splat' memarg2    { V128Load16Splat $2 }
    | 'v128.load32_splat' memarg4    { V128Load32Splat $2 }
    | 'v128.load64_splat' memarg8    { V128Load64Splat $2 }
    | 'v128.load32_zero' memarg4     { V128Load32Zero $2 }
    | 'v128.load64_zero' memarg8     { V128Load64Zero $2 }
    | 'v128.load8x8_s' memarg8       { V128Load8x8S $2 }
    | 'v128.load8x8_u' memarg8       { V128Load8x8U $2 }
    | 'v128.load16x4_s' memarg8      { V128Load16x4S $2 }
    | 'v128.load16x4_u' memarg8      { V128Load16x4U $2 }
    | 'v128.load32x2_s' memarg8      { V128Load32x2S $2 }
    | 'v128.load32x2_u' memarg8      { V128Load32x2U $2 }
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
    | 'v128.store' memarg16          { V128Store $2 }
    | 'v128.store8_lane' memarg1 lane_index { V128Store8Lane $2 $3 }
    | 'v128.store16_lane' memarg2 lane_index { V128Store16Lane $2 $3 }
    | 'v128.store32_lane' memarg4 lane_index { V128Store32Lane $2 $3 }
    | 'v128.store64_lane' memarg8 lane_index { V128Store64Lane $2 $3 }
    | 'i32.store8' memarg1           { I32Store8 $2 }
    | 'i32.store16' memarg2          { I32Store16 $2 }
    | 'i64.store8' memarg1           { I64Store8 $2 }
    | 'i64.store16' memarg2          { I64Store16 $2 }
    | 'i64.store32' memarg4          { I64Store32 $2 }
    | 'memory.size'                  { MemorySize }
    | 'memory.grow'                  { MemoryGrow }
    | 'memory.fill'                  { MemoryFill }
    | 'memory.copy'                  { MemoryCopy }
    | 'memory.init' index            { MemoryInit $2 }
    | 'data.drop' index              { DataDrop $2 }
    -- table instructions
    | 'table.init' index opt(index) {
        case $3 of
            Nothing -> TableInit (Index 0) $2
            Just elemIdx -> TableInit $2 elemIdx
    }
    | 'table.get' opt(index)             { TableGet (fromMaybe (Index 0) $2) }
    | 'table.set' opt(index)             { TableSet (fromMaybe (Index 0) $2) }
    | 'table.copy' opt(index) opt(index) { TableCopy (fromMaybe (Index 0) $2) (fromMaybe (Index 0) $3) }
    | 'table.fill' opt(index)            { TableFill (fromMaybe (Index 0) $2) }
    | 'table.size' opt(index)            { TableSize (fromMaybe (Index 0) $2) }
    | 'table.grow' opt(index)            { TableGrow (fromMaybe (Index 0) $2) }
    | 'elem.drop' index                  { ElemDrop $2 }
    -- numeric instructions    
    | 'i32.const' int32                  { I32Const $2 }
    | 'i64.const' int64                  { I64Const $2 }
    | 'f32.const' float32                { F32Const $2 }
    | 'f64.const' float64                { F64Const $2 }
    | 'v128.const' v128_const            { V128Const $2 }
    | 'i32.clz'                          { IUnOp BS32 IClz }
    | 'i32.ctz'                          { IUnOp BS32 ICtz }
    | 'i32.popcnt'                       { IUnOp BS32 IPopcnt }
    | 'i32.extend8_s'                    { IUnOp BS32 IExtend8S }
    | 'i32.extend16_s'                   { IUnOp BS32 IExtend16S }
    | 'i32.add'                          { IBinOp BS32 IAdd }
    | 'i32.sub'                          { IBinOp BS32 ISub }
    | 'i32.mul'                          { IBinOp BS32 IMul }
    | 'i32.div_s'                        { IBinOp BS32 IDivS }
    | 'i32.div_u'                        { IBinOp BS32 IDivU }
    | 'i32.rem_s'                        { IBinOp BS32 IRemS }
    | 'i32.rem_u'                        { IBinOp BS32 IRemU }
    | 'i32.and'                          { IBinOp BS32 IAnd }
    | 'i32.or'                           { IBinOp BS32 IOr }
    | 'i32.xor'                          { IBinOp BS32 IXor }
    | 'i32.shl'                          { IBinOp BS32 IShl }
    | 'i32.shr_s'                        { IBinOp BS32 IShrS }
    | 'i32.shr_u'                        { IBinOp BS32 IShrU }
    | 'i32.rotl'                         { IBinOp BS32 IRotl }
    | 'i32.rotr'                         { IBinOp BS32 IRotr }
    | 'i64.clz'                          { IUnOp BS64 IClz }
    | 'i64.ctz'                          { IUnOp BS64 ICtz }
    | 'i64.popcnt'                       { IUnOp BS64 IPopcnt }
    | 'i64.extend8_s'                    { IUnOp BS64 IExtend8S }
    | 'i64.extend16_s'                   { IUnOp BS64 IExtend16S }
    | 'i64.extend32_s'                   { IUnOp BS64 IExtend32S }
    | 'i64.add'                          { IBinOp BS64 IAdd }
    | 'i64.sub'                          { IBinOp BS64 ISub }
    | 'i64.mul'                          { IBinOp BS64 IMul }
    | 'i64.div_s'                        { IBinOp BS64 IDivS }
    | 'i64.div_u'                        { IBinOp BS64 IDivU }
    | 'i64.rem_s'                        { IBinOp BS64 IRemS }
    | 'i64.rem_u'                        { IBinOp BS64 IRemU }
    | 'i64.and'                          { IBinOp BS64 IAnd }
    | 'i64.or'                           { IBinOp BS64 IOr }
    | 'i64.xor'                          { IBinOp BS64 IXor }
    | 'i64.shl'                          { IBinOp BS64 IShl }
    | 'i64.shr_s'                        { IBinOp BS64 IShrS }
    | 'i64.shr_u'                        { IBinOp BS64 IShrU }
    | 'i64.rotl'                         { IBinOp BS64 IRotl }
    | 'i64.rotr'                         { IBinOp BS64 IRotr }
    | 'f32.abs'                          { FUnOp BS32 FAbs }
    | 'f32.neg'                          { FUnOp BS32 FNeg }
    | 'f32.ceil'                         { FUnOp BS32 FCeil }
    | 'f32.floor'                        { FUnOp BS32 FFloor }
    | 'f32.trunc'                        { FUnOp BS32 FTrunc }
    | 'f32.nearest'                      { FUnOp BS32 FNearest }
    | 'f32.sqrt'                         { FUnOp BS32 FSqrt }
    | 'f32.add'                          { FBinOp BS32 FAdd }
    | 'f32.sub'                          { FBinOp BS32 FSub }
    | 'f32.mul'                          { FBinOp BS32 FMul }
    | 'f32.div'                          { FBinOp BS32 FDiv }
    | 'f32.min'                          { FBinOp BS32 FMin }
    | 'f32.max'                          { FBinOp BS32 FMax }
    | 'f32.copysign'                     { FBinOp BS32 FCopySign }
    | 'f64.abs'                          { FUnOp BS64 FAbs }
    | 'f64.neg'                          { FUnOp BS64 FNeg }
    | 'f64.ceil'                         { FUnOp BS64 FCeil }
    | 'f64.floor'                        { FUnOp BS64 FFloor }
    | 'f64.trunc'                        { FUnOp BS64 FTrunc }
    | 'f64.nearest'                      { FUnOp BS64 FNearest }
    | 'f64.sqrt'                         { FUnOp BS64 FSqrt }
    | 'f64.add'                          { FBinOp BS64 FAdd }
    | 'f64.sub'                          { FBinOp BS64 FSub }
    | 'f64.mul'                          { FBinOp BS64 FMul }
    | 'f64.div'                          { FBinOp BS64 FDiv }
    | 'f64.min'                          { FBinOp BS64 FMin }
    | 'f64.max'                          { FBinOp BS64 FMax }
    | 'f64.copysign'                     { FBinOp BS64 FCopySign }
    | 'i32.eqz'                          { I32Eqz }
    | 'i32.eq'                           { IRelOp BS32 IEq }
    | 'i32.ne'                           { IRelOp BS32 INe }
    | 'i32.lt_s'                         { IRelOp BS32 ILtS }
    | 'i32.lt_u'                         { IRelOp BS32 ILtU }
    | 'i32.gt_s'                         { IRelOp BS32 IGtS }
    | 'i32.gt_u'                         { IRelOp BS32 IGtU }
    | 'i32.le_s'                         { IRelOp BS32 ILeS }
    | 'i32.le_u'                         { IRelOp BS32 ILeU }
    | 'i32.ge_s'                         { IRelOp BS32 IGeS }
    | 'i32.ge_u'                         { IRelOp BS32 IGeU }
    | 'i64.eqz'                          { I64Eqz }
    | 'i64.eq'                           { IRelOp BS64 IEq }
    | 'i64.ne'                           { IRelOp BS64 INe }
    | 'i64.lt_s'                         { IRelOp BS64 ILtS }
    | 'i64.lt_u'                         { IRelOp BS64 ILtU }
    | 'i64.gt_s'                         { IRelOp BS64 IGtS }
    | 'i64.gt_u'                         { IRelOp BS64 IGtU }
    | 'i64.le_s'                         { IRelOp BS64 ILeS }
    | 'i64.le_u'                         { IRelOp BS64 ILeU }
    | 'i64.ge_s'                         { IRelOp BS64 IGeS }
    | 'i64.ge_u'                         { IRelOp BS64 IGeU }
    | 'f32.eq'                           { FRelOp BS32 FEq }
    | 'f32.ne'                           { FRelOp BS32 FNe }
    | 'f32.lt'                           { FRelOp BS32 FLt }
    | 'f32.gt'                           { FRelOp BS32 FGt }
    | 'f32.le'                           { FRelOp BS32 FLe }
    | 'f32.ge'                           { FRelOp BS32 FGe }
    | 'f64.eq'                           { FRelOp BS64 FEq }
    | 'f64.ne'                           { FRelOp BS64 FNe }
    | 'f64.lt'                           { FRelOp BS64 FLt }
    | 'f64.gt'                           { FRelOp BS64 FGt }
    | 'f64.le'                           { FRelOp BS64 FLe }
    | 'f64.ge'                           { FRelOp BS64 FGe }
    | 'i32.wrap_i64'                     { I32WrapI64 }
    | 'i32.trunc_f32_s'                  { ITruncFS BS32 BS32 }
    | 'i32.trunc_f32_u'                  { ITruncFU BS32 BS32 }
    | 'i32.trunc_f64_s'                  { ITruncFS BS32 BS64 }
    | 'i32.trunc_f64_u'                  { ITruncFU BS32 BS64 }
    | 'i32.trunc_sat_f32_s'              { ITruncSatFS BS32 BS32 }
    | 'i32.trunc_sat_f32_u'              { ITruncSatFU BS32 BS32 }
    | 'i32.trunc_sat_f64_s'              { ITruncSatFS BS32 BS64 }
    | 'i32.trunc_sat_f64_u'              { ITruncSatFU BS32 BS64 }
    | 'i64.extend_i32_s'                 { I64ExtendSI32 }
    | 'i64.extend_i32_u'                 { I64ExtendUI32 }
    | 'i64.trunc_f32_s'                  { ITruncFS BS64 BS32 }
    | 'i64.trunc_f32_u'                  { ITruncFU BS64 BS32 }
    | 'i64.trunc_f64_s'                  { ITruncFS BS64 BS64 }
    | 'i64.trunc_f64_u'                  { ITruncFU BS64 BS64 }
    | 'i64.trunc_sat_f32_s'              { ITruncSatFS BS64 BS32 }
    | 'i64.trunc_sat_f32_u'              { ITruncSatFU BS64 BS32 }
    | 'i64.trunc_sat_f64_s'              { ITruncSatFS BS64 BS64 }
    | 'i64.trunc_sat_f64_u'              { ITruncSatFU BS64 BS64 }
    | 'f32.convert_i32_s'                { FConvertIS BS32 BS32 }
    | 'f32.convert_i32_u'                { FConvertIU BS32 BS32 }
    | 'f32.convert_i64_s'                { FConvertIS BS32 BS64 }
    | 'f32.convert_i64_u'                { FConvertIU BS32 BS64 }
    | 'f32x4.convert_i32x4_s'            { FConvertIS (BS128 F32x4) (BS128 I32x4) }
    | 'f32x4.convert_i32x4_u'            { FConvertIU (BS128 F32x4) (BS128 I32x4) }
    | 'f64x2.convert_low_i32x4_s'        { FConvertIS (BS128 F64x2) (BS128 I32x4) }
    | 'f64x2.convert_low_i32x4_u'        { FConvertIU (BS128 F64x2) (BS128 I32x4) }
    | 'i8x16.narrow_i16x8_s'             { V128Narrow I8x16 I16x8 True }
    | 'i8x16.narrow_i16x8_u'             { V128Narrow I8x16 I16x8 False }
    | 'i16x8.narrow_i32x4_s'             { V128Narrow I16x8 I32x4 True }
    | 'i16x8.narrow_i32x4_u'             { V128Narrow I16x8 I32x4 False }
    | 'f64x2.promote_low_f32x4'          { F64x2PromoteLowF32x4 }
    | 'f32x4.demote_f64x2_zero'          { F32x4DemoteF64x2Zero }
    | 'f32.demote_f64'                   { F32DemoteF64 }
    | 'f64.convert_i32_s'                { FConvertIS BS64 BS32 }
    | 'f64.convert_i32_u'                { FConvertIU BS64 BS32 }
    | 'f64.convert_i64_s'                { FConvertIS BS64 BS64 }
    | 'f64.convert_i64_u'                { FConvertIU BS64 BS64 }
    | 'f64.promote_f32'                  { F64PromoteF32 }
    | 'i32.reinterpret_f32'              { IReinterpretF BS32 }
    | 'i64.reinterpret_f64'              { IReinterpretF BS64 }
    | 'f32.reinterpret_i32'              { FReinterpretI BS32 }
    | 'f64.reinterpret_i64'              { FReinterpretI BS64 }
    -- simd
    | 'i8x16.shuffle' u32 u32 u32 u32 u32 u32 u32 u32 u32 u32 u32 u32 u32 u32 u32 u32 {%
        let idxs = [$2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17] in
        if any (\i -> i < 0 || i > 0xFF) idxs
        then Left "malformed lane index"
        else Right $ I8x16Shuffle $ map fromIntegral idxs
    }
    | 'i8x16.swizzle'                    { I8x16Swizzle }
    | 'v128.not'                         { IUnOp (BS128 I128x1) INot }
    | 'v128.and'                         { IBinOp (BS128 I128x1) IAnd }
    | 'v128.andnot'                      { IBinOp (BS128 I128x1) IAndNot }
    | 'v128.or'                          { IBinOp (BS128 I128x1) IOr }
    | 'v128.xor'                         { IBinOp (BS128 I128x1) IXor }
    | 'v128.bitselect'                   { V128BitSelect }
    | 'v128.any_true'                    { V128AnyTrue }
    | 'i8x16.splat'                      { V128Splat I8x16 }
    | 'i16x8.splat'                      { V128Splat I16x8 }
    | 'i32x4.splat'                      { V128Splat I32x4 }
    | 'i64x2.splat'                      { V128Splat I64x2 }
    | 'f32x4.splat'                      { V128Splat F32x4 }
    | 'f64x2.splat'                      { V128Splat F64x2 }
    | 'i8x16.all_true'                   { V128AllTrue I8x16 }
    | 'i16x8.all_true'                   { V128AllTrue I16x8 }
    | 'i32x4.all_true'                   { V128AllTrue I32x4 }
    | 'i64x2.all_true'                   { V128AllTrue I64x2 }
    | 'i8x16.extract_lane_s' lane_index  { V128ExtractLane I8x16 $2 True }
    | 'i16x8.extract_lane_s' lane_index  { V128ExtractLane I16x8 $2 True }
    | 'i8x16.extract_lane_u' lane_index  { V128ExtractLane I8x16 $2 False }
    | 'i16x8.extract_lane_u' lane_index  { V128ExtractLane I16x8 $2 False }
    | 'i32x4.extract_lane' lane_index    { V128ExtractLane I32x4 $2 False }
    | 'i64x2.extract_lane' lane_index    { V128ExtractLane I64x2 $2 False }
    | 'f32x4.extract_lane' lane_index    { V128ExtractLane F32x4 $2 False }
    | 'f64x2.extract_lane' lane_index    { V128ExtractLane F64x2 $2 False }
    | 'i8x16.replace_lane' lane_index    { V128ReplaceLane I8x16 $2 }
    | 'i16x8.replace_lane' lane_index    { V128ReplaceLane I16x8 $2 }
    | 'i32x4.replace_lane' lane_index    { V128ReplaceLane I32x4 $2 }
    | 'i64x2.replace_lane' lane_index    { V128ReplaceLane I64x2 $2 }
    | 'f32x4.replace_lane' lane_index    { V128ReplaceLane F32x4 $2 }
    | 'f64x2.replace_lane' lane_index    { V128ReplaceLane F64x2 $2 }
    | 'i8x16.add'                        { IBinOp (BS128 I8x16) IAdd }
    | 'i16x8.add'                        { IBinOp (BS128 I16x8) IAdd }
    | 'i32x4.add'                        { IBinOp (BS128 I32x4) IAdd }
    | 'i64x2.add'                        { IBinOp (BS128 I64x2) IAdd }
    | 'i8x16.sub'                        { IBinOp (BS128 I8x16) ISub }
    | 'i16x8.sub'                        { IBinOp (BS128 I16x8) ISub }
    | 'i32x4.sub'                        { IBinOp (BS128 I32x4) ISub }
    | 'i64x2.sub'                        { IBinOp (BS128 I64x2) ISub }
    | 'i8x16.add_sat_s'                  { IBinOp (BS128 I8x16) IAddSatS }
    | 'i16x8.add_sat_s'                  { IBinOp (BS128 I16x8) IAddSatS }
    | 'i8x16.sub_sat_s'                  { IBinOp (BS128 I8x16) ISubSatS }
    | 'i16x8.sub_sat_s'                  { IBinOp (BS128 I16x8) ISubSatS }
    | 'i8x16.add_sat_u'                  { IBinOp (BS128 I8x16) IAddSatU }
    | 'i16x8.add_sat_u'                  { IBinOp (BS128 I16x8) IAddSatU }
    | 'i8x16.sub_sat_u'                  { IBinOp (BS128 I8x16) ISubSatU }
    | 'i16x8.sub_sat_u'                  { IBinOp (BS128 I16x8) ISubSatU }
    | 'i8x16.avgr_u'                     { IBinOp (BS128 I8x16) IAvgrU }
    | 'i16x8.avgr_u'                     { IBinOp (BS128 I16x8) IAvgrU }
    | 'i8x16.min_s'                      { IBinOp (BS128 I8x16) IMinS }
    | 'i16x8.min_s'                      { IBinOp (BS128 I16x8) IMinS }
    | 'i32x4.min_s'                      { IBinOp (BS128 I32x4) IMinS }
    | 'i8x16.min_u'                      { IBinOp (BS128 I8x16) IMinU }
    | 'i16x8.min_u'                      { IBinOp (BS128 I16x8) IMinU }
    | 'i32x4.min_u'                      { IBinOp (BS128 I32x4) IMinU }
    | 'i8x16.max_s'                      { IBinOp (BS128 I8x16) IMaxS }
    | 'i16x8.max_s'                      { IBinOp (BS128 I16x8) IMaxS }
    | 'i32x4.max_s'                      { IBinOp (BS128 I32x4) IMaxS }
    | 'i8x16.max_u'                      { IBinOp (BS128 I8x16) IMaxU }
    | 'i16x8.max_u'                      { IBinOp (BS128 I16x8) IMaxU }
    | 'i32x4.max_u'                      { IBinOp (BS128 I32x4) IMaxU }
    | 'i16x8.extmul_low_i8x16_s'         { IBinOp (BS128 I16x8) (IExtMul True False) }
    | 'i32x4.extmul_low_i16x8_s'         { IBinOp (BS128 I32x4) (IExtMul True False) }
    | 'i64x2.extmul_low_i32x4_s'         { IBinOp (BS128 I64x2) (IExtMul True False) }
    | 'i16x8.extmul_low_i8x16_u'         { IBinOp (BS128 I16x8) (IExtMul False False) }
    | 'i32x4.extmul_low_i16x8_u'         { IBinOp (BS128 I32x4) (IExtMul False False) }
    | 'i64x2.extmul_low_i32x4_u'         { IBinOp (BS128 I64x2) (IExtMul False False) }
    | 'i16x8.extmul_high_i8x16_s'        { IBinOp (BS128 I16x8) (IExtMul True True) }
    | 'i32x4.extmul_high_i16x8_s'        { IBinOp (BS128 I32x4) (IExtMul True True) }
    | 'i64x2.extmul_high_i32x4_s'        { IBinOp (BS128 I64x2) (IExtMul True True) }
    | 'i16x8.extmul_high_i8x16_u'        { IBinOp (BS128 I16x8) (IExtMul False True) }
    | 'i32x4.extmul_high_i16x8_u'        { IBinOp (BS128 I32x4) (IExtMul False True) }
    | 'i64x2.extmul_high_i32x4_u'        { IBinOp (BS128 I64x2) (IExtMul False True) }
    | 'i16x8.mul'                        { IBinOp (BS128 I16x8) IMul }
    | 'i32x4.mul'                        { IBinOp (BS128 I32x4) IMul }
    | 'i64x2.mul'                        { IBinOp (BS128 I64x2) IMul }
    | 'i8x16.shl'                        { IBinOp (BS128 I8x16) IShl }
    | 'i16x8.shl'                        { IBinOp (BS128 I16x8) IShl }
    | 'i32x4.shl'                        { IBinOp (BS128 I32x4) IShl }
    | 'i64x2.shl'                        { IBinOp (BS128 I64x2) IShl }
    | 'i8x16.shr_u'                      { IBinOp (BS128 I8x16) IShrU }
    | 'i16x8.shr_u'                      { IBinOp (BS128 I16x8) IShrU }
    | 'i32x4.shr_u'                      { IBinOp (BS128 I32x4) IShrU }
    | 'i64x2.shr_u'                      { IBinOp (BS128 I64x2) IShrU }
    | 'i8x16.shr_s'                      { IBinOp (BS128 I8x16) IShrS }
    | 'i16x8.shr_s'                      { IBinOp (BS128 I16x8) IShrS }
    | 'i32x4.shr_s'                      { IBinOp (BS128 I32x4) IShrS }
    | 'i64x2.shr_s'                      { IBinOp (BS128 I64x2) IShrS }
    | 'i8x16.popcnt'                     { IUnOp (BS128 I8x16) IPopcnt }
    | 'i8x16.abs'                        { IUnOp (BS128 I8x16) IAbs }
    | 'i16x8.abs'                        { IUnOp (BS128 I16x8) IAbs }
    | 'i32x4.abs'                        { IUnOp (BS128 I32x4) IAbs }
    | 'i64x2.abs'                        { IUnOp (BS128 I64x2) IAbs }
    | 'i8x16.neg'                        { IUnOp (BS128 I8x16) INeg }
    | 'i16x8.neg'                        { IUnOp (BS128 I16x8) INeg }
    | 'i32x4.neg'                        { IUnOp (BS128 I32x4) INeg }
    | 'i64x2.neg'                        { IUnOp (BS128 I64x2) INeg }
    | 'i16x8.extadd_pairwise_i8x16_s'    { IUnOp (BS128 I16x8) (IExtAddPairwise True) }
    | 'i32x4.extadd_pairwise_i16x8_s'    { IUnOp (BS128 I32x4) (IExtAddPairwise True) }
    | 'i16x8.extadd_pairwise_i8x16_u'    { IUnOp (BS128 I16x8) (IExtAddPairwise False) }
    | 'i32x4.extadd_pairwise_i16x8_u'    { IUnOp (BS128 I32x4) (IExtAddPairwise False) }
    | 'i8x16.bitmask'                    { V128BitMask I8x16 }
    | 'i16x8.bitmask'                    { V128BitMask I16x8 }
    | 'i32x4.bitmask'                    { V128BitMask I32x4 }
    | 'i64x2.bitmask'                    { V128BitMask I64x2 }
    | 'i8x16.eq'                         { IRelOp (BS128 I8x16) IEq }
    | 'i16x8.eq'                         { IRelOp (BS128 I16x8) IEq }
    | 'i32x4.eq'                         { IRelOp (BS128 I32x4) IEq }
    | 'i64x2.eq'                         { IRelOp (BS128 I64x2) IEq }
    | 'i8x16.ne'                         { IRelOp (BS128 I8x16) INe }
    | 'i16x8.ne'                         { IRelOp (BS128 I16x8) INe }
    | 'i32x4.ne'                         { IRelOp (BS128 I32x4) INe }
    | 'i64x2.ne'                         { IRelOp (BS128 I64x2) INe }
    | 'i8x16.lt_s'                       { IRelOp (BS128 I8x16) ILtS }
    | 'i16x8.lt_s'                       { IRelOp (BS128 I16x8) ILtS }
    | 'i32x4.lt_s'                       { IRelOp (BS128 I32x4) ILtS }
    | 'i64x2.lt_s'                       { IRelOp (BS128 I64x2) ILtS }
    | 'i8x16.lt_u'                       { IRelOp (BS128 I8x16) ILtU }
    | 'i16x8.lt_u'                       { IRelOp (BS128 I16x8) ILtU }
    | 'i32x4.lt_u'                       { IRelOp (BS128 I32x4) ILtU }
    | 'i64x2.lt_u'                       { IRelOp (BS128 I64x2) ILtU }
    | 'i8x16.le_s'                       { IRelOp (BS128 I8x16) ILeS }
    | 'i16x8.le_s'                       { IRelOp (BS128 I16x8) ILeS }
    | 'i32x4.le_s'                       { IRelOp (BS128 I32x4) ILeS }
    | 'i64x2.le_s'                       { IRelOp (BS128 I64x2) ILeS }
    | 'i8x16.le_u'                       { IRelOp (BS128 I8x16) ILeU }
    | 'i16x8.le_u'                       { IRelOp (BS128 I16x8) ILeU }
    | 'i32x4.le_u'                       { IRelOp (BS128 I32x4) ILeU }
    | 'i64x2.le_u'                       { IRelOp (BS128 I64x2) ILeU }
    | 'i8x16.gt_s'                       { IRelOp (BS128 I8x16) IGtS }
    | 'i16x8.gt_s'                       { IRelOp (BS128 I16x8) IGtS }
    | 'i32x4.gt_s'                       { IRelOp (BS128 I32x4) IGtS }
    | 'i64x2.gt_s'                       { IRelOp (BS128 I64x2) IGtS }
    | 'i8x16.gt_u'                       { IRelOp (BS128 I8x16) IGtU }
    | 'i16x8.gt_u'                       { IRelOp (BS128 I16x8) IGtU }
    | 'i32x4.gt_u'                       { IRelOp (BS128 I32x4) IGtU }
    | 'i64x2.gt_u'                       { IRelOp (BS128 I64x2) IGtU }
    | 'i8x16.ge_s'                       { IRelOp (BS128 I8x16) IGeS }
    | 'i16x8.ge_s'                       { IRelOp (BS128 I16x8) IGeS }
    | 'i32x4.ge_s'                       { IRelOp (BS128 I32x4) IGeS }
    | 'i64x2.ge_s'                       { IRelOp (BS128 I64x2) IGeS }
    | 'i8x16.ge_u'                       { IRelOp (BS128 I8x16) IGeU }
    | 'i16x8.ge_u'                       { IRelOp (BS128 I16x8) IGeU }
    | 'i32x4.ge_u'                       { IRelOp (BS128 I32x4) IGeU }
    | 'i64x2.ge_u'                       { IRelOp (BS128 I64x2) IGeU }
    | 'f32x4.add'                        { FBinOp (BS128 F32x4) FAdd }
    | 'f64x2.add'                        { FBinOp (BS128 F64x2) FAdd }
    | 'f32x4.sub'                        { FBinOp (BS128 F32x4) FSub }
    | 'f64x2.sub'                        { FBinOp (BS128 F64x2) FSub }
    | 'f32x4.mul'                        { FBinOp (BS128 F32x4) FMul }
    | 'f64x2.mul'                        { FBinOp (BS128 F64x2) FMul }
    | 'f32x4.div'                        { FBinOp (BS128 F32x4) FDiv }
    | 'f64x2.div'                        { FBinOp (BS128 F64x2) FDiv }
    | 'f32x4.min'                        { FBinOp (BS128 F32x4) FMin }
    | 'f64x2.min'                        { FBinOp (BS128 F64x2) FMin }
    | 'f32x4.max'                        { FBinOp (BS128 F32x4) FMax }
    | 'f64x2.max'                        { FBinOp (BS128 F64x2) FMax }
    | 'f32x4.pmin'                       { FBinOp (BS128 F32x4) FPMin }
    | 'f64x2.pmin'                       { FBinOp (BS128 F64x2) FPMin }
    | 'f32x4.pmax'                       { FBinOp (BS128 F32x4) FPMax }
    | 'f64x2.pmax'                       { FBinOp (BS128 F64x2) FPMax }
    | 'f32x4.abs'                        { FUnOp (BS128 F32x4) FAbs }
    | 'f64x2.abs'                        { FUnOp (BS128 F64x2) FAbs }
    | 'f32x4.neg'                        { FUnOp (BS128 F32x4) FNeg }
    | 'f64x2.neg'                        { FUnOp (BS128 F64x2) FNeg }
    | 'f32x4.ceil'                       { FUnOp (BS128 F32x4) FCeil }
    | 'f64x2.ceil'                       { FUnOp (BS128 F64x2) FCeil }
    | 'f32x4.floor'                      { FUnOp (BS128 F32x4) FFloor }
    | 'f64x2.floor'                      { FUnOp (BS128 F64x2) FFloor }
    | 'f32x4.trunc'                      { FUnOp (BS128 F32x4) FTrunc }
    | 'f64x2.trunc'                      { FUnOp (BS128 F64x2) FTrunc }
    | 'f32x4.nearest'                    { FUnOp (BS128 F32x4) FNearest }
    | 'f64x2.nearest'                    { FUnOp (BS128 F64x2) FNearest }
    | 'f32x4.sqrt'                       { FUnOp (BS128 F32x4) FSqrt }
    | 'f64x2.sqrt'                       { FUnOp (BS128 F64x2) FSqrt }
    | 'f32x4.eq'                         { FRelOp (BS128 F32x4) FEq }
    | 'f64x2.eq'                         { FRelOp (BS128 F64x2) FEq }
    | 'f32x4.ne'                         { FRelOp (BS128 F32x4) FNe }
    | 'f64x2.ne'                         { FRelOp (BS128 F64x2) FNe }
    | 'f32x4.lt'                         { FRelOp (BS128 F32x4) FLt }
    | 'f64x2.lt'                         { FRelOp (BS128 F64x2) FLt }
    | 'f32x4.le'                         { FRelOp (BS128 F32x4) FLe }
    | 'f64x2.le'                         { FRelOp (BS128 F64x2) FLe }
    | 'f32x4.gt'                         { FRelOp (BS128 F32x4) FGt }
    | 'f64x2.gt'                         { FRelOp (BS128 F64x2) FGt }
    | 'f32x4.ge'                         { FRelOp (BS128 F32x4) FGe }
    | 'f64x2.ge'                         { FRelOp (BS128 F64x2) FGe }
    | 'i16x8.extend_high_i8x16_s'        { V128IExtend I16x8 I8x16 True True }
    | 'i16x8.extend_high_i8x16_u'        { V128IExtend I16x8 I8x16 True False }
    | 'i16x8.extend_low_i8x16_s'         { V128IExtend I16x8 I8x16 False True }
    | 'i16x8.extend_low_i8x16_u'         { V128IExtend I16x8 I8x16 False False }
    | 'i32x4.extend_high_i16x8_s'        { V128IExtend I32x4 I16x8 True True }
    | 'i32x4.extend_high_i16x8_u'        { V128IExtend I32x4 I16x8 True False }
    | 'i32x4.extend_low_i16x8_s'         { V128IExtend I32x4 I16x8 False True }
    | 'i32x4.extend_low_i16x8_u'         { V128IExtend I32x4 I16x8 False False}
    | 'i64x2.extend_high_i32x4_s'        { V128IExtend I64x2 I32x4 True True }
    | 'i64x2.extend_high_i32x4_u'        { V128IExtend I64x2 I32x4 True False }
    | 'i64x2.extend_low_i32x4_s'         { V128IExtend I64x2 I32x4 False True }
    | 'i64x2.extend_low_i32x4_u'         { V128IExtend I64x2 I32x4 False False }

typeuse(next)
    : '(' typeuse1(folded_instr_list(next), instruction_list(next)) {
        let (tu, rest) = $2 in
        let (next, instr) = either id id rest in
        (tu, instr, next)
    }
    | instruction_list(next) { let (next, instr) = $1 in (emptyTypeUse, instr, next) }

typeuse1(close, next)
    : 'type' index ')' typesign(close, next) {
        case $4 of
            (FuncType [] [], rest) -> (IndexedTypeUse $2 Nothing, rest)
            (ft, rest) -> (IndexedTypeUse $2 (Just ft), rest)
    }
    | typesign1(close, next) {
        let (fnType, rest) = $1 in
        (AnonimousTypeUse fnType, rest)
    }

typesign(close, next)
    : '(' typesign1(close, next) { $2 }
    | next { (emptyFuncType, Left $1) }

typesign1(close, next)
    : 'param' list(valtype) ')' typesign(close, next) {
        let (ft, rest) = $4 in
        (mergeFuncType (FuncType (map (ParamType Nothing) $2) []) ft, rest)
    }
    | 'param' ident valtype ')' typesign(close, next) {
         let (ft, rest) = $5 in
        (mergeFuncType (FuncType [ParamType (Just $2) $3] []) ft, rest)
    }
    | typesign_result1(close, next) { $1 }

typesign_result(close, next)
    : '(' typesign_result1(close, next) { $2 }
    | next { (emptyFuncType, Left $1) }

typesign_result1(close, next)
    : 'result' list(valtype) ')' typesign_result(close, next) {
        let (ft, rest) = $4 in
        (mergeFuncType (FuncType [] $2) ft, rest)
    }
    | close { (emptyFuncType, Right $1) }

never : EOF { () }

typedef :: { TypeDef }
    : 'type' opt(ident) functype ')' { TypeDef $2 $3 }

functype :: { FuncType }
    : '(' 'func' typesign(')', ')')  { let (ft, _) = $3 in ft }

memarg1 :: { MemArg }
    : opt(offset) opt(align) {% parseMemArg 1 $1 $2 }

memarg2 :: { MemArg }
    : opt(offset) opt(align) {% parseMemArg 2 $1 $2 }

memarg4 :: { MemArg }
    : opt(offset) opt(align) {% parseMemArg 4 $1 $2 }

memarg8 :: { MemArg }
    : opt(offset) opt(align) {% parseMemArg 8 $1 $2 }

memarg16 :: { MemArg }
    : opt(offset) opt(align) {% parseMemArg 16 $1 $2 }

select_type_or_instructions(terminator)
    : '(' select_type_or_instructions1(terminator) { $2 }
    | instruction_list(terminator) {
        let (end, instr) = $1 in
        (end, Nothing, instr)
    }

select_type_or_instructions1(terminator) 
    : 'result' list(valtype) ')' select_type_or_instructions(terminator) {
        let (end, res, instr) = $4 in
        (end, Just ($2 ++ fromMaybe [] res), instr)
    }
    | folded_instr_list(terminator) {
        let (end, instr) = $1 in
        (end, Nothing, instr)
    }

instruction_list(terminator)
    : terminator { ($1, []) }
    | plaininstr mixed_instruction_list(terminator) { ([PlainInstr $1] ++) `fmap` $2 }
    | 'call_indirect' opt(index) typeuse(terminator) {%
        let tableIdx = fromMaybe (Index 0) $2 in
        let (tu, instr, end) = $3 in
        onlyAnonimParams tu >> (return (end, [PlainInstr $ CallIndirect tableIdx tu] ++ instr))
    }
    | 'select' select_type_or_instructions(terminator) {
        let (end, t, instr) = $2 in (end, [PlainInstr $ Select t] ++ instr)
    }
    | 'block' opt(ident) typeuse('end') opt(ident) mixed_instruction_list(terminator) {% do
        let (tu, instr, _) = $3 
        matchIdents $2 $4
        onlyAnonimParams tu
        return $ ([BlockInstr $2 tu instr] ++) `fmap` $5
    }
    | 'loop' opt(ident) typeuse('end') opt(ident) mixed_instruction_list(terminator) {% do
        let (tu, instr, _) = $3
        matchIdents $2 $4
        onlyAnonimParams tu
        return $ ([LoopInstr $2 tu instr] ++) `fmap` $5
    }
    | 'if' opt(ident) typeuse(if_else) mixed_instruction_list(terminator) {% do
        let (tu, trueBranch, (falseBranch, identAfter)) = $3
        matchIdents $2 identAfter
        onlyAnonimParams tu
        return $ ([IfInstr $2 tu trueBranch falseBranch] ++) `fmap` $4
    }

mixed_instruction_list(terminator)
    : '(' folded_instr1 mixed_instruction_list(terminator) { ($2 ++) `fmap` $3 }
    | instruction_list(terminator) { $1 }

if_else :: { ([Instruction], Maybe Ident) }
    : 'end' opt(ident) { ([], $2) }
    | 'else' opt(ident) mixed_instruction_list('end') opt(ident) {%
        matchIdents $2 $4 >> return (snd $3, if isNothing $2 then $4 else $2)
    }

folded_instr_list(terminator) : folded_instr1 mixed_instruction_list(terminator) { ($1 ++) `fmap` $2 }

folded_instr :: { [Instruction] }
    : '(' folded_instr1 { $2 }

folded_instr1 :: { [Instruction] }
    : plaininstr mixed_instruction_list(')') { snd $2 ++ [PlainInstr $1] }
    | 'call_indirect' opt(index) typeuse(')') {%
        let tableIdx = fromMaybe (Index 0) $2 in
        let (tu, instr, _) = $3 in
        onlyAnonimParams tu >> (return $ instr ++ [PlainInstr $ CallIndirect tableIdx tu])
    }
    | 'select' select_type_or_instructions(')') {
        let (_, t, instr) = $2 in instr ++ [PlainInstr $ Select t]
    }
    | 'block' opt(ident) typeuse(')') {%
        let (typeUse, instr, _) = $3 in
        onlyAnonimParams typeUse >> (return [BlockInstr $2 typeUse instr])
    }
    | 'loop' opt(ident) typeuse(')') {%
        let (typeUse, instr, _) = $3 in
        onlyAnonimParams typeUse >> (return [LoopInstr $2 typeUse instr])
    }
    | 'if' opt(ident) '(' typeuse1(folded_then_else, never) {%
        let (typeUse, Right (pred, (trueBranch, falseBranch))) = $4 in
        onlyAnonimParams typeUse >> (return $ pred ++ [IfInstr $2 typeUse trueBranch falseBranch])
    }

folded_then_else :: { ([Instruction], ([Instruction], [Instruction])) }
    : 'then' mixed_instruction_list(')') folded_else { ([], (snd $2, $3)) }
    | folded_instr1 '(' folded_then_else {
        let (pred, branches) = $3 in
        ($1 ++ pred, branches)
    }

folded_else :: { [Instruction] }
    : ')' { [] }
    | '(' 'else' mixed_instruction_list(')') ')' { snd $3 }

importdesc :: { ImportDesc }
    : 'func' opt(ident) typeuse(')') {
        let (ft, _, _) = $3 in ImportFunc $2 ft
    }
    | 'table' opt(ident) tabletype ')' { ImportTable $2 $3 }
    | 'memory' opt(ident) limits ')' { ImportMemory $2 $3 }
    | 'global' opt(ident) globaltype ')' { ImportGlobal $2 $3 }

import :: { Import }
    : 'import' name name '(' importdesc ')' { Import [] $2 $3 $5 }

-- FUNCTION --
function :: { ModuleField }
    : 'func' opt(ident) export_import_typeuse_locals_body {%
        case $3 $2 of
            mf@(MFFunc fn) -> checkLocalIdentUniqueness fn >> return mf
            mf -> return mf
    }

export_import_typeuse_locals_body :: { Maybe Ident -> ModuleField }
    : instruction_list(')') {
        \i -> MFFunc emptyFunction { ident = i, body = snd $1 }
    }
    | '(' export_import_typeuse_locals_body1 { $2 }

export_import_typeuse_locals_body1 :: { Maybe Ident -> ModuleField }
    : 'export' name ')' export_import_typeuse_locals_body {
        \ident ->
            case $4 ident of
                MFImport imp -> MFImport imp { reExportAs = $2 : reExportAs imp }
                MFFunc func -> MFFunc func { exportFuncAs = $2 : exportFuncAs func }
                _ -> error "unexpected field"
    }
    | import_typeuse_locals_body1 { $1 }

import_typeuse_locals_body1 :: { Maybe Ident -> ModuleField }
    : 'import' name name ')' typeuse(')') {
        let (ft, _, _) = $5 in
        \ident -> MFImport $ Import [] $2 $3 $ ImportFunc ident ft
    }
    | typeuse1(func_mid1, instruction_list(')')) {
        let (funcType, rest) = $1 in
        let (locals, body) = either (\a -> ([], snd a)) id rest in
        \ident -> MFFunc $ emptyFunction { locals, body, ident, funcType }
    }

func_mid :: { ([LocalType], [Instruction]) }
    : instruction_list(')') { ([], snd $1) }
    | '(' func_mid1 { $2 }

func_mid1 :: { ([LocalType], [Instruction]) }
    : 'local' list(valtype) ')' func_mid { (map (LocalType Nothing) $2 ++ fst $4, snd $4) }
    | 'local' ident valtype ')' func_mid { (LocalType (Just $2) $3 : fst $5, snd $5) }
    | folded_instr_list(')') { ([], snd $1) }

-- FUNCTION END --

-- GLOBAL --

global :: { ModuleField }
    : 'global' opt(ident) global_type_export_import { $3 $2 }

globaltype :: { GlobalType }
    : valtype { Const $1 }
    | '(' 'mut' valtype ')' { Mut $3 }

global_type_export_import :: { Maybe Ident -> ModuleField }
    : valtype mixed_instruction_list(')') { \ident -> MFGlobal $ Global [] ident (Const $1) $ snd $2 }
    | '(' global_mut_export_import { $2 }

global_mut_export_import :: { Maybe Ident -> ModuleField }
    : 'mut' valtype ')' mixed_instruction_list(')') { \ident -> MFGlobal $ Global [] ident (Mut $2) $ snd $4 }
    | 'export' name ')' global_type_export_import {
        \ident ->
            case $4 ident of
                MFImport imp -> MFImport imp { reExportAs = $2 : reExportAs imp }
                MFGlobal global -> MFGlobal global { exportGlobalAs = $2 : exportGlobalAs global }
                _ -> error "unexpected field"
    }
    | 'import' name name ')' globaltype ')' {
        \ident -> MFImport $ Import [] $2 $3 $ ImportGlobal ident $5
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
        \ident ->
            case $4 ident of
                [MFImport imp] -> [MFImport imp { reExportAs = $2 : reExportAs imp }]
                (MFMem (Memory exps i l)):rest -> (MFMem (Memory ($2:exps) i l)):rest
                _ -> error "unexpected field"
    }
    | 'import' name name ')' limits ')' {
        \ident -> [MFImport $ Import [] $2 $3 $ ImportMemory ident $5]
    }
    | 'data' datastring ')' ')' {
        \ident ->
            let m = fromIntegral $ LBS.length $2 in
            let lim = if m `mod` 0x10000 == 0 then m `div` 0x10000 else m `div` 0x10000 + 1 in
            -- TODO: unhardcode memory index
            let memIdx = fromMaybe (Index 0) $ Named `fmap` ident in
            [
                MFMem $ Memory [] ident $ Limit lim $ Just lim,
                MFData $ DataSegment Nothing (ActiveData memIdx [PlainInstr $ I32Const 0]) $2
            ]
    }

memory_limits :: { Maybe Ident -> [ModuleField] }
    : limits ')' { \ident -> [MFMem $ Memory [] ident $1] }

-- MEMOTY END --

-- TABLE --
limits :: { Limit }
    : u32 opt(u32) { Limit (fromIntegral $1) (fromIntegral `fmap` $2) }

elemtype :: { ElemType }
    : 'funcref' { FuncRef }
    | 'externref' { ExternRef }

heaptype :: { ElemType }
    : 'func' { FuncRef }
    | 'extern' { ExternRef }

tabletype :: { TableType }
    : limits elemtype { TableType $1 $2 }

table :: { [ModuleField] }
    : 'table' opt(ident) limits_elemtype_elem { $3 $2 }

limits_elemtype_elem :: { Maybe Ident -> [ModuleField] }
    : tabletype ')' { \ident -> [MFTable $ Table [] ident $1] }
    | elemtype '(' 'elem' indexes_or_ref_exprs ')' ')' {
        \ident ->
            let funcsLen = fromIntegral $ length $4 in [
                MFTable $ Table [] ident $ TableType (Limit funcsLen (Just funcsLen)) $1,
                -- TODO: unhardcode table index
                let tableIndex = (fromMaybe (Index 0) $ Named `fmap` ident) in
                let offset = [PlainInstr $ I32Const 0] in
                let elements = $4 in
                MFElem $ ElemSegment Nothing FuncRef (Active tableIndex offset) elements
            ]
    }
    | '(' import_export_table { $2 }

indexes_or_ref_exprs :: {[[Instruction]]}
    : index list(index) { funcIndexToExpr ($1:$2) }
    | elemexpr list(elemexpr) { $1 : $2 }
    | {- empty -} { [] }

import_export_table :: { Maybe Ident -> [ModuleField] }
    : 'import' name name ')' tabletype ')' {
        \ident -> [MFImport $ Import [] $2 $3 $ ImportTable ident $5]
    }
    | 'export' name ')' limits_elemtype_elem {
        \ident ->
            case $4 ident of
                [MFImport imp] -> [MFImport imp { reExportAs = $2 : reExportAs imp }]
                (MFTable (Table exps i t)):rest -> (MFTable (Table ($2:exps) i t)):rest
                _ -> error "unexpected field"
    }

-- TABLE END --

exportdesc :: { ExportDesc }
    : 'func' index ')' { ExportFunc $2 }
    | 'table' index ')' { ExportTable $2 }
    | 'memory' index ')' { ExportMemory $2 }
    | 'global' index ')' { ExportGlobal $2 }

export :: { Export }
    : 'export' name '(' exportdesc ')' { Export $2 $4 }

start :: { StartFunction }
    : 'start' index ')' { StartFunction $2 }

elem :: { ElemSegment }
    : 'elem' opt(ident) elem1 { $3{ ident = $2 } }

elem1 :: { ElemSegment }
    : elemlist ')' { let (t, els) = $1 in ElemSegment Nothing t Passive els }
    | 'declare' elemlist ')' { let (t, els) = $2 in ElemSegment Nothing t Declarative els }
    | '(' elem1_active ')' { $2 }

elem1_active :: { ElemSegment }
    : 'table' index ')' '(' elem1_active_offset {
        let (offset, t, els) = $5 in ElemSegment Nothing t (Active $2 offset) els
    }
    | elem1_active_offset {
        let (offset, t, els) = $1 in ElemSegment Nothing t (Active (Index 0) offset) els
    }

elem1_active_offset :: { ([Instruction], ElemType, [[Instruction]]) }
    : 'offset' mixed_instruction_list(')') elemlist { (snd $2, fst $3, snd $3) }
    | folded_instr1 elemlist { ($1, fst $2, snd $2) }

elemlist :: { (ElemType, [[Instruction]]) }
    : 'func' list(index) { (FuncRef, funcIndexToExpr $2) }
    | 'funcref' list(elemexpr) { (FuncRef, $2) }
    | 'externref' list(elemexpr) { (ExternRef, $2) }
    | list(index) { (FuncRef, funcIndexToExpr $1) }

elemexpr :: { [Instruction] }
    : plaininstr { [PlainInstr $1] }
    | '(' 'item' mixed_instruction_list(')') { snd $3 }
    | '(' folded_instr1 { $2 }

offsetexpr1 :: { [Instruction] }
    : 'offset' mixed_instruction_list(')') { snd $2 }
    | folded_instr1 { $1 }

memory_offsetexpr1 :: { (MemoryIndex, [Instruction]) }
    : offsetexpr1 { (Index 0, $1)}
    | 'memory' index ')' '(' offsetexpr1 { ($2, $5) }

memory_mode :: { DataMode }
    : '(' memory_offsetexpr1 { uncurry ActiveData $2 }
    | {- empty -} { PassiveData }

datasegment :: { DataSegment }
    : 'data' opt(ident) memory_mode datastring ')' { DataSegment $2 $3 $4 }

modulefield1_single :: { ModuleField }
    : typedef { MFType $1 }
    | import { MFImport $1 }
    | export { MFExport $1 }
    | start { MFStart $1 }
    | elem { MFElem $1 }
    | datasegment { MFData $1 }
    | function { $1 }
    | global { $1 }

modulefield1_multi :: { [ModuleField] }
    : table { $1 }
    | memory { $1 }

modulefield1 :: { [ModuleField] }
    : modulefield1_single { [$1] }
    | modulefield1_multi { $1 }

modulefield :: { [ModuleField] }
    : '(' modulefield1 { $2 }

modAsFields :: { [ModuleField] }
    : '(' 'module' list(modulefield) ')' EOF { concat $3 }
    | '(' modulefield1 list(modulefield) EOF { $2 ++ concat $3}

mod :: { S.Module }
    : modAsFields {% desugarize $1 }

-- Wasm Script Extended Grammar
script :: { Script }
    : '(' command1 list(command) EOF { $2 : $3 }
    | '(' modulefield1 list(modulefield) EOF {%
        (\m -> [ModuleDef $ RawModDef Nothing m]) `fmap` (desugarize $ $2 ++ concat $3)
    }

command :: { Command }
    : '(' command1 { $2 }

command1 :: { Command }
    : module1 { ModuleDef $1 }
    | 'register' string opt(ident) ')' { Register $2 $3 }
    | action1 { Action $1 }
    | assertion1 { let (Just (AlexPn _ line _), a) = $1 in Assertion line a }
    | meta1 { Meta $1 }

module1 :: { ModuleDef }
    : 'module' opt(ident) 'binary' datastring ')' { BinaryModDef $2 $4 }
    | 'module' opt(ident) 'quote' list(string) ')' { TextModDef $2 (TL.concat $4) }
    | 'module' opt(ident) list(modulefield) ')' {% RawModDef $2 `fmap` (desugarize $ concat $3) }

action1 :: { Action }
    : 'invoke' opt(ident) string list(folded_instr) ')' {%
        fmap (Invoke $2 $3) $ (mapM (constInstructionToValue . head) $4)
    }
    | 'get' opt(ident) string ')' { Get $2 $3 }

assertion1 :: { (Maybe AlexPosn, Assertion) }
    : 'assert_return' '(' action1 list(folded_instr) ')' {%
        fmap ((\a -> ($1, a)) . AssertReturn $3) $ (mapM (constInstructionToValue . head) $4)
    }
    | 'assert_return_canonical_nan' '(' action1 ')' { ($1, AssertReturnCanonicalNaN $3) }
    | 'assert_return_arithmetic_nan' '(' action1 ')' { ($1, AssertReturnArithmeticNaN $3) }
    | 'assert_trap' '(' assertion_trap string ')' { ($1, AssertTrap $3 $4) }
    | 'assert_malformed' '(' module1 string ')' { ($1, AssertMalformed $3 $4) }
    | 'assert_invalid' '(' module1 string ')' { ($1, AssertInvalid $3 $4) }
    | 'assert_unlinkable' '(' module1 string ')' { ($1, AssertUnlinkable $3 $4) }
    | 'assert_exhaustion' '(' action1 string ')' { ($1, AssertExhaustion $3 $4) }

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

matchIdents :: Maybe Ident -> Maybe Ident -> Either String ()
matchIdents Nothing Nothing = return ()
matchIdents (Just a) (Just b) = if a == b then return () else throwError "mismatching label"
matchIdents Nothing (Just _) = throwError "mismatching label"
matchIdents (Just _) Nothing = return ()

onlyAnonimParams :: TypeUse -> Either String ()
onlyAnonimParams (IndexedTypeUse _ (Just ft)) = onlyAnonimFT ft
onlyAnonimParams (AnonimousTypeUse ft) = onlyAnonimFT ft
onlyAnonimParams _ = return ()

onlyAnonimFT :: FuncType -> Either String ()
onlyAnonimFT (FuncType params _) = mapM_ isAnonim params
    where
        isAnonim ParamType{ ident = Just _ } =
            throwError "only anonimous params allowed in block signatures"
        isAnonim _ = return ()

checkLocalIdentUniqueness :: Function -> Either String Function
checkLocalIdentUniqueness fn@Function { funcType, locals } =
    let ps = case funcType of
            (AnonimousTypeUse ft) -> params ft
            IndexedTypeUse _ ft -> params $ fromMaybe emptyFuncType ft
    in
    let allIdents = (catMaybes $ map (\(LocalType { ident }) -> ident) locals)
            ++ (catMaybes $ map (\(ParamType { ident }) -> ident) ps)
    in
    if nub allIdents == allIdents
    then return fn
    else throwError "duplicate local"

asOffset :: LBS.ByteString -> Maybe Natural
asOffset str = do
    num <- TL.stripPrefix "offset=" $ TLEncoding.decodeUtf8 str
    fromIntegral . fst <$> eitherToMaybe (TLRead.decimal num)

asAlign :: LBS.ByteString -> Maybe Natural
asAlign str = do
    num <- TL.stripPrefix "align=" $ TLEncoding.decodeUtf8 str
    fromIntegral . fst <$> eitherToMaybe (TLRead.decimal num)

parseMemArg :: Natural -> Maybe Natural -> Maybe Natural -> Either String MemArg
parseMemArg defAlign optOffset optAlign = do
    let offset = fromMaybe 0 optOffset
    let parsedAlign = fromIntegral $ fromMaybe defAlign optAlign
    if parsedAlign == 0 then Left "alignment" else return ()
    let align = fromIntegral $ round $ logBase 2 parsedAlign
    if 2 ^ align /= parsedAlign then Left "alignment" else return ()
    if offset >= 2 ^ 32 || align >= 2 ^ 32
    then Left "u32 is out of boundaries"
    else return $ MemArg offset align

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

data FuncType = FuncType { params :: [ParamType], results :: [ValueType] } deriving (Show, Eq)

emptyFuncType :: FuncType
emptyFuncType = FuncType [] []

data ParamType = ParamType {
        ident :: Maybe Ident,
        paramType :: ValueType
    } deriving (Show, Eq)

newtype Ident = Ident TL.Text deriving (Show, Eq)

data Index = Named Ident | Index Natural deriving (Show, Eq)

type LabelIndex = Index
type FuncIndex = Index
type TypeIndex = Index
type LocalIndex = Index
type GlobalIndex = Index
type TableIndex = Index
type MemoryIndex = Index
type ElemIndex = Index
type DataIndex = Index

data V128Rep =
    I8x16Const [Integer]
    | I16x8Const [Integer]
    | I32x4Const [Integer]
    | I64x2Const [Integer]
    | F32x4Const [FloatRep]
    | F64x2Const [FloatRep]
    deriving (Show, Eq)

data PlainInstr =
    -- Control instructions
    Unreachable
    | Nop
    | Br LabelIndex
    | BrIf LabelIndex
    | BrTable [LabelIndex] LabelIndex
    | Return
    | Call FuncIndex
    | CallIndirect TableIndex TypeUse
    -- Reference instructions
    | RefNull ElemType
    | RefIsNull
    | RefFunc FuncIndex
    | RefExtern Natural
    -- Parametric instructions
    | Drop
    | Select (Maybe [ValueType])
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
    | V128Load MemArg
    | V128Load8Lane MemArg Natural
    | V128Load16Lane MemArg Natural
    | V128Load32Lane MemArg Natural
    | V128Load64Lane MemArg Natural
    | V128Load8Splat MemArg
    | V128Load16Splat MemArg
    | V128Load32Splat MemArg
    | V128Load64Splat MemArg
    | V128Load32Zero MemArg
    | V128Load64Zero MemArg
    | V128Load8x8S MemArg
    | V128Load8x8U MemArg
    | V128Load16x4S MemArg
    | V128Load16x4U MemArg
    | V128Load32x2S MemArg
    | V128Load32x2U MemArg
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
    | V128Store MemArg
    | V128Store8Lane MemArg Natural
    | V128Store16Lane MemArg Natural
    | V128Store32Lane MemArg Natural
    | V128Store64Lane MemArg Natural
    | I32Store8 MemArg
    | I32Store16 MemArg
    | I64Store8 MemArg
    | I64Store16 MemArg
    | I64Store32 MemArg
    | MemorySize
    | MemoryGrow
    | MemoryFill
    | MemoryCopy
    | MemoryInit DataIndex
    | DataDrop DataIndex
    -- Table instructions
    | TableInit TableIndex ElemIndex
    | TableGrow TableIndex
    | TableSize TableIndex
    | TableFill TableIndex
    | TableGet TableIndex
    | TableSet TableIndex
    | TableCopy TableIndex TableIndex
    | ElemDrop ElemIndex
    -- Numeric instructions
    | I32Const Integer
    | I64Const Integer
    | F32Const FloatRep
    | F64Const FloatRep
    | V128Const V128Rep
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
    | ITruncSatFU {- Int Size -} BitSize {- Float Size -} BitSize
    | ITruncSatFS {- Int Size -} BitSize {- Float Size -} BitSize
    | I64ExtendSI32
    | I64ExtendUI32
    | FConvertIU {- Float Size -} BitSize {- Int Size -} BitSize
    | FConvertIS {- Float Size -} BitSize {- Int Size -} BitSize
    | F32DemoteF64
    | F64PromoteF32
    | IReinterpretF BitSize
    | FReinterpretI BitSize
    -- Vector instructions
    | V128Splat SimdShape
    | V128ExtractLane SimdShape Natural Bool
    | V128ReplaceLane SimdShape Natural
    | V128AllTrue SimdShape
    | V128BitMask SimdShape
    | V128AnyTrue
    | V128BitSelect
    | I8x16Shuffle [Int]
    | I8x16Swizzle
    | V128Narrow SimdShape SimdShape Bool
    | F64x2PromoteLowF32x4
    | F32x4DemoteF64x2Zero
    | V128IExtend SimdShape SimdShape {- high -} Bool {- signed -} Bool
    deriving (Show, Eq)

data TypeDef = TypeDef (Maybe Ident) FuncType deriving (Show, Eq)

data TypeUse =
    IndexedTypeUse TypeIndex (Maybe FuncType)
    | AnonimousTypeUse FuncType
    deriving (Show, Eq)

emptyTypeUse = AnonimousTypeUse emptyFuncType

data Instruction =
    PlainInstr PlainInstr
    | BlockInstr {
        label :: Maybe Ident,
        blockType :: TypeUse,
        body :: [Instruction]
    }
    | LoopInstr {
        label :: Maybe Ident,
        blockType :: TypeUse,
        body :: [Instruction]
    }
    | IfInstr {
        label :: Maybe Ident,
        blockType :: TypeUse,
        trueBranch :: [Instruction],
        falseBranch :: [Instruction]
    }
    deriving (Show, Eq)

data Import = Import {
        reExportAs :: [TL.Text],
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
        exportFuncAs :: [TL.Text],
        ident :: Maybe Ident,
        funcType :: TypeUse,
        locals :: [LocalType],
        body :: [Instruction]
    }
    deriving (Show, Eq)

emptyFunction :: Function
emptyFunction =
    Function {
        exportFuncAs = [],
        ident = Nothing,
        funcType = AnonimousTypeUse emptyFuncType,
        locals = [],
        body = []
    }

data Global = Global {
        exportGlobalAs :: [TL.Text],
        ident :: Maybe Ident,
        globalType :: GlobalType,
        initializer :: [Instruction]
    }
    deriving (Show, Eq)

data Memory = Memory [TL.Text] (Maybe Ident) Limit deriving (Show, Eq)

data Table = Table [TL.Text] (Maybe Ident) TableType deriving (Show, Eq)

data ExportDesc =
    ExportFunc FuncIndex
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

data ElemMode
    = Passive
    | Active TableIndex [Instruction]
    | Declarative
    deriving (Show, Eq)

data ElemSegment = ElemSegment {
        ident :: Maybe Ident,
        elemType :: ElemType,
        mode :: ElemMode,
        elements :: [[Instruction]]
    }
    deriving (Show, Eq)

data DataMode =
    PassiveData
    | ActiveData MemoryIndex [Instruction]
    deriving (Show, Eq)

data DataSegment = DataSegment {
        ident :: Maybe Ident,
        dataMode :: DataMode,
        datastring :: LBS.ByteString
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

happyError (Lexeme _ EOF : []) = Left $ "Error occuried during parsing phase at the end of file"
happyError (Lexeme Nothing tok : tokens) = Left $ "Error occuried during parsing phase at the end of file"
happyError (Lexeme (Just (AlexPn abs line col)) tok : tokens) = Left $
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

data ModuleDef
    = RawModDef (Maybe Ident) S.Module
    | TextModDef (Maybe Ident) TL.Text
    | BinaryModDef (Maybe Ident) LBS.ByteString
    deriving (Show, Eq)

data Command
    = ModuleDef ModuleDef
    | Register TL.Text (Maybe Ident)
    | Action Action
    | Assertion Int Assertion
    | Meta Meta
    deriving (Show, Eq)

data ValuePattern =
    ExactValue (S.Instruction Natural)
    | CanonicalNan
    | ArithmeticNan
    | VectorPat SimdShape [ValuePattern]
    deriving (Show, Eq)

data Action
    = Invoke (Maybe Ident) TL.Text [ValuePattern]
    | Get (Maybe Ident) TL.Text
    deriving (Show, Eq)

type FailureString = TL.Text

data Assertion
    = AssertReturn Action [ValuePattern]
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

v128RepToBytes :: V128Rep -> Either String ByteArray.ByteArray
v128RepToBytes (I8x16Const bytes) =
    return $ ByteArray.byteArrayFromListN 16 $ (fromIntegral :: Integer -> Word8) <$> bytes
v128RepToBytes (I16x8Const words) =
    return $ ByteArray.byteArrayFromListN 8 $ (fromIntegral :: Integer -> Word16) <$> words
v128RepToBytes (I32x4Const dwords) =
    return $ ByteArray.byteArrayFromListN 4 $ integerToWord32 <$> dwords
v128RepToBytes (I64x2Const qwords) =
    return $ ByteArray.byteArrayFromListN 2 $ integerToWord64 <$> qwords
v128RepToBytes (F32x4Const floats) =
    ByteArray.byteArrayFromListN 4 <$> mapM (fmap floatToWord . asFloat) floats
v128RepToBytes (F64x2Const doubles) =
    ByteArray.byteArrayFromListN 2 <$> mapM (fmap doubleToWord . asDouble) doubles

isNaNRep :: FloatRep -> Bool
isNaNRep (NanRep Canonical) = True
isNaNRep (NanRep Arithmetic) = True
isNaNRep _ = False

constInstructionToValue :: Instruction -> Either String ValuePattern
constInstructionToValue (PlainInstr (I32Const v)) = return $ ExactValue $ S.I32Const $ integerToWord32 v
constInstructionToValue (PlainInstr (F32Const (NanRep Canonical))) = return CanonicalNan
constInstructionToValue (PlainInstr (F32Const (NanRep Arithmetic))) = return ArithmeticNan
constInstructionToValue (PlainInstr (F32Const v)) = ExactValue . S.F32Const <$> asFloat v
constInstructionToValue (PlainInstr (I64Const v)) = return $ ExactValue $ S.I64Const $ integerToWord64 v
constInstructionToValue (PlainInstr (F64Const (NanRep Canonical))) = return CanonicalNan
constInstructionToValue (PlainInstr (F64Const (NanRep Arithmetic))) = return ArithmeticNan
constInstructionToValue (PlainInstr (F64Const v)) = ExactValue . S.F64Const <$> asDouble v
constInstructionToValue (PlainInstr (V128Const (F32x4Const floats))) | any isNaNRep floats =
    VectorPat F32x4 <$> mapM (constInstructionToValue . PlainInstr . F32Const) floats
constInstructionToValue (PlainInstr (V128Const (F64x2Const floats))) | any isNaNRep floats =
    VectorPat F64x2 <$> mapM (constInstructionToValue . PlainInstr . F64Const) floats
constInstructionToValue (PlainInstr (V128Const v)) = ExactValue . S.V128Const <$> v128RepToBytes v
constInstructionToValue (PlainInstr (RefNull et)) = return $ ExactValue $ S.RefNull et
constInstructionToValue (PlainInstr (RefExtern n)) = return $ ExactValue $ S.RefExtern n
constInstructionToValue _ = Left "Only const instructions supported as arguments for actions"

funcIndexToExpr :: [FuncIndex] -> [[Instruction]]
funcIndexToExpr = map $ (:[]) . PlainInstr . RefFunc

desugarize :: [ModuleField] -> Either String S.Module
desugarize fields = do
    checkImportsOrder fields
    checkStartCount fields
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
    }
    funs <- mapM (synFunctionToStruct mod) $ functions mod
    elements <- mapM (synElemToStruct mod) $ elems mod
    segments <- mapM (synDataToStruct mod) $ datas mod
    globs <- mapM (synGlobalToStruct mod) $ globals mod
    checkFuncIdentsUniqueness mod
    checkTableIdentsUniqueness mod
    checkMemoryIdentsUniqueness mod
    checkGlobalIdentsUniqueness mod
    return S.Module {
        S.types = map synTypeDefToStruct $ types mod,
        S.functions = funs,
        S.tables = map synTableToStruct $ tables mod,
        S.imports = map (synImportToStruct $ types mod) $ imports mod,
        S.elems = elements,
        S.datas = segments,
        S.mems = map synMemoryToStruct $ mems mod,
        S.globals = globs,
        S.start = fmap (synStartToStruct mod) $ start mod,
        S.exports = synExportsToStruct mod $ extractExports mod fields
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
        
        checkImportsOrder :: [ModuleField] -> Either String ()
        checkImportsOrder fields = foldM checkDef False fields >> return ()
            where
                checkDef nonImportOccured (MFImport _) =
                    if nonImportOccured
                    then Left "Import sections have to be before any definition"
                    else Right False
                checkDef _ (MFFunc _) = return True
                checkDef _ (MFGlobal _) = return True
                checkDef _ (MFMem _) = return True
                checkDef _ (MFTable _) = return True
                checkDef nonImportOccured _ = return nonImportOccured
        
        checkStartCount :: [ModuleField] -> Either String ()
        checkStartCount fields = foldM checkDef False fields >> return ()
            where
                checkDef startOccured (MFStart _) =
                    if startOccured
                    then Left "Multiple start sections"
                    else Right True
                checkDef startOccured _ = return startOccured

        extractTypeDef :: [TypeDef] -> ModuleField -> [TypeDef]
        extractTypeDef defs (MFType _) = defs -- should be extracted before implicit defs
        extractTypeDef defs (MFImport Import { desc = ImportFunc _ typeUse }) =
            matchTypeUse defs typeUse
        extractTypeDef defs (MFFunc Function { funcType, body }) =
            extractTypeDefFromInstructions (matchTypeUse defs funcType) body
        extractTypeDef defs (MFGlobal Global { initializer }) =
            extractTypeDefFromInstructions defs initializer
        extractTypeDef defs _ = defs

        extractTypeDefFromInstructions :: [TypeDef] -> [Instruction] -> [TypeDef]
        extractTypeDefFromInstructions = foldl' extractTypeDefFromInstruction

        extractTypeDefFromInstruction :: [TypeDef] -> Instruction -> [TypeDef]
        extractTypeDefFromInstruction defs (PlainInstr (CallIndirect _ typeUse)) =
            matchTypeUse defs typeUse
        extractTypeDefFromInstruction defs (BlockInstr { body, blockType }) =
            extractTypeDefFromInstructions (matchTypeUse defs blockType) body
        extractTypeDefFromInstruction defs (LoopInstr { body, blockType }) =
            extractTypeDefFromInstructions (matchTypeUse defs blockType) body
        extractTypeDefFromInstruction defs (IfInstr { blockType, trueBranch, falseBranch }) =
            extractTypeDefFromInstructions (matchTypeUse defs blockType) $ trueBranch ++ falseBranch
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

        nth :: Natural -> [a] -> Maybe a
        nth 0 (x : xs) = Just x
        nth n (_ : xs) = nth (n - 1) xs
        nth _ _ = Nothing

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
            def <- nth n defs
            guard $ matchTypeFunc funcType def
            return n
        getTypeIndex defs (IndexedTypeUse (Index n) Nothing) = return n
        
        -- imports
        synImportToStruct :: [TypeDef] -> Import -> S.Import
        synImportToStruct defs (Import _ mod name (ImportFunc _ typeUse)) =
            case getTypeIndex defs typeUse of
                Just idx -> S.Import mod name $ S.ImportFunc idx
                Nothing -> error $ "cannot find type index for function import: " ++ show typeUse
        synImportToStruct _ (Import _ mod name (ImportTable _ tableType)) =
            S.Import mod name $ S.ImportTable tableType
        synImportToStruct _ (Import _ mod name (ImportMemory _ limit)) =
            S.Import mod name $ S.ImportMemory limit
        synImportToStruct _ (Import _ mod name (ImportGlobal _ globalType)) =
            S.Import mod name $ S.ImportGlobal globalType

        extractImport :: [Import] -> ModuleField -> [Import]
        extractImport imports (MFImport imp) = imp : imports
        extractImport imports _ = imports

        unwrapLabel ctx labelIdx =
            case getLabelIdx ctx labelIdx of
                Just i -> Right i
                Nothing -> Left "unknown label"

        -- functions
        synInstrToStruct :: FunCtx -> Instruction -> Either String (S.Instruction Natural)
        synInstrToStruct _ (PlainInstr Unreachable) = return S.Unreachable
        synInstrToStruct _ (PlainInstr Nop) = return S.Nop
        synInstrToStruct ctx (PlainInstr (Br labelIdx)) =
            S.Br <$> unwrapLabel ctx labelIdx
        synInstrToStruct ctx (PlainInstr (BrIf labelIdx)) =
            S.BrIf <$> unwrapLabel ctx labelIdx
        synInstrToStruct ctx (PlainInstr (BrTable lbls lbl)) = do
            labels <- mapM (unwrapLabel ctx) lbls
            S.BrTable labels <$> unwrapLabel ctx lbl
        synInstrToStruct _ (PlainInstr Return) = return S.Return
        synInstrToStruct FunCtx { ctxMod } (PlainInstr (Call funIdx)) =
            case getFuncIndex ctxMod funIdx of
                Just idx -> return $ S.Call idx
                Nothing -> Left "unknown function"
        synInstrToStruct FunCtx { ctxMod } (PlainInstr (CallIndirect tableIdx typeUse)) =
            case getTableIndex ctxMod tableIdx of
                Just tableIdx ->
                    case getTypeIndex (types ctxMod) typeUse of
                        Just idx -> return $ S.CallIndirect tableIdx idx
                        Nothing -> Left "unknown type"
                Nothing -> Left "unknown table"
        synInstrToStruct _ (PlainInstr Drop) = return $ S.Drop
        synInstrToStruct _ (PlainInstr (Select vt)) = return $ S.Select vt
        synInstrToStruct _ (PlainInstr (RefNull elType)) = return $ S.RefNull elType
        synInstrToStruct _ (PlainInstr RefIsNull) = return $ S.RefIsNull
        synInstrToStruct FunCtx { ctxMod } (PlainInstr (RefFunc funIdx)) =
            case getFuncIndex ctxMod funIdx of
                Just idx -> return $ S.RefFunc idx
                Nothing -> Left "unknown function"
        synInstrToStruct FunCtx { ctxMod } (PlainInstr (RefExtern idx)) =
            return $ S.RefExtern idx
        synInstrToStruct ctx (PlainInstr (GetLocal localIdx)) =
            case getLocalIndex ctx localIdx of
                Just idx -> return $ S.GetLocal idx
                Nothing -> Left "unknown local"
        synInstrToStruct ctx (PlainInstr (SetLocal localIdx)) =
            case getLocalIndex ctx localIdx of
                Just idx -> return $ S.SetLocal idx
                Nothing -> Left "unknown local"
        synInstrToStruct ctx (PlainInstr (TeeLocal localIdx)) =
            case getLocalIndex ctx localIdx of
                Just idx -> return $ S.TeeLocal idx
                Nothing -> Left "unknown local"
        synInstrToStruct FunCtx { ctxMod } (PlainInstr (GetGlobal globalIdx)) =
            case getGlobalIndex ctxMod globalIdx of
                Just idx -> return $ S.GetGlobal idx
                Nothing -> Left "unknown global"
        synInstrToStruct FunCtx { ctxMod } (PlainInstr (SetGlobal globalIdx)) =
            case getGlobalIndex ctxMod globalIdx of
                Just idx -> return $ S.SetGlobal idx
                Nothing -> Left "unknown global"
        synInstrToStruct _ (PlainInstr (I32Load memArg)) = return $ S.I32Load memArg
        synInstrToStruct _ (PlainInstr (I64Load memArg)) = return $ S.I64Load memArg
        synInstrToStruct _ (PlainInstr (F32Load memArg)) = return $ S.F32Load memArg
        synInstrToStruct _ (PlainInstr (F64Load memArg)) = return $ S.F64Load memArg
        synInstrToStruct _ (PlainInstr (V128Load memArg)) = return $ S.V128Load memArg
        synInstrToStruct _ (PlainInstr (V128Load8Lane memArg idx)) = return $ S.V128Load8Lane memArg idx
        synInstrToStruct _ (PlainInstr (V128Load16Lane memArg idx)) = return $ S.V128Load16Lane memArg idx
        synInstrToStruct _ (PlainInstr (V128Load32Lane memArg idx)) = return $ S.V128Load32Lane memArg idx
        synInstrToStruct _ (PlainInstr (V128Load64Lane memArg idx)) = return $ S.V128Load64Lane memArg idx
        synInstrToStruct _ (PlainInstr (V128Load8Splat memArg)) = return $ S.V128Load8Splat memArg
        synInstrToStruct _ (PlainInstr (V128Load16Splat memArg)) = return $ S.V128Load16Splat memArg
        synInstrToStruct _ (PlainInstr (V128Load32Splat memArg)) = return $ S.V128Load32Splat memArg
        synInstrToStruct _ (PlainInstr (V128Load64Splat memArg)) = return $ S.V128Load64Splat memArg
        synInstrToStruct _ (PlainInstr (V128Load32Zero memArg)) = return $ S.V128Load32Zero memArg
        synInstrToStruct _ (PlainInstr (V128Load64Zero memArg)) = return $ S.V128Load64Zero memArg
        synInstrToStruct _ (PlainInstr (V128Load8x8S memArg)) = return $ S.V128Load8x8S memArg
        synInstrToStruct _ (PlainInstr (V128Load8x8U memArg)) = return $ S.V128Load8x8U memArg
        synInstrToStruct _ (PlainInstr (V128Load16x4S memArg)) = return $ S.V128Load16x4S memArg
        synInstrToStruct _ (PlainInstr (V128Load16x4U memArg)) = return $ S.V128Load16x4U memArg
        synInstrToStruct _ (PlainInstr (V128Load32x2S memArg)) = return $ S.V128Load32x2S memArg
        synInstrToStruct _ (PlainInstr (V128Load32x2U memArg)) = return $ S.V128Load32x2U memArg
        synInstrToStruct _ (PlainInstr (I32Load8S memArg)) = return $ S.I32Load8S memArg
        synInstrToStruct _ (PlainInstr (I32Load8U memArg)) = return $ S.I32Load8U memArg
        synInstrToStruct _ (PlainInstr (I32Load16S memArg)) = return $ S.I32Load16S memArg
        synInstrToStruct _ (PlainInstr (I32Load16U memArg)) = return $ S.I32Load16U memArg
        synInstrToStruct _ (PlainInstr (I64Load8S memArg)) = return $ S.I64Load8S memArg
        synInstrToStruct _ (PlainInstr (I64Load8U memArg)) = return $ S.I64Load8U memArg
        synInstrToStruct _ (PlainInstr (I64Load16S memArg)) = return $ S.I64Load16S memArg
        synInstrToStruct _ (PlainInstr (I64Load16U memArg)) = return $ S.I64Load16U memArg
        synInstrToStruct _ (PlainInstr (I64Load32S memArg)) = return $ S.I64Load32S memArg
        synInstrToStruct _ (PlainInstr (I64Load32U memArg)) = return $ S.I64Load32U memArg
        synInstrToStruct _ (PlainInstr (I32Store memArg)) = return $ S.I32Store memArg
        synInstrToStruct _ (PlainInstr (I64Store memArg)) = return $ S.I64Store memArg
        synInstrToStruct _ (PlainInstr (F32Store memArg)) = return $ S.F32Store memArg
        synInstrToStruct _ (PlainInstr (F64Store memArg)) = return $ S.F64Store memArg
        synInstrToStruct _ (PlainInstr (V128Store memArg)) = return $ S.V128Store memArg
        synInstrToStruct _ (PlainInstr (V128Store8Lane memArg idx)) = return $ S.V128Store8Lane memArg idx
        synInstrToStruct _ (PlainInstr (V128Store16Lane memArg idx)) = return $ S.V128Store16Lane memArg idx
        synInstrToStruct _ (PlainInstr (V128Store32Lane memArg idx)) = return $ S.V128Store32Lane memArg idx
        synInstrToStruct _ (PlainInstr (V128Store64Lane memArg idx)) = return $ S.V128Store64Lane memArg idx
        synInstrToStruct _ (PlainInstr (I32Store8 memArg)) = return $ S.I32Store8 memArg
        synInstrToStruct _ (PlainInstr (I32Store16 memArg)) = return $ S.I32Store16 memArg
        synInstrToStruct _ (PlainInstr (I64Store8 memArg)) = return $ S.I64Store8 memArg
        synInstrToStruct _ (PlainInstr (I64Store16 memArg)) = return $ S.I64Store16 memArg
        synInstrToStruct _ (PlainInstr (I64Store32 memArg)) = return $ S.I64Store32 memArg
        synInstrToStruct _ (PlainInstr MemorySize) = return $ S.MemorySize
        synInstrToStruct _ (PlainInstr MemoryGrow) = return $ S.MemoryGrow
        synInstrToStruct _ (PlainInstr MemoryFill) = return $ S.MemoryFill
        synInstrToStruct _ (PlainInstr MemoryCopy) = return $ S.MemoryCopy
        synInstrToStruct FunCtx { ctxMod } (PlainInstr (MemoryInit dataIdx)) =
            case getDataIndex ctxMod dataIdx of
                Just dataIdx -> return $ S.MemoryInit dataIdx
                Nothing -> Left "unknown data"
        synInstrToStruct FunCtx { ctxMod } (PlainInstr (DataDrop dataIdx)) =
            case getDataIndex ctxMod dataIdx of
                Just dataIdx -> return $ S.DataDrop dataIdx
                Nothing -> Left "unknown data"
        synInstrToStruct FunCtx { ctxMod } (PlainInstr (TableInit tableIdx elemIdx)) =
            case getTableIndex ctxMod tableIdx of
                Just tableIdx ->
                    case getElemIndex ctxMod elemIdx of
                        Just elemIdx -> return $ S.TableInit tableIdx elemIdx
                        Nothing -> Left "unknown elem"
                Nothing -> Left "unknown table"
        synInstrToStruct FunCtx { ctxMod } (PlainInstr (TableCopy toIdx fromIdx)) =
            case getTableIndex ctxMod fromIdx of
                Just fromIdx ->
                    case getTableIndex ctxMod toIdx of
                        Just toIdx -> return $ S.TableCopy toIdx fromIdx
                        Nothing -> Left "unknown table"
                Nothing -> Left "unknown table"
        synInstrToStruct FunCtx { ctxMod } (PlainInstr (TableSize tableIdx)) =
            case getTableIndex ctxMod tableIdx of
                Just tableIdx -> return $ S.TableSize tableIdx
                Nothing -> Left "unknown table"
        synInstrToStruct FunCtx { ctxMod } (PlainInstr (TableFill tableIdx)) =
            case getTableIndex ctxMod tableIdx of
                Just tableIdx -> return $ S.TableFill tableIdx
                Nothing -> Left "unknown table"
        synInstrToStruct FunCtx { ctxMod } (PlainInstr (TableGrow tableIdx)) =
            case getTableIndex ctxMod tableIdx of
                Just tableIdx -> return $ S.TableGrow tableIdx
                Nothing -> Left "unknown table"
        synInstrToStruct FunCtx { ctxMod } (PlainInstr (TableSet tableIdx)) =
            case getTableIndex ctxMod tableIdx of
                Just tableIdx -> return $ S.TableSet tableIdx
                Nothing -> Left "unknown table"
        synInstrToStruct FunCtx { ctxMod } (PlainInstr (TableGet tableIdx)) =
            case getTableIndex ctxMod tableIdx of
                Just tableIdx -> return $ S.TableGet tableIdx
                Nothing -> Left "unknown table"
        synInstrToStruct FunCtx { ctxMod } (PlainInstr (ElemDrop elemIdx)) =
            case getElemIndex ctxMod elemIdx of
                Just elemIdx -> return $ S.ElemDrop elemIdx
                Nothing -> Left "unknown elem"
        synInstrToStruct _ (PlainInstr (I32Const val)) = return $ S.I32Const $ integerToWord32 val
        synInstrToStruct _ (PlainInstr (I64Const val)) = return $ S.I64Const $ integerToWord64 val
        synInstrToStruct _ (PlainInstr (F32Const (NanRep Arithmetic))) =
            Left "arithmetic nan constant allowed only in script"
        synInstrToStruct _ (PlainInstr (F32Const (NanRep Canonical))) =
            Left "canonical nan constant allowed only in script"
        synInstrToStruct _ (PlainInstr (F32Const rep)) = S.F32Const <$> asFloat rep
        synInstrToStruct _ (PlainInstr (F64Const (NanRep Arithmetic))) =
            Left "arithmetic nan constant allowed only in script"
        synInstrToStruct _ (PlainInstr (F64Const (NanRep Canonical))) =
            Left "canonical nan constant allowed only in script"
        synInstrToStruct _ (PlainInstr (F64Const rep)) = S.F64Const <$> asDouble rep
        synInstrToStruct _ (PlainInstr (V128Const rep)) = S.V128Const <$> v128RepToBytes rep
        synInstrToStruct _ (PlainInstr (IUnOp sz op)) = return $ S.IUnOp sz op
        synInstrToStruct _ (PlainInstr (IBinOp sz op)) = return $ S.IBinOp sz op
        synInstrToStruct _ (PlainInstr I32Eqz) = return $ S.I32Eqz
        synInstrToStruct _ (PlainInstr I64Eqz) = return $ S.I64Eqz
        synInstrToStruct _ (PlainInstr (IRelOp sz op)) = return $ S.IRelOp sz op
        synInstrToStruct _ (PlainInstr (FUnOp sz op)) = return $ S.FUnOp sz op
        synInstrToStruct _ (PlainInstr (FBinOp sz op)) = return $ S.FBinOp sz op
        synInstrToStruct _ (PlainInstr (FRelOp sz op)) = return $ S.FRelOp sz op
        synInstrToStruct _ (PlainInstr I32WrapI64) = return $ S.I32WrapI64
        synInstrToStruct _ (PlainInstr (ITruncFU sz sz')) = return $ S.ITruncFU sz sz'
        synInstrToStruct _ (PlainInstr (ITruncFS sz sz')) = return $ S.ITruncFS sz sz'
        synInstrToStruct _ (PlainInstr (ITruncSatFU sz sz')) = return $ S.ITruncSatFU sz sz'
        synInstrToStruct _ (PlainInstr (ITruncSatFS sz sz')) = return $ S.ITruncSatFS sz sz'
        synInstrToStruct _ (PlainInstr I64ExtendSI32) = return $ S.I64ExtendSI32
        synInstrToStruct _ (PlainInstr I64ExtendUI32) = return $ S.I64ExtendUI32
        synInstrToStruct _ (PlainInstr (FConvertIU sz sz')) = return $ S.FConvertIU sz sz'
        synInstrToStruct _ (PlainInstr (FConvertIS sz sz')) = return $ S.FConvertIS sz sz'
        synInstrToStruct _ (PlainInstr F32DemoteF64) = return $ S.F32DemoteF64
        synInstrToStruct _ (PlainInstr F64PromoteF32) = return $ S.F64PromoteF32
        synInstrToStruct _ (PlainInstr (IReinterpretF sz)) = return $ S.IReinterpretF sz
        synInstrToStruct _ (PlainInstr (FReinterpretI sz)) = return $ S.FReinterpretI sz
        synInstrToStruct _ (PlainInstr (V128Splat shape)) = return $ S.V128Splat shape
        synInstrToStruct _ (PlainInstr (V128ExtractLane shape idx sign)) = return $ S.V128ExtractLane shape idx sign
        synInstrToStruct _ (PlainInstr (V128ReplaceLane shape idx)) = return $ S.V128ReplaceLane shape idx
        synInstrToStruct _ (PlainInstr (V128AllTrue shape)) = return $ S.V128AllTrue shape
        synInstrToStruct _ (PlainInstr (V128BitMask shape)) = return $ S.V128BitMask shape
        synInstrToStruct _ (PlainInstr V128AnyTrue) = return $ S.V128AnyTrue
        synInstrToStruct _ (PlainInstr V128BitSelect) = return $ S.V128BitSelect
        synInstrToStruct _ (PlainInstr (I8x16Shuffle idxs)) = return $ S.I8x16Shuffle idxs
        synInstrToStruct _ (PlainInstr I8x16Swizzle) = return $ S.I8x16Swizzle
        synInstrToStruct _ (PlainInstr F64x2PromoteLowF32x4) = return $ S.F64x2PromoteLowF32x4
        synInstrToStruct _ (PlainInstr F32x4DemoteF64x2Zero) = return $ S.F32x4DemoteF64x2Zero
        synInstrToStruct _ (PlainInstr (V128Narrow t f s)) = return $ S.V128Narrow t f s
        synInstrToStruct _ (PlainInstr (V128IExtend t f h s)) = return $ S.V128IExtend t f h s
        synInstrToStruct ctx@FunCtx { ctxMod = Module { types } } BlockInstr {label, blockType, body} = do
            let ctx' = ctx { ctxLabels = label : ctxLabels ctx }
            bt <- case blockType of
                AnonimousTypeUse (FuncType [] []) -> return $ S.Inline Nothing
                AnonimousTypeUse (FuncType [] [vt]) -> return $ S.Inline (Just vt)
                typed -> case getTypeIndex types typed of
                    Just idx -> return $ S.TypeIndex idx
                    Nothing -> Left "unknown type"
            S.Block bt <$> mapM (synInstrToStruct ctx') body
        synInstrToStruct ctx@FunCtx { ctxMod = Module { types } } LoopInstr {label, blockType, body} = do
            let ctx' = ctx { ctxLabels = label : ctxLabels ctx }
            bt <- case blockType of
                AnonimousTypeUse (FuncType [] []) -> return $ S.Inline Nothing
                AnonimousTypeUse (FuncType [] [vt]) -> return $ S.Inline (Just vt)
                typed -> case getTypeIndex types typed of
                    Just idx -> return $ S.TypeIndex idx
                    Nothing -> Left "unknown type"
            S.Loop bt <$> mapM (synInstrToStruct ctx') body
        synInstrToStruct ctx@FunCtx { ctxMod = Module { types } } IfInstr {label, blockType, trueBranch, falseBranch} = do
            let ctx' = ctx { ctxLabels = label : ctxLabels ctx }
            bt <- case blockType of
                AnonimousTypeUse (FuncType [] []) -> return $ S.Inline Nothing
                AnonimousTypeUse (FuncType [] [vt]) -> return $ S.Inline (Just vt)
                typed -> case getTypeIndex types typed of
                    Just idx -> return $ S.TypeIndex idx
                    Nothing -> Left "unknown type"
            trueBranch' <- mapM (synInstrToStruct ctx') trueBranch
            falseBranch' <- mapM (synInstrToStruct ctx') falseBranch
            return $ S.If bt trueBranch' falseBranch'
        
        synFunctionToStruct :: Module -> Function -> Either String S.Function
        synFunctionToStruct mod Function { funcType, locals, body } = do
            typeIdx <- (
                    case getTypeIndex (types mod) funcType of
                        Just idx -> Right idx
                        Nothing -> Left "Type was not found or type signature doesn't match with type"
                )
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
            let ctx = FunCtx mod [] locals params
            instructions <- mapM (synInstrToStruct ctx) body
            return S.Function {
                S.funcType = typeIdx,
                S.localTypes = map localType locals,
                S.body = instructions
            }

        extractFunction :: [Function] -> ModuleField -> [Function]
        extractFunction funcs (MFFunc fun) = fun : funcs
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

        checkFuncIdentsUniqueness :: Module -> Either String ()
        checkFuncIdentsUniqueness m@Module { imports, functions } = do
            mapM_ checkImportUniqueness $ filter isFuncImport imports
            mapM_ checkFuncUniqueness functions
            where
                checkImportUniqueness Import { desc = ImportFunc (Just id) _ } =
                    if length (getFuncIndexes m id) > 1
                    then throwError "duplicate func"
                    else return ()
                checkImportUniqueness _ = return ()

                checkFuncUniqueness Function { ident = Just id } =
                    if length (getFuncIndexes m id) > 1
                    then throwError "duplicate func"
                    else return ()
                checkFuncUniqueness _ = return ()

        getFuncIndexes :: Module -> Ident -> [Natural]
        getFuncIndexes Module { imports, functions } id =
            let funcImports = zip [0..] $ filter isFuncImport imports in
            let importIndexes = map fst $ filter (\(_, Import { desc = ImportFunc ident _ }) -> ident == Just id) funcImports in
            let isIdent (_, Function { ident }) = ident == Just id in
            let funcIndexes = map fst $ filter isIdent $ zip [length funcImports..] functions in
            map fromIntegral $ importIndexes ++ funcIndexes

        getFuncIndex :: Module -> FuncIndex -> Maybe Natural
        getFuncIndex mod (Named id) =
            case getFuncIndexes mod id of
                [idx] -> return idx
                _ -> Nothing
        getFuncIndex _ (Index idx) = Just idx

        -- tables
        synTableToStruct :: Table -> S.Table
        synTableToStruct (Table _ _ tableType) = S.Table tableType

        checkTableIdentsUniqueness :: Module -> Either String ()
        checkTableIdentsUniqueness m@Module { imports, tables } = do
            mapM_ checkImportUniqueness $ filter isTableImport imports
            mapM_ checkTableUniqueness tables
            where
                checkImportUniqueness Import { desc = ImportTable (Just id) _ } =
                    if length (getTableIndexes m id) > 1
                    then throwError "duplicate table"
                    else return ()
                checkImportUniqueness _ = return ()

                checkTableUniqueness (Table _ (Just id) _) =
                    if length (getTableIndexes m id) > 1
                    then throwError "duplicate table"
                    else return ()
                checkTableUniqueness _ = return ()

        extractTable :: [Table] -> ModuleField -> [Table]
        extractTable tables (MFTable table) = table : tables
        extractTable tables _ = tables

        isTableImport :: Import -> Bool
        isTableImport Import { desc = ImportTable _ _ } = True
        isTableImport _ = False

        getTableIndexes :: Module -> Ident -> [Natural]
        getTableIndexes Module { imports, tables } id =
            let tableImports = zip [0..] $ filter isTableImport imports in
            let importIndexes = map fst $ filter (\(_, Import { desc = ImportTable ident _ }) -> ident == Just id) tableImports in
            let isIdent (_, (Table _ ident _)) = ident == Just id in
            let tableIndexes = map fst $ filter isIdent $ zip [length tableImports..] tables in
            map fromIntegral $ importIndexes ++ tableIndexes

        getTableIndex :: Module -> TableIndex -> Maybe Natural
        getTableIndex mod (Named id) =
            case getTableIndexes mod id of
                [idx] -> return idx
                _ -> Nothing
        getTableIndex _ (Index idx) = Just idx

        -- memory
        synMemoryToStruct :: Memory -> S.Memory
        synMemoryToStruct (Memory _ _ limits) = S.Memory limits

        checkMemoryIdentsUniqueness :: Module -> Either String ()
        checkMemoryIdentsUniqueness m@Module { imports, mems } = do
            mapM_ checkImportUniqueness $ filter isMemImport imports
            mapM_ checkMemUniqueness mems
            where
                checkImportUniqueness Import { desc = ImportMemory (Just id) _ } =
                    if length (getMemIndexes m id) > 1
                    then Left "duplicate memory"
                    else return ()
                checkImportUniqueness _ = return ()

                checkMemUniqueness (Memory _ (Just id) _) =
                    if length (getMemIndexes m id) > 1
                    then Left "duplicate memory"
                    else return ()
                checkMemUniqueness _ = return ()

        extractMemory :: [Memory] -> ModuleField -> [Memory]
        extractMemory mems (MFMem mem) = mem : mems
        extractMemory mems _ = mems

        isMemImport :: Import -> Bool
        isMemImport Import { desc = ImportMemory _ _ } = True
        isMemImport _ = False

        getMemIndexes :: Module -> Ident -> [Natural]
        getMemIndexes Module { imports, mems } id =
            let memImports = zip [0..] $ filter isMemImport imports in
            let importIndexes = map fst $ filter (\(_, Import { desc = ImportMemory ident _ }) -> ident == Just id) memImports in
            let isIdent (_, (Memory _ ident _)) = ident == Just id in
            let memIndexes = map fst $ filter isIdent $ zip [length memImports..] mems in
            map fromIntegral $ importIndexes ++ memIndexes

        getMemIndex :: Module -> MemoryIndex -> Maybe Natural
        getMemIndex mod (Named id) =
            case getMemIndexes mod id of
                [idx] -> return idx
                _ -> Nothing
        getMemIndex _ (Index idx) = Just idx

        -- global
        synGlobalToStruct :: Module -> Global -> Either String S.Global
        synGlobalToStruct mod Global { globalType, initializer } =
            let ctx = FunCtx mod [] [] [] in
            S.Global globalType <$> mapM (synInstrToStruct ctx) initializer

        checkGlobalIdentsUniqueness :: Module -> Either String ()
        checkGlobalIdentsUniqueness m@Module { imports, globals } = do
            mapM_ checkImportUniqueness $ filter isGlobalImport imports
            mapM_ checkGlobalUniqueness globals
            where
                checkImportUniqueness Import { desc = ImportGlobal (Just id) _ } =
                    if length (getGlobalIndexes m id) > 1
                    then Left "duplicate global"
                    else return ()
                checkImportUniqueness _ = return ()

                checkGlobalUniqueness (Global _ (Just id) _ _) =
                    if length (getGlobalIndexes m id) > 1
                    then Left "duplicate global"
                    else return ()
                checkGlobalUniqueness _ = return ()

        extractGlobal :: [Global] -> ModuleField -> [Global]
        extractGlobal globals (MFGlobal global) = global : globals
        extractGlobal globals _ = globals

        isGlobalImport :: Import -> Bool
        isGlobalImport Import { desc = ImportGlobal _ _ } = True
        isGlobalImport _ = False

        getGlobalIndexes :: Module -> Ident -> [Natural]
        getGlobalIndexes Module { imports, globals } id =
            let globalImports = zip [0..] $ filter isGlobalImport imports in
            let importIndexes = map fst $ filter (\(_, Import { desc = ImportGlobal ident _ }) -> ident == Just id) globalImports in
            let isIdent (_, Global { ident }) = ident == Just id in
            let globalIndexes = map fst $ filter isIdent $ zip [length globalImports..] globals in
            map fromIntegral $ importIndexes ++ globalIndexes

        getGlobalIndex :: Module -> GlobalIndex -> Maybe Natural
        getGlobalIndex mod@Module { imports, globals } (Named id) =
            case getGlobalIndexes mod id of
                [idx] -> return idx
                _ -> Nothing
        getGlobalIndex Module { imports, globals } (Index idx) = Just idx

        -- elem segment
        synElemToStruct :: Module -> ElemSegment -> Either String S.ElemSegment
        synElemToStruct mod ElemSegment { ident, elemType, mode, elements } = do
            let ctx = FunCtx mod [] [] []
            m <- case mode of {
                Active tableIndex offset ->
                    let offsetInstrs = mapM (synInstrToStruct ctx) offset in
                    let idx = fromJust $ getTableIndex mod tableIndex in
                    S.Active idx <$> offsetInstrs;
                Passive -> return S.Passive;
                Declarative -> return S.Declarative
            }
            let elemExprs = mapM (mapM (synInstrToStruct ctx)) elements
            S.ElemSegment elemType m <$> elemExprs

        extractElemSegment :: [ElemSegment] -> ModuleField -> [ElemSegment]
        extractElemSegment elems (MFElem elem) = elem : elems
        extractElemSegment elems _ = elems

        getElemIndex :: Module -> GlobalIndex -> Maybe Natural
        getElemIndex mod@Module { elems } (Named id) =
            let isIdent (_, ElemSegment { ident }) = ident == Just id in
            let elemIndexes = map fst $ filter isIdent $ zip [0..] elems in
            case elemIndexes of
                [idx] -> return idx
                _ -> Nothing
        getElemIndex _ (Index idx) = Just idx

        -- data segment
        synDataToStruct :: Module -> DataSegment -> Either String S.DataSegment
        synDataToStruct mod DataSegment { dataMode, datastring } = do
            m <- case dataMode of
                PassiveData -> return S.PassiveData
                ActiveData memIndex offset -> do
                    let ctx = FunCtx mod [] [] []
                    offsetInstrs <- mapM (synInstrToStruct ctx) offset
                    idx <- case getMemIndex mod memIndex of
                        Just idx -> return idx
                        Nothing -> throwError "unknown memory"
                    return $ S.ActiveData idx offsetInstrs
            return $ S.DataSegment m datastring

        extractDataSegment :: [DataSegment] -> ModuleField -> [DataSegment]
        extractDataSegment datas (MFData dataSegment) = dataSegment : datas
        extractDataSegment datas _ = datas

        getDataIndex :: Module -> GlobalIndex -> Maybe Natural
        getDataIndex mod@Module { datas } (Named id) =
            let isIdent (_, DataSegment { ident }) = ident == Just id in
            let dataIndexes = map fst $ filter isIdent $ zip [0..] datas in
            case dataIndexes of
                [idx] -> return idx
                _ -> Nothing
        getDataIndex _ (Index idx) = Just idx

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
        extractExports :: Module -> [ModuleField] -> [ModuleField]
        extractExports mod mf =
            let fromImports = foldl' reexport (0, 0, 0, 0, []) $ imports mod in
            let (_, _, _, _, result) = foldl' extractExport fromImports mf in
            reverse result
            where
                reexport (fidx, gidx, midx, tidx, mf) (Import {reExportAs, desc = ImportFunc _ _}) =
                    let exports = map (\name -> MFExport $ Export name $ ExportFunc $ Index fidx) reExportAs in
                    (fidx + 1, gidx, midx, tidx, exports ++ mf)
                reexport (fidx, gidx, midx, tidx, mf) (Import {reExportAs, desc = ImportGlobal _ _}) =
                    let exports = map (\name -> MFExport $ Export name $ ExportGlobal $ Index gidx) reExportAs in
                    (fidx, gidx + 1, midx, tidx, exports ++ mf)
                reexport (fidx, gidx, midx, tidx, mf) (Import {reExportAs, desc = ImportMemory _ _}) =
                    let exports = map (\name -> MFExport $ Export name $ ExportMemory $ Index midx) reExportAs in
                    (fidx, gidx, midx + 1, tidx, exports ++ mf)
                reexport (fidx, gidx, midx, tidx, mf) (Import {reExportAs, desc = ImportTable _ _}) =
                    let exports = map (\name -> MFExport $ Export name $ ExportTable $ Index tidx) reExportAs in
                    (fidx, gidx, midx, tidx + 1, exports ++ mf)

                extractExport (fidx, gidx, midx, tidx, mf) (MFFunc fun@Function{ exportFuncAs }) =
                    let exports = map (\name -> MFExport $ Export name $ ExportFunc $ Index fidx) exportFuncAs in
                    (fidx + 1, gidx, midx, tidx, [MFFunc fun] ++ exports ++ mf)
                extractExport (fidx, gidx, midx, tidx, mf) (MFGlobal glob@Global{ exportGlobalAs }) =
                    let exports = map (\name -> MFExport $ Export name $ ExportGlobal $ Index gidx) exportGlobalAs in
                    (fidx, gidx + 1, midx, tidx, [MFGlobal glob] ++ exports ++ mf)
                extractExport (fidx, gidx, midx, tidx, mf) (MFMem (Memory exps i l)) =
                    let exports = map (\name -> MFExport $ Export name $ ExportMemory $ Index midx) exps in
                    (fidx, gidx, midx + 1, tidx, [MFMem (Memory exps i l)] ++ exports ++ mf)
                extractExport (fidx, gidx, midx, tidx, mf) (MFTable (Table exps i t)) =
                    let exports = map (\name -> MFExport $ Export name $ ExportTable $ Index tidx) exps in
                    (fidx, gidx, midx, tidx + 1, [MFTable (Table exps i t)] ++ exports ++ mf)
                extractExport (fidx, gidx, midx, tidx, mf) f = (fidx, gidx, midx, tidx, f:mf)
 
        synExportsToStruct :: Module -> [ModuleField] -> [S.Export]
        synExportsToStruct mod (MFExport Export { name, desc = ExportFunc idx } : rest) =
            let exp = S.Export name $ S.ExportFunc $ fromJust $ getFuncIndex mod idx in
            exp : synExportsToStruct mod rest
        synExportsToStruct mod (MFExport Export { name, desc = ExportTable idx } : rest) =
            let exp = S.Export name $ S.ExportTable $ fromJust $ getTableIndex mod idx in
            exp : synExportsToStruct mod rest
        synExportsToStruct mod (MFExport Export { name, desc = ExportMemory idx } : rest) =
            let exp = S.Export name $ S.ExportMemory $ fromJust $ getMemIndex mod idx in
            exp : synExportsToStruct mod rest
        synExportsToStruct mod (MFExport Export { name, desc = ExportGlobal idx } : rest) =
            let exp = S.Export name $ S.ExportGlobal $ fromJust $ getGlobalIndex mod idx in
            exp : synExportsToStruct mod rest
        synExportsToStruct mod (_ : rest) = synExportsToStruct mod rest
        synExportsToStruct _ [] = []
}