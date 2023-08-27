{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Wasm.Validate (
    ValidationError(..),
    ValidationResult(..),
    validate,
    isValid,
    ValidModule,
    getModule
) where

import Language.Wasm.Structure
import qualified Data.Set as Set
import Data.List (foldl')
import qualified Data.Text.Lazy as TL
import Data.Maybe (fromMaybe, catMaybes)
import Numeric.Natural (Natural)
import Prelude hiding ((<>))

import Control.Monad (foldM, forM_, when, unless)
import Control.Monad.Reader (ReaderT, runReaderT, withReaderT, ask)
import Control.Monad.Except (Except, runExcept, throwError)

import Debug.Trace as Debug

data ValidationError =
    DuplicatedExportNames [String]
    | InvalidTableType
    | MinMoreThanMaxInMemoryLimit
    | MemoryLimitExceeded
    | AlignmentOverflow
    | MoreThanOneMemory
    | FunctionIndexOutOfRange Natural
    | TableIndexOutOfRange Natural
    | MemoryIndexOutOfRange Natural
    | LocalIndexOutOfRange Natural
    | GlobalIndexOutOfRange Natural
    | ElemIndexOutOfRange Natural
    | DataIndexOutOfRange Natural
    | LabelIndexOutOfRange
    | TypeIndexOutOfRange
    | ResultTypeDoesntMatch
    | TypeMismatch { actual :: Arrow, expected :: Arrow }
    | RefTypeMismatch ElemType ElemType
    | InvalidResultArity
    | InvalidConstantExpr
    | InvalidStartFunctionType
    | GlobalIsImmutable
    | UndeclaredFunctionRef Natural
    deriving (Show, Eq)

type ValidationResult = Either ValidationError ()

-- semigroup definition for Either a b is in conflict with my ad-hoc instance
-- to keep an old code Prelude version is hidden and redefined locally
(<>) = mappend

instance Monoid ValidationResult where
    mempty = Right ()
    mappend (Right ()) vr = vr
    mappend vr (Right ()) = vr
    mappend vr _ = vr

isValid :: ValidationResult -> Bool
isValid (Right ()) = True
isValid (Left reason) = False

type Validator = Module -> ValidationResult

data VType =
    Val ValueType
    | Var
    | NonRefVar
    | Any
    deriving (Show, Eq)

type End = [VType]

empty :: [ValueType]
empty = []

class ToEnd a where
    toEnd :: a -> [VType]

instance ToEnd VType where
    toEnd val = [val]

instance ToEnd ValueType where
    toEnd val = [Val val]

instance ToEnd [ValueType] where
    toEnd = map Val

instance ToEnd [VType] where
    toEnd = id

data Arrow = Arrow End End deriving (Show, Eq)

(==>) :: (ToEnd a, ToEnd b) => a -> b -> Arrow
(==>) a b = Arrow (toEnd a) (toEnd b)

asArrow :: FuncType -> Arrow
asArrow (FuncType params results) = Arrow (map Val params) (map Val $ reverse results)

isArrowMatch :: Arrow -> Arrow -> Bool
isArrowMatch (f `Arrow` t) ( f' `Arrow` t') = isEndMatch f f' && isEndMatch t t'
    where
        isRef :: VType -> Bool
        isRef (Val Func) = True
        isRef (Val Extern) = True
        isRef _            = False

        isEndMatch :: End -> End -> Bool
        isEndMatch (Any:l) (Any:r) =
            let (leftTail, rightTail) = unzip $ zip (takeWhile (/= Any) $ reverse l) (takeWhile (/= Any) $ reverse r) in
            isEndMatch (reverse leftTail) (reverse rightTail)
        isEndMatch (Any:l) r =
            let (leftTail, rightTail) = unzip $ zip (takeWhile (/= Any) $ reverse l) (takeWhile (/= Any) $ reverse r) in
            isEndMatch (reverse leftTail) (reverse rightTail)
        isEndMatch l (Any:r) =
            let (leftTail, rightTail) = unzip $ zip (takeWhile (/= Any) $ reverse l) (takeWhile (/= Any) $ reverse r) in
            isEndMatch (reverse leftTail) (reverse rightTail)
        isEndMatch (Var:l) (x:r) =
            let subst = replace Var x in
            isEndMatch (subst l) (subst r)
        isEndMatch (x:l) (Var:r) =
            let subst = replace Var x in
            isEndMatch (subst l) (subst r)
        isEndMatch (NonRefVar:l) (x:r) =
            let subst = replace NonRefVar x in
            isEndMatch (subst l) (subst r)
        isEndMatch (x:l) (NonRefVar:r) =
            let subst = replace NonRefVar x in
            isEndMatch (subst l) (subst r)
        isEndMatch (Val v:l) (Val v':r) = v == v' && isEndMatch l r
        isEndMatch [] [] = True
        isEndMatch _ _ = False

data Ctx = Ctx {
    types :: [FuncType],
    funcs :: [FuncType],
    tableTypes :: [TableType],
    elems :: [ElemType],
    datas :: [DataMode],
    mems :: [Limit],
    globals :: [GlobalType],
    locals :: [ValueType],
    labels :: [[ValueType]],
    returns :: [ValueType],
    importedGlobals :: Natural,
    refs :: Set.Set Natural
} deriving (Show, Eq)

type Checker = ReaderT Ctx (Except ValidationError)

freshVar :: Checker VType
freshVar = return Var

runChecker :: Ctx -> Checker a -> Either ValidationError a
runChecker ctx = runExcept . flip runReaderT ctx

(!?) :: [a] -> Natural -> Maybe a
(!?) (x:_) 0 = Just x
(!?) (_:rest) n = rest !? (n - 1)
(!?) [] _ = Nothing

safeHead :: [a] -> Maybe a
safeHead (x: _) = Just x
safeHead [] = Nothing

maybeToEither :: ValidationError -> Maybe a -> Checker a
maybeToEither _ (Just a) = return a
maybeToEither l Nothing = throwError l

asType :: GlobalType -> VType
asType (Const v) = Val v
asType (Mut v) = Val v

shouldBeMut :: GlobalType -> Checker ()
shouldBeMut (Mut _) = return ()
shouldBeMut (Const v) = throwError GlobalIsImmutable

getLabel :: LabelIndex -> Checker [ValueType]
getLabel lbl = do
    Ctx { labels } <- ask
    case labels !? lbl of
        Nothing -> throwError LabelIndexOutOfRange
        Just v -> return v

withLabel :: [ValueType] -> Checker a -> Checker a
withLabel result = withReaderT (\ctx -> ctx { labels = result : labels ctx })

isMemArgValid :: Int -> MemArg -> Checker ()
isMemArgValid sizeInBytes MemArg { align } = if 2 ^ align <= sizeInBytes then return () else throwError AlignmentOverflow

checkMemoryInstr :: Int -> MemArg -> Checker ()
checkMemoryInstr size memarg = do
    isMemArgValid size memarg
    Ctx { mems } <- ask 
    if length mems < 1 then throwError (MemoryIndexOutOfRange 0) else return ()

getBlockType :: BlockType -> Checker Arrow
getBlockType (Inline Nothing) = return $ empty ==> empty
getBlockType (Inline (Just valType)) = return $ empty ==> valType
getBlockType (TypeIndex typeIdx) = do
    Ctx { types } <- ask
    maybeToEither TypeIndexOutOfRange $ asArrow <$> types !? typeIdx

getResultType :: BlockType -> Checker [ValueType]
getResultType (Inline Nothing) = return []
getResultType (Inline (Just valType)) = return [valType]
getResultType (TypeIndex typeIdx) = do
    Ctx { types } <- ask
    maybeToEither TypeIndexOutOfRange $ results <$> types !? typeIdx

elemTypeToRefType :: ElemType -> ValueType 
elemTypeToRefType FuncRef = Func
elemTypeToRefType ExternRef = Extern

getInstrType :: [VType] -> Instruction Natural -> Checker Arrow
getInstrType _ Unreachable = return $ Any ==> Any
getInstrType _ Nop = return $ empty ==> empty
getInstrType _ Block { blockType, body } = do
    bt@(Arrow from _) <- getBlockType blockType
    resultType <- getResultType blockType
    t <- withLabel resultType $ getExpressionTypeWithInput from body
    if isArrowMatch t bt
    then return bt
    else throwError $ TypeMismatch t bt
getInstrType _ Loop { blockType, body } = do
    bt@(Arrow from _) <- getBlockType blockType
    resultType <- getResultType blockType
    t <- withLabel (map (\(Val v) -> v) from) $ getExpressionTypeWithInput from body
    if isArrowMatch t bt
    then return bt
    else throwError $ TypeMismatch t bt
getInstrType _ If { blockType, true, false } = do
    bt@(Arrow from _) <- getBlockType blockType
    resultType <- getResultType blockType
    l <- withLabel resultType $ getExpressionTypeWithInput from true
    r <- withLabel resultType $ getExpressionTypeWithInput from false
    if isArrowMatch l bt
    then (
            if isArrowMatch r bt
            then let Arrow from to = bt in
                (return $ (from ++ [Val I32]) ==> to)
            else (throwError $ TypeMismatch r bt)
        )
    else throwError $ TypeMismatch l bt
getInstrType _ (Br lbl) = do
    r <- map Val <$> getLabel lbl
    return $ (Any : r) ==> Any
getInstrType _ (BrIf lbl) = do
    r <- map Val <$> getLabel lbl
    return $ (r ++ [Val I32]) ==> r
getInstrType stack (BrTable lbls lbl) = do
    r <- getLabel lbl
    let returns lbl = do
            args <- map Val <$> getLabel lbl
            res <- matchStack stack (Val I32 : reverse args) []
            return (args, res)
    alternatives <- mapM returns lbls
    (_, def) <- returns lbl
    if all (\(args, res) -> res == def && length args == length r) alternatives
    then return $ ([Any] ++ (map Val r) ++ [Val I32]) ==> Any
    else throwError ResultTypeDoesntMatch
getInstrType _ Return = do
    Ctx { returns } <- ask
    return $ (Any : (map Val returns)) ==> Any
getInstrType _ (Call fun) = do
    Ctx { funcs } <- ask
    maybeToEither (FunctionIndexOutOfRange fun) $ asArrow <$> funcs !? fun
getInstrType _ (CallIndirect tableIdx sign) = do
    Ctx { types, tableTypes = tables } <- ask
    if length tables <= fromIntegral tableIdx
    then throwError (TableIndexOutOfRange tableIdx)
    else do
        Arrow from to <- maybeToEither TypeIndexOutOfRange $ asArrow <$> types !? sign
        return $ (from ++ [Val I32]) ==> to
getInstrType _ Drop = do
    var <- freshVar
    return $ var ==> empty
getInstrType _ (Select Nothing) = do
    var <- return NonRefVar
    return $ [var, var, Val I32] ==> var
getInstrType _ (Select (Just vt)) =
    case vt of
        [t] -> return $ [t, t, I32] ==> t
        _ -> throwError InvalidResultArity
getInstrType _ (RefNull elType) = do
    let t = case elType of { FuncRef -> Func; ExternRef -> Extern }
    return $ empty ==> Val t
getInstrType _ RefIsNull = do
    var <- freshVar
    return $ var ==> Val I32
getInstrType _ (RefFunc funIdx) = do
    Ctx { funcs, refs } <- ask
    if fromIntegral funIdx < length funcs
    then do
        unless (Set.member funIdx refs) $
            throwError $ UndeclaredFunctionRef $ fromIntegral funIdx
        return $ empty ==> Val Func
    else throwError $ FunctionIndexOutOfRange $ fromIntegral funIdx
getInstrType _ (GetLocal local) = do
    Ctx { locals }  <- ask
    t <- maybeToEither (LocalIndexOutOfRange local) $ locals !? local
    return $ empty ==> Val t
getInstrType _ (SetLocal local) = do
    Ctx { locals } <- ask
    t <- maybeToEither (LocalIndexOutOfRange local) $ locals !? local
    return $ Val t ==> empty
getInstrType _ (TeeLocal local) = do
    Ctx { locals } <- ask
    t <- maybeToEither (LocalIndexOutOfRange local) $ locals !? local
    return $ Val t ==> Val t
getInstrType _ (GetGlobal global) = do
    Ctx { globals } <- ask
    t <- maybeToEither (GlobalIndexOutOfRange global) $ asType <$> globals !? global
    return $ empty ==> t
getInstrType _ (SetGlobal global) = do
    Ctx { globals } <- ask
    t <- maybeToEither (GlobalIndexOutOfRange global) $ asType <$> globals !? global
    shouldBeMut $ globals !! fromIntegral global
    return $ t ==> empty
getInstrType _ (I32Load memarg) = do
    checkMemoryInstr 4 memarg
    return $ I32 ==> I32
getInstrType _ (I64Load memarg) = do
    checkMemoryInstr 8 memarg
    return $ I32 ==> I64
getInstrType _ (F32Load memarg) = do
    checkMemoryInstr 4 memarg
    return $ I32 ==> F32
getInstrType _ (F64Load memarg) = do
    checkMemoryInstr 8 memarg
    return $ I32 ==> F64
getInstrType _ (I32Load8S memarg) = do
    checkMemoryInstr 1 memarg
    return $ I32 ==> I32
getInstrType _ (I32Load8U memarg) = do
    checkMemoryInstr 1 memarg
    return $ I32 ==> I32
getInstrType _ (I32Load16S memarg) = do
    checkMemoryInstr 2 memarg
    return $ I32 ==> I32
getInstrType _ (I32Load16U memarg) = do
    checkMemoryInstr 2 memarg
    return $ I32 ==> I32
getInstrType _ (I64Load8S memarg) = do
    checkMemoryInstr 1 memarg
    return $ I32 ==> I64
getInstrType _ (I64Load8U memarg) = do
    checkMemoryInstr 1 memarg
    return $ I32 ==> I64
getInstrType _ (I64Load16S memarg) = do
    checkMemoryInstr 2 memarg
    return $ I32 ==> I64
getInstrType _ (I64Load16U memarg) = do
    checkMemoryInstr 2 memarg
    return $ I32 ==> I64
getInstrType _ (I64Load32S memarg) = do
    checkMemoryInstr 4 memarg
    return $ I32 ==> I64
getInstrType _ (I64Load32U memarg) = do
    checkMemoryInstr 4 memarg
    return $ I32 ==> I64
getInstrType _ (I32Store memarg) = do
    checkMemoryInstr 4 memarg
    return $ [I32, I32] ==> empty
getInstrType _ (I64Store memarg) = do
    checkMemoryInstr 8 memarg
    return $ [I32, I64] ==> empty
getInstrType _ (F32Store memarg) = do
    checkMemoryInstr 4 memarg
    return $ [I32, F32] ==> empty
getInstrType _ (F64Store memarg) = do
    checkMemoryInstr 8 memarg
    return $ [I32, F64] ==> empty
getInstrType _ (I32Store8 memarg) = do
    checkMemoryInstr 1 memarg
    return $ [I32, I32] ==> empty
getInstrType _ (I32Store16 memarg) = do
    checkMemoryInstr 2 memarg
    return $ [I32, I32] ==> empty
getInstrType _ (I64Store8 memarg) = do
    checkMemoryInstr 1 memarg
    return $ [I32, I64] ==> empty
getInstrType _ (I64Store16 memarg) = do
    checkMemoryInstr 2 memarg
    return $ [I32, I64] ==> empty
getInstrType _ (I64Store32 memarg) = do
    checkMemoryInstr 4 memarg
    return $ [I32, I64] ==> empty
getInstrType _ MemorySize = do
    Ctx { mems } <- ask 
    when (length mems < 1) $ throwError (MemoryIndexOutOfRange 0)
    return $ empty ==> I32
getInstrType _ MemoryGrow = do
    Ctx { mems } <- ask
    when (length mems < 1) $ throwError (MemoryIndexOutOfRange 0)
    return $ I32 ==> I32
getInstrType _ MemoryFill = do
    Ctx { mems } <- ask
    when (length mems < 1) $ throwError (MemoryIndexOutOfRange 0)
    return $ [I32, I32, I32] ==> empty
getInstrType _ MemoryCopy = do
    Ctx { mems } <- ask
    when (length mems < 1) $ throwError (MemoryIndexOutOfRange 0)
    return $ [I32, I32, I32] ==> empty
getInstrType _ (MemoryInit dataIdx) = do
    Ctx { mems, datas } <- ask
    when (length mems < 1) $ throwError (MemoryIndexOutOfRange 0)
    when (length datas <= fromIntegral dataIdx) $ throwError (DataIndexOutOfRange dataIdx)
    return $ [I32, I32, I32] ==> empty
getInstrType _ (DataDrop dataIdx) = do
    Ctx { datas } <- ask
    when (length datas <= fromIntegral dataIdx) $ throwError (DataIndexOutOfRange dataIdx)
    return $ empty ==> empty
getInstrType _ (TableInit tableIdx elemIdx) = do
    Ctx { tableTypes = tables, elems } <- ask
    when (length tables <= fromIntegral tableIdx) $ throwError (TableIndexOutOfRange tableIdx)
    when (length elems <= fromIntegral elemIdx) $ throwError (ElemIndexOutOfRange elemIdx)
    let TableType _ tableType = tables !! fromIntegral tableIdx
    let elemType = elems !! fromIntegral elemIdx
    when (elemType /= tableType) $ throwError (RefTypeMismatch tableType elemType)
    return $ [I32, I32, I32] ==> empty
getInstrType _ (TableCopy toIdx fromIdx) = do
    Ctx { tableTypes = tables } <- ask
    let (from, to) = (fromIntegral fromIdx, fromIntegral toIdx)
    when (length tables <= from) $ throwError (TableIndexOutOfRange fromIdx)
    when (length tables <= to) $ throwError (TableIndexOutOfRange toIdx)
    let TableType _ fromType = tables !! from
    let TableType _ toType = tables !! to
    when (fromType /= toType) $ throwError (RefTypeMismatch fromType toType)
    return $ [I32, I32, I32] ==> empty
getInstrType _ (TableFill tableIdx) = do
    Ctx { tableTypes = tables } <- ask
    when (length tables <= fromIntegral tableIdx) $ throwError (TableIndexOutOfRange tableIdx)
    let TableType _ tableType = tables !! fromIntegral tableIdx
    return $ [I32, elemTypeToRefType tableType, I32] ==> empty
getInstrType _ (TableSize tableIdx) = do
    Ctx { tableTypes = tables } <- ask
    when (length tables <= fromIntegral tableIdx) $ throwError (TableIndexOutOfRange tableIdx)
    return $ empty ==> I32
getInstrType _ (TableGrow tableIdx) = do
    Ctx { tableTypes = tables } <- ask
    when (length tables <= fromIntegral tableIdx) $ throwError (TableIndexOutOfRange tableIdx)
    let TableType _ tableType = tables !! fromIntegral tableIdx
    return $ [elemTypeToRefType tableType, I32] ==> I32
getInstrType _ (TableGet tableIdx) = do
    Ctx { tableTypes = tables } <- ask
    when (length tables <= fromIntegral tableIdx) $ throwError (TableIndexOutOfRange tableIdx)
    let TableType _ tableType = tables !! fromIntegral tableIdx
    return $ I32 ==> (elemTypeToRefType tableType)
getInstrType _ (TableSet tableIdx) = do
    Ctx { tableTypes = tables } <- ask
    when (length tables <= fromIntegral tableIdx) $ throwError (TableIndexOutOfRange tableIdx)
    let TableType _ tableType = tables !! fromIntegral tableIdx
    return $ [I32, elemTypeToRefType tableType] ==> empty
getInstrType _ (ElemDrop elemIdx) = do
    Ctx { elems } <- ask
    when (length elems <= fromIntegral elemIdx) $ throwError (ElemIndexOutOfRange elemIdx)
    return $ empty ==> empty
getInstrType _ (I32Const _) = return $ empty ==> I32
getInstrType _ (I64Const _) = return $ empty ==> I64
getInstrType _ (F32Const _) = return $ empty ==> F32
getInstrType _ (F64Const _) = return $ empty ==> F64
getInstrType _ (IUnOp BS32 _) = return $ I32 ==> I32
getInstrType _ (IUnOp BS64 _) = return $ I64 ==> I64
getInstrType _ (IBinOp BS32 _) = return $ [I32, I32] ==> I32
getInstrType _ (IBinOp BS64 _) = return $ [I64, I64] ==> I64
getInstrType _ I32Eqz = return $ I32 ==> I32
getInstrType _ I64Eqz = return $ I64 ==> I32
getInstrType _ (IRelOp BS32 _) = return $ [I32, I32] ==> I32
getInstrType _ (IRelOp BS64 _) = return $ [I64, I64] ==> I32
getInstrType _ (FUnOp BS32 _) = return $ F32 ==> F32
getInstrType _ (FUnOp BS64 _) = return $ F64 ==> F64
getInstrType _ (FBinOp BS32 _) = return $ [F32, F32] ==> F32
getInstrType _ (FBinOp BS64 _) = return $ [F64, F64] ==> F64
getInstrType _ (FRelOp BS32 _) = return $ [F32, F32] ==> I32
getInstrType _ (FRelOp BS64 _) = return $ [F64, F64] ==> I32
getInstrType _ I32WrapI64 = return $ I64 ==> I32
getInstrType _ (ITruncFU BS32 BS32) = return $ F32 ==> I32
getInstrType _ (ITruncFU BS32 BS64) = return $ F64 ==> I32
getInstrType _ (ITruncFU BS64 BS32) = return $ F32 ==> I64
getInstrType _ (ITruncFU BS64 BS64) = return $ F64 ==> I64
getInstrType _ (ITruncFS BS32 BS32) = return $ F32 ==> I32
getInstrType _ (ITruncFS BS32 BS64) = return $ F64 ==> I32
getInstrType _ (ITruncFS BS64 BS32) = return $ F32 ==> I64
getInstrType _ (ITruncFS BS64 BS64) = return $ F64 ==> I64
getInstrType _ (ITruncSatFU BS32 BS32) = return $ F32 ==> I32
getInstrType _ (ITruncSatFU BS32 BS64) = return $ F64 ==> I32
getInstrType _ (ITruncSatFU BS64 BS32) = return $ F32 ==> I64
getInstrType _ (ITruncSatFU BS64 BS64) = return $ F64 ==> I64
getInstrType _ (ITruncSatFS BS32 BS32) = return $ F32 ==> I32
getInstrType _ (ITruncSatFS BS32 BS64) = return $ F64 ==> I32
getInstrType _ (ITruncSatFS BS64 BS32) = return $ F32 ==> I64
getInstrType _ (ITruncSatFS BS64 BS64) = return $ F64 ==> I64
getInstrType _ I64ExtendSI32 = return $ I32 ==> I64
getInstrType _ I64ExtendUI32 = return $ I32 ==> I64
getInstrType _ (FConvertIU BS32 BS32) = return $ I32 ==> F32
getInstrType _ (FConvertIU BS32 BS64) = return $ I64 ==> F32
getInstrType _ (FConvertIU BS64 BS32) = return $ I32 ==> F64
getInstrType _ (FConvertIU BS64 BS64) = return $ I64 ==> F64
getInstrType _ (FConvertIS BS32 BS32) = return $ I32 ==> F32
getInstrType _ (FConvertIS BS32 BS64) = return $ I64 ==> F32
getInstrType _ (FConvertIS BS64 BS32) = return $ I32 ==> F64
getInstrType _ (FConvertIS BS64 BS64) = return $ I64 ==> F64
getInstrType _ F32DemoteF64 = return $ F64 ==> F32
getInstrType _ F64PromoteF32 = return $ F32 ==> F64
getInstrType _ (IReinterpretF BS32) = return $ F32 ==> I32
getInstrType _ (IReinterpretF BS64) = return $ F64 ==> I64
getInstrType _ (FReinterpretI BS32) = return $ I32 ==> F32
getInstrType _ (FReinterpretI BS64) = return $ I64 ==> F64


replace :: (Eq a) => a -> a -> [a] -> [a]
replace _ _ [] = []
replace x y (v:r) = (if x == v then y else v) : replace x y r

getExpressionTypeWithInput :: [VType] -> Expression -> Checker Arrow
getExpressionTypeWithInput inp = fmap (inp `Arrow`) . foldM go inp
    where
        go :: [VType] -> Instruction Natural -> Checker [VType]
        go stack instr = do
            (f `Arrow` t) <- getInstrType stack instr
            matchStack stack (reverse f) t

isRef :: ValueType -> Bool
isRef (Func)   = True
isRef (Extern) = True
isRef _        = False

matchStack :: [VType] -> [VType] -> [VType] -> Checker [VType]
matchStack stack@(Any:_) _arg res = return $ res ++ stack
matchStack (Val v:stack) (Val v':args) res =
    if v == v'
    then matchStack stack args res
    else throwError $ TypeMismatch ((reverse $ Val v':args) `Arrow` res) ([] `Arrow` (Val v:stack))
matchStack _ (Any:_) res = return $ res
matchStack (Val v:stack) (Var:args) res =
    let subst = replace Var (Val v) in
    matchStack stack (subst args) (subst res)
matchStack (Var:stack) (Val v:args) res =
    let subst = replace Var (Val v) in
    matchStack stack (subst args) (subst res)
matchStack (Val v:stack) (NonRefVar:args) res =
    let subst = replace NonRefVar (Val v) in
    if isRef v
    then throwError $ TypeMismatch (empty ==> empty) (empty ==> empty)
    else matchStack stack (subst args) (subst res)
matchStack (NonRefVar:stack) (Val v:args) res =
    let subst = replace NonRefVar (Val v) in
    if isRef v
    then throwError $ TypeMismatch (empty ==> empty) (empty ==> empty)
    else matchStack stack (subst args) (subst res)
matchStack (Var:stack) (NonRefVar:args) res =
    let subst = replace NonRefVar NonRefVar in
    matchStack stack (subst args) (subst res)
matchStack (NonRefVar:stack) (Var:args) res =
    let subst = replace Var NonRefVar in
    matchStack stack (subst args) (subst res)
matchStack stack [] res = return $ res ++ stack
matchStack [] args res = throwError $ TypeMismatch ((reverse args) `Arrow` res) ([] `Arrow` [])
matchStack st args res = error $ "inconsistent checker state: " ++ show (st, args, res)

getExpressionType :: Expression -> Checker Arrow
getExpressionType = getExpressionTypeWithInput []

isConstExpression :: Expression -> Checker ()
isConstExpression [] = return ()
isConstExpression ((I32Const _):rest) = isConstExpression rest
isConstExpression ((I64Const _):rest) = isConstExpression rest
isConstExpression ((F32Const _):rest) = isConstExpression rest
isConstExpression ((F64Const _):rest) = isConstExpression rest
isConstExpression ((RefNull _):rest) = isConstExpression rest
isConstExpression ((RefFunc _):rest) = isConstExpression rest
isConstExpression ((GetGlobal idx):rest) = do
    Ctx {globals, importedGlobals} <- ask
    if importedGlobals <= idx
        then throwError (GlobalIndexOutOfRange idx)
        else return ()
    case globals !! fromIntegral idx of
        Const _ -> isConstExpression rest
        Mut _ -> throwError InvalidConstantExpr
isConstExpression _ = throwError InvalidConstantExpr

getFuncTypes :: Module -> [FuncType]
getFuncTypes Module {types, functions, imports} =
    let funImports = catMaybes $ map getFuncType imports in
    funImports ++ map ((types !!) . fromIntegral . funcType) functions
    where
        getFuncType (Import _ _ (ImportFunc typeIdx)) = Just $ types !! (fromIntegral typeIdx)
        getFuncType _ = Nothing

ctxFromModule :: [ValueType] -> [[ValueType]] -> [ValueType] -> Module -> Ctx
ctxFromModule locals labels returns m =
    let Module {types, tables, mems, globals, imports, elems, exports, datas} = m in
    let tableImports = catMaybes $ map getTableType imports in
    let memsImports = catMaybes $ map getMemType imports in
    let globalImports = catMaybes $ map getGlobalType imports in
    Ctx {
        types,
        funcs = getFuncTypes m,
        tableTypes = tableImports ++ map (\(Table t) -> t) tables,
        elems = map elemType elems,
        datas = map dataMode datas,
        mems = memsImports ++ map (\(Memory l) -> l) mems,
        globals = globalImports ++ map (\(Global g _) -> g) globals,
        locals,
        labels,
        returns,
        importedGlobals = fromIntegral $ length globalImports,
        refs = Set.unions $ map getElemRefs elems
            ++ map getGlobalRefs globals
            ++ map getExportRefs exports
    }
    where
        getTableType (Import _ _ (ImportTable tableType)) = Just tableType
        getTableType _ = Nothing

        getMemType (Import _ _ (ImportMemory lim)) = Just lim
        getMemType _ = Nothing

        getGlobalType (Import _ _ (ImportGlobal gl)) = Just gl
        getGlobalType _ = Nothing

        getElemRefs ElemSegment{ elemType = FuncRef, elements} =
            foldl extractRef Set.empty elements
            where
                extractRef refs [RefFunc idx] = Set.insert idx refs
                extractRef refs _ = refs
        getElemRefs _ = Set.empty

        getGlobalRefs Global {initializer = [RefFunc idx]} = Set.singleton idx
        getGlobalRefs _ = Set.empty

        getExportRefs Export {desc = ExportFunc idx} = Set.singleton idx
        getExportRefs _ = Set.empty

isFunctionValid :: Function -> Validator
isFunctionValid Function {funcType, localTypes = locals, body} mod@Module {types} =
    if fromIntegral funcType < length types
    then do
        let FuncType params results = types !! fromIntegral funcType
        let ctx = ctxFromModule (params ++ locals) [results] results mod
        arr <- runChecker ctx $ getExpressionType body
        if isArrowMatch arr (empty ==> (reverse results))
        then return ()
        else Left $ TypeMismatch arr (empty ==> (reverse results))
    else Left TypeIndexOutOfRange

functionsShouldBeValid :: Validator
functionsShouldBeValid mod@Module {functions} =
    foldMap (flip isFunctionValid mod) functions

tablesShouldBeValid :: Validator
tablesShouldBeValid Module { imports, tables } =
    let tableImports = filter isTableImport imports in
    let res = foldMap (\Import { desc = ImportTable t } -> isValidTableType t) tableImports in
    foldl' (\r (Table t) -> r <> isValidTableType t) res tables
    where
        isValidTableType :: TableType -> ValidationResult
        isValidTableType (TableType (Limit min max) _) =
            if min <= fromMaybe min max
            then return ()
            else Left InvalidTableType

memoryShouldBeValid :: Validator
memoryShouldBeValid Module { imports, mems } =
    let memImports = filter isMemImport imports in
    let res = foldMap (\Import { desc = ImportMemory l } -> isValidLimit l) memImports in
    let res' = foldl' (\r (Memory l) -> r <> isValidLimit l) res mems in
    if length memImports + length mems <= 1
        then res'
        else Left MoreThanOneMemory
    where
        isValidLimit :: Limit -> ValidationResult
        isValidLimit (Limit min max) =
            let minMax = if min <= fromMaybe min max then return () else Left MinMoreThanMaxInMemoryLimit in
            let maxLim = if fromMaybe min max <= 65536 then return () else Left MemoryLimitExceeded in
            minMax <> maxLim

globalsShouldBeValid :: Validator
globalsShouldBeValid m@Module { imports, globals } =
    let ctx = ctxFromModule [] [] [] m in
    foldMap (isGlobalValid ctx) globals
    where
        getGlobalType :: GlobalType -> ValueType
        getGlobalType (Const vt) = vt
        getGlobalType (Mut vt) = vt

        isGlobalValid :: Ctx -> Global -> ValidationResult
        isGlobalValid ctx (Global gt init) = runChecker ctx $ do
            isConstExpression init
            t <- getExpressionType init
            let expected = empty ==> getGlobalType gt
            if isArrowMatch expected t then return () else throwError $ TypeMismatch t expected

elemsShouldBeValid :: Validator
elemsShouldBeValid m@Module { elems, functions, tables, imports } =
    let ctx = ctxFromModule [] [] [] m in
    foldMap (isElemValid ctx) elems
    where
        isElemValid :: Ctx -> ElemSegment -> ValidationResult
        isElemValid ctx (ElemSegment elemType mode elements) = do
            forM_ elements $ \elem -> runChecker ctx $ do
                arr <- getExpressionType elem
                isConstExpression elem
                unless (isValidRef elemType arr)
                    $ throwError $ RefTypeMismatch elemType elemType
            case mode of
                Active tableIdx offset -> runChecker ctx $ do
                    isConstExpression offset
                    t <- getExpressionType offset
                    unless (isArrowMatch (empty ==> I32) t) $ do
                        throwError $ TypeMismatch t (empty ==> I32)
                    let tableImports = filter isTableImport imports
                    when (tableIdx >= fromIntegral (length tableImports + length tables)) $ do
                        throwError $ TableIndexOutOfRange tableIdx
                    let TableType _ tableType = tableTypes ctx !! (fromIntegral tableIdx)
                    when (tableType /= elemType) $ do
                        throwError $ RefTypeMismatch elemType tableType
                _ -> return ()
        
        isValidRef :: ElemType -> Arrow -> Bool
        isValidRef FuncRef arr | arr == (empty ==> Func) = True
        isValidRef ExternRef arr | arr == (empty ==> Extern) = True
        isValidRef _ _ = False

datasShouldBeValid :: Validator
datasShouldBeValid m@Module { datas, mems, imports } =
    let ctx = ctxFromModule [] [] [] m in
    foldMap (isDataValid ctx) datas
    where
        isDataValid :: Ctx -> DataSegment -> ValidationResult
        isDataValid ctx (DataSegment (ActiveData memIdx offset) _) =
            let check = runChecker ctx $ do
                    isConstExpression offset
                    t <- getExpressionType offset
                    if isArrowMatch (empty ==> I32) t
                    then return ()
                    else throwError $ TypeMismatch t (empty ==> I32) 
            in
            let memImports = filter isMemImport imports in
            if memIdx < (fromIntegral $ length memImports + length mems)
            then check
            else Left (MemoryIndexOutOfRange memIdx)
        isDataValid ctx (DataSegment PassiveData _) = return ()

startShouldBeValid :: Validator
startShouldBeValid Module { start = Nothing } = return ()
startShouldBeValid m@Module { start = Just (StartFunction idx) } =
    let types = getFuncTypes m in
    let i = fromIntegral idx in
    if length types > i
    then if FuncType [] [] == types !! i then return () else Left InvalidStartFunctionType
    else Left $ FunctionIndexOutOfRange $ fromIntegral i

exportsShouldBeValid :: Validator
exportsShouldBeValid Module { exports, imports, functions, mems, tables, globals } =
    areExportNamesUnique <> foldMap isExportValid exports
    where
        funcImports = filter isFuncImport imports
        tableImports = filter isTableImport imports
        memImports = filter isMemImport imports
        globalImports = filter isGlobalImport imports

        isExportValid :: Export -> ValidationResult
        isExportValid (Export _ (ExportFunc funIdx)) =
            if fromIntegral funIdx < length funcImports + length functions then return () else Left (FunctionIndexOutOfRange funIdx)
        isExportValid (Export _ (ExportTable tableIdx)) =
            if fromIntegral tableIdx < length tableImports + length tables then return () else Left (TableIndexOutOfRange tableIdx)
        isExportValid (Export _ (ExportMemory memIdx)) =
            if fromIntegral memIdx < length memImports + length mems then return () else Left (MemoryIndexOutOfRange memIdx)
        isExportValid (Export _ (ExportGlobal globalIdx)) =
            if fromIntegral globalIdx < length globalImports + length globals
            then return ()
            else Left (GlobalIndexOutOfRange globalIdx)

        areExportNamesUnique :: ValidationResult
        areExportNamesUnique =
            case foldl' go (Set.empty, []) exports of
                (_, []) -> return ()
                (_, dup) -> Left $ DuplicatedExportNames dup
            where
                go :: (Set.Set TL.Text, [String]) -> Export -> (Set.Set TL.Text, [String])
                go (set, dup) (Export name _) =
                    if Set.member name set
                    then (set, show name : dup)
                    else (Set.insert name set, dup)

importsShouldBeValid :: Validator
importsShouldBeValid Module { imports, types } =
    foldMap isImportValid imports
    where
        isImportValid :: Import -> ValidationResult
        isImportValid (Import _ _ (ImportFunc typeIdx)) =
            if fromIntegral typeIdx < length types
            then return ()
            else Left TypeIndexOutOfRange
        isImportValid (Import _ _ (ImportTable _)) = return () -- checked in tables section
        isImportValid (Import _ _ (ImportMemory _)) = return () -- checked in mems section
        isImportValid (Import _ _ (ImportGlobal _)) = return ()

newtype ValidModule = ValidModule { getModule :: Module } deriving (Show, Eq)

validate :: Module -> Either ValidationError ValidModule
validate mod = const (ValidModule mod) <$> foldMap ($ mod) validators
    where
        validators :: [Validator]
        validators = [
                functionsShouldBeValid,
                tablesShouldBeValid,
                memoryShouldBeValid,
                globalsShouldBeValid,
                elemsShouldBeValid,
                datasShouldBeValid,
                startShouldBeValid,
                exportsShouldBeValid,
                importsShouldBeValid
            ]
