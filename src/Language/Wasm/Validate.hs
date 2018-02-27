{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Wasm.Validate (
    ValidationResult(..),
    validate,
    isValid
) where

import Language.Wasm.Structure
import qualified Data.Set as Set
import Data.List (foldl')
import qualified Data.Text.Lazy as TL
import Data.Maybe (fromMaybe, maybeToList, catMaybes, isNothing)
import Data.Monoid ((<>))
import Numeric.Natural (Natural)

import Control.Monad.State.Lazy (StateT, evalStateT, get, put)
import Control.Monad.Reader (ReaderT, runReaderT, withReaderT, ask)
import Control.Monad.Except (Except, runExcept, throwError)

import Debug.Trace as Debug

data ValidationResult =
    DuplicatedExportNames [String]
    | InvalidTableType
    | InvalidMemoryLimit
    | MoreThanOneMemory
    | MoreThanOneTable
    | IndexOutOfRange
    | ResultTypeDoesntMatch
    | NoTableInModule
    | NoMemoryInModule
    | TypeMismatch
    | InvalidConstantExpr
    | InvalidStartFunctionType
    | ImportedGlobalIsNotConst
    | Valid
    deriving (Show, Eq)

instance Monoid ValidationResult where
    mempty = Valid
    mappend Valid vr = vr
    mappend vr Valid = vr
    mappend vr _ = vr

isValid :: ValidationResult -> Bool
isValid Valid = True
isValid reason = Debug.trace ("Module mismatched with reason " ++ show reason) $ False

type Validator = Module -> ValidationResult

data VType =
    Val ValueType
    | Var Int
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
asArrow (FuncType params results) = Arrow (map Val params) (map Val results)

isArrowMatch :: Arrow -> Arrow -> Bool
isArrowMatch (f `Arrow` t) ( f' `Arrow` t') = isEndMatch f f' && isEndMatch t t'
    where
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
        isEndMatch (Var v:l) (x:r) =
            let subst = replace (Var v) x in
            isEndMatch (subst l) (subst r)
        isEndMatch (x:l) (Var v:r) =
            let subst = replace (Var v) x in
            isEndMatch (subst l) (subst r)
        isEndMatch (Val v:l) (Val v':r) = v == v' && isEndMatch l r
        isEndMatch [] [] = True
        isEndMatch _ _ = False

data Ctx = Ctx {
    types :: [FuncType],
    funcs :: [FuncType],
    tables :: [TableType],
    mems :: [Limit],
    globals :: [GlobalType],
    locals :: [ValueType],
    labels :: [Maybe ValueType],
    returns :: Maybe ValueType
} deriving (Show, Eq)

type Checker = ReaderT Ctx (StateT Int (Except ValidationResult))

freshVar :: Checker VType
freshVar = do
    i <- get
    put (i + 1)
    return $ Var i

runChecker :: Ctx -> Checker a -> Either ValidationResult a
runChecker ctx = runExcept . flip evalStateT 0 . flip runReaderT ctx

(!?) :: [a] -> Natural -> Maybe a
(!?) (x:_) 0 = Just x
(!?) (_:rest) n = rest !? (n - 1)
(!?) [] _ = Nothing

safeHead :: [a] -> Maybe a
safeHead (x: _) = Just x
safeHead [] = Nothing

maybeToEither :: ValidationResult -> Maybe a -> Checker a
maybeToEither _ (Just a) = return a
maybeToEither l Nothing = throwError l

isIndexValid :: (Integral idx, Integral len) => idx -> len -> ValidationResult
isIndexValid idx len = if fromIntegral idx < fromIntegral len then Valid else IndexOutOfRange

asType :: GlobalType -> VType
asType (Const v) = Val v
asType (Mut v) = Val v

getLabel :: LabelIndex -> Checker (Maybe ValueType)
getLabel lbl = do
    Ctx { labels } <- ask
    case labels !? lbl of
        Nothing -> throwError IndexOutOfRange
        Just v -> return v

withLabel :: [ValueType] -> Checker a -> Checker a
withLabel result = withReaderT (\ctx -> ctx { labels = safeHead result : labels ctx })

getInstrType :: Instruction -> Checker Arrow
getInstrType Unreachable = return $ Any ==> Any
getInstrType Nop = return $ empty ==> empty
getInstrType Block { result, body } = do
    let blockType = empty ==> result
    t <- withLabel result $ getExpressionType body
    if isArrowMatch t blockType
    then return $ empty ==> result
    else throwError TypeMismatch
getInstrType Loop { result, body } = do
    let blockType = empty ==> result
    t <- withLabel result $ getExpressionType body
    if isArrowMatch t blockType
    then return $ empty ==> result
    else throwError TypeMismatch
getInstrType If { result, true, false } = do
    let blockType = empty ==> result
    l <- withLabel result $ getExpressionType true
    r <- withLabel result $ getExpressionType false
    if isArrowMatch l blockType && isArrowMatch r blockType
    then return $ I32 ==> result
    else throwError TypeMismatch
getInstrType (Br lbl) = do
    r <- map Val . maybeToList <$> getLabel lbl
    return $ (Any : r) ==> Any
getInstrType (BrIf lbl) = do
    r <- map Val . maybeToList <$> getLabel lbl
    return $ (r ++ [Val I32]) ==> r
getInstrType (BrTable lbls lbl) = do
    r <- getLabel lbl
    rs <- mapM getLabel lbls
    -- this check for equality doesn't match the spec,
    -- but a reference compiler does the same
    if all (\r' -> r' == r || isNothing r' || isNothing r) rs
    then return $ ([Any] ++ (map Val $ maybeToList r) ++ [Val I32]) ==> Any
    else throwError ResultTypeDoesntMatch
getInstrType Return = do
    Ctx { returns } <- ask
    return $ (Any : (map Val $ maybeToList returns)) ==> Any
getInstrType (Call fun) = do
    Ctx { funcs } <- ask
    maybeToEither IndexOutOfRange $ asArrow <$> funcs !? fun
getInstrType (CallIndirect sign) = do
    Ctx { types, tables } <- ask
    if length tables < 1
    then throwError NoTableInModule
    else do
        Arrow from to <- maybeToEither IndexOutOfRange $ asArrow <$> types !? sign
        return $ (from ++ [Val I32]) ==> to
getInstrType Drop = do
    var <- freshVar
    return $ var ==> empty
getInstrType Select = do
    var <- freshVar
    return $ [var, var, Val I32] ==> var
getInstrType (GetLocal local) = do
    Ctx { locals }  <- ask
    t <- maybeToEither IndexOutOfRange $ locals !? local
    return $ empty ==> Val t
getInstrType (SetLocal local) = do
    Ctx { locals } <- ask
    t <- maybeToEither IndexOutOfRange $ locals !? local
    return $ Val t ==> empty
getInstrType (TeeLocal local) = do
    Ctx { locals } <- ask
    t <- maybeToEither IndexOutOfRange $ locals !? local
    return $ Val t ==> Val t
getInstrType (GetGlobal global) = do
    Ctx { globals } <- ask
    t <- maybeToEither IndexOutOfRange $ asType <$> globals !? global
    return $ empty ==> t
getInstrType (SetGlobal global) = do
    Ctx { globals } <- ask
    t <- maybeToEither IndexOutOfRange $ asType <$> globals !? global
    return $ t ==> empty
-- TODO: check memory alignment
getInstrType (I32Load _) = do
    Ctx { mems } <- ask
    if length mems < 1 then throwError NoMemoryInModule else return $ I32 ==> I32
getInstrType (I64Load _) = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ I32 ==> I64
getInstrType (F32Load _) = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ I32 ==> F32
getInstrType (F64Load _) = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ I32 ==> F64
getInstrType (I32Load8S _) = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ I32 ==> I32
getInstrType (I32Load8U _) = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ I32 ==> I32
getInstrType (I32Load16S _) = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ I32 ==> I32
getInstrType (I32Load16U _) = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ I32 ==> I32
getInstrType (I64Load8S _) = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ I32 ==> I64
getInstrType (I64Load8U _) = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ I32 ==> I64
getInstrType (I64Load16S _) = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ I32 ==> I64
getInstrType (I64Load16U _) = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ I32 ==> I64
getInstrType (I64Load32S _) = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ I32 ==> I64
getInstrType (I64Load32U _) = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ I32 ==> I64
getInstrType (I32Store _) = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ [I32, I32] ==> empty
getInstrType (I64Store _) = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ [I32, I64] ==> empty
getInstrType (F32Store _) = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ [I32, F32] ==> empty
getInstrType (F64Store _) = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ [I32, F64] ==> empty
getInstrType (I32Store8 _) = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ [I32, I32] ==> empty
getInstrType (I32Store16 _) = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ [I32, I32] ==> empty
getInstrType (I64Store8 _) = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ [I32, I64] ==> empty
getInstrType (I64Store16 _) = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ [I32, I64] ==> empty
getInstrType (I64Store32 _) = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ [I32, I64] ==> empty
getInstrType CurrentMemory = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ empty ==> I32
getInstrType GrowMemory = do
    Ctx { mems } <- ask 
    if length mems < 1 then throwError NoMemoryInModule else return $ I32 ==> I32
getInstrType (I32Const _) = return $ empty ==> I32
getInstrType (I64Const _) = return $ empty ==> I64
getInstrType (F32Const _) = return $ empty ==> F32
getInstrType (F64Const _) = return $ empty ==> F64
getInstrType (IUnOp BS32 _) = return $ I32 ==> I32
getInstrType (IUnOp BS64 _) = return $ I64 ==> I64
getInstrType (IBinOp BS32 _) = return $ [I32, I32] ==> I32
getInstrType (IBinOp BS64 _) = return $ [I64, I64] ==> I64
getInstrType I32Eqz = return $ I32 ==> I32
getInstrType I64Eqz = return $ I64 ==> I32
getInstrType (IRelOp BS32 _) = return $ [I32, I32] ==> I32
getInstrType (IRelOp BS64 _) = return $ [I64, I64] ==> I32
getInstrType (FUnOp BS32 _) = return $ F32 ==> F32
getInstrType (FUnOp BS64 _) = return $ F64 ==> F64
getInstrType (FBinOp BS32 _) = return $ [F32, F32] ==> F32
getInstrType (FBinOp BS64 _) = return $ [F64, F64] ==> F64
getInstrType (FRelOp BS32 _) = return $ [F32, F32] ==> I32
getInstrType (FRelOp BS64 _) = return $ [F64, F64] ==> I32
getInstrType I32WrapI64 = return $ I64 ==> I32
getInstrType (ITruncFU BS32 BS32) = return $ F32 ==> I32
getInstrType (ITruncFU BS32 BS64) = return $ F64 ==> I32
getInstrType (ITruncFU BS64 BS32) = return $ F32 ==> I64
getInstrType (ITruncFU BS64 BS64) = return $ F64 ==> I64
getInstrType (ITruncFS BS32 BS32) = return $ F32 ==> I32
getInstrType (ITruncFS BS32 BS64) = return $ F64 ==> I32
getInstrType (ITruncFS BS64 BS32) = return $ F32 ==> I64
getInstrType (ITruncFS BS64 BS64) = return $ F64 ==> I64
getInstrType I64ExtendSI32 = return $ I32 ==> I64
getInstrType I64ExtendUI32 = return $ I32 ==> I64
getInstrType (FConvertIU BS32 BS32) = return $ I32 ==> F32
getInstrType (FConvertIU BS32 BS64) = return $ I64 ==> F32
getInstrType (FConvertIU BS64 BS32) = return $ I32 ==> F64
getInstrType (FConvertIU BS64 BS64) = return $ I64 ==> F64
getInstrType (FConvertIS BS32 BS32) = return $ I32 ==> F32
getInstrType (FConvertIS BS32 BS64) = return $ I64 ==> F32
getInstrType (FConvertIS BS64 BS32) = return $ I32 ==> F64
getInstrType (FConvertIS BS64 BS64) = return $ I64 ==> F64
getInstrType F32DemoteF64 = return $ F64 ==> F32
getInstrType F64PromoteF32 = return $ F32 ==> F64
getInstrType (IReinterpretF BS32) = return $ F32 ==> I32
getInstrType (IReinterpretF BS64) = return $ F64 ==> I64
getInstrType (FReinterpretI BS32) = return $ I32 ==> F32
getInstrType (FReinterpretI BS64) = return $ I64 ==> F64


replace :: (Eq a) => a -> a -> [a] -> [a]
replace _ _ [] = []
replace x y (v:r) = (if x == v then y else v) : replace x y r

unify :: Arrow -> Arrow -> Checker Arrow
unify (from `Arrow` to) (from' `Arrow` to') =
    unify' (from `Arrow` reverse to) (reverse from' `Arrow` to')
    where
        unify' :: Arrow -> Arrow -> Checker Arrow
        unify' (f `Arrow` []) (f' `Arrow` t') =
            return $ (reverse f' ++ f) `Arrow` t'
        unify' (f `Arrow` t) ([] `Arrow` t') =
            return $ f `Arrow` (reverse t ++ t')
        unify' (f `Arrow` (Val v':t)) ((Val v:f') `Arrow` t') =
            if v == v'
            then unify' (f `Arrow` t) (f' `Arrow` t')
            else throwError TypeMismatch
        unify' (f `Arrow` (Var r:t)) ((Val v:f') `Arrow` t') =
            let subst = replace (Var r) (Val v) in
            unify' (subst f `Arrow` subst t) (f' `Arrow` t')
        unify' (f `Arrow` (Val v:t)) ((Var r:f') `Arrow` t') =
            let subst = replace (Var r) (Val v) in
            unify' (f `Arrow` t) (subst f' `Arrow` subst t')
        unify' (f `Arrow` (Var r:t)) ((Var r':f') `Arrow` t') =
            let subst = replace (Var r') (Var r) in
            unify' (f `Arrow` t) (subst f' `Arrow` subst t')
        unify' (f `Arrow` (Any:_)) (_ `Arrow` t') =
            return $ f `Arrow` (Any : t')
        unify' (f `Arrow` _) (f'@(Any:_) `Arrow` t') =
            return $ (f' ++ f) `Arrow` t'

getExpressionType :: [Instruction] -> Checker Arrow
getExpressionType instrs =
    case reverse instrs of
        [] -> return $ Any ==> Any
        (i:rest) -> do
            arr <- getInstrType i
            go arr rest
    where
        go :: Arrow -> [Instruction] -> Checker Arrow
        go arr [] = return arr
        go arr (i:rest) = do
            a <- getInstrType i
            arr' <- unify a arr
            go arr' rest

isConstExpression :: [Instruction] -> Checker ()
isConstExpression [] = return ()
isConstExpression ((I32Const _):rest) = isConstExpression rest
isConstExpression ((I64Const _):rest) = isConstExpression rest
isConstExpression ((F32Const _):rest) = isConstExpression rest
isConstExpression ((F64Const _):rest) = isConstExpression rest
isConstExpression ((GetGlobal idx):rest) = do
    Ctx {globals} <- ask
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

ctxFromModule :: [ValueType] -> [Maybe ValueType] -> Maybe ValueType -> Module -> Ctx
ctxFromModule locals labels returns m@Module {types, functions, tables, mems, globals, imports} =
    let tableImports = catMaybes $ map getTableType imports in
    let memsImports = catMaybes $ map getMemType imports in
    let globalImports = catMaybes $ map getGlobalType imports in
    Ctx {
        types,
        funcs = getFuncTypes m,
        tables = tableImports ++ map (\(Table t) -> t) tables,
        mems = memsImports ++ map (\(Memory l) -> l) mems,
        globals = globalImports ++ map (\(Global g _) -> g) globals,
        locals,
        labels,
        returns
    }
    where
        getTableType (Import _ _ (ImportTable tableType)) = Just tableType
        getTableType _ = Nothing

        getMemType (Import _ _ (ImportMemory lim)) = Just lim
        getMemType _ = Nothing

        getGlobalType (Import _ _ (ImportGlobal gl)) = Just gl
        getGlobalType _ = Nothing

isFunctionValid :: Function -> Validator
isFunctionValid Function {funcType, locals, body} mod@Module {types} =
    let FuncType params results = types !! fromIntegral funcType in
    let r = safeHead results in
    let ctx = ctxFromModule (params ++ locals) [r] r mod in
    case runChecker ctx $ getExpressionType body of
        Left err -> err
        Right arr ->
            if isArrowMatch arr (empty ==> results)
            then Valid
            else TypeMismatch

functionsShouldBeValid :: Validator
functionsShouldBeValid mod@Module {functions} =
    foldMap (flip isFunctionValid mod) functions

tablesShouldBeValid :: Validator
tablesShouldBeValid Module { imports, tables } =
    let tableImports = filter isTableImport imports in
    let res = foldMap (\Import { desc = ImportTable t } -> isValidTableType t) tableImports in
    let res' = foldl' (\r (Table t) -> r <> isValidTableType t) res tables in
    if length tableImports + length tables <= 1
        then res'
        else MoreThanOneTable
    where
        isValidTableType :: TableType -> ValidationResult
        isValidTableType (TableType (Limit min max) _) =
            if min <= fromMaybe min max
            then Valid
            else InvalidTableType

isTableImport :: Import -> Bool
isTableImport Import { desc = ImportTable _ } = True
isTableImport _ = False

memoryShouldBeValid :: Validator
memoryShouldBeValid Module { imports, mems } =
    let memImports = filter isMemImport imports in
    let res = foldMap (\Import { desc = ImportMemory l } -> isValidLimit l) memImports in
    let res' = foldl' (\r (Memory l) -> r <> isValidLimit l) res mems in
    if length memImports + length mems <= 1
        then res'
        else MoreThanOneMemory
    where
        isValidLimit :: Limit -> ValidationResult
        isValidLimit (Limit min max) = if min <= fromMaybe min max then Valid else InvalidMemoryLimit

isMemImport :: Import -> Bool
isMemImport Import { desc = ImportMemory _ } = True
isMemImport _ = False

globalsShouldBeValid :: Validator
globalsShouldBeValid m@Module { imports, globals } =
    let ctx = ctxFromModule [] [] Nothing m in
    foldMap (isGlobalValid ctx) globals
    where
        getGlobalType :: GlobalType -> ValueType
        getGlobalType (Const vt) = vt
        getGlobalType (Mut vt) = vt

        isGlobalValid :: Ctx -> Global -> ValidationResult
        isGlobalValid ctx (Global gt init) =
            let check = runChecker ctx $ do
                    isConstExpression init
                    t <- getExpressionType init
                    return $ isArrowMatch (empty ==> getGlobalType gt) t
            in
            case check of
                Left err -> err
                Right eq -> if eq then Valid else TypeMismatch

elemsShouldBeValid :: Validator
elemsShouldBeValid m@Module { elems, functions, tables, imports } =
    let ctx = ctxFromModule [] [] Nothing m in
    foldMap (isElemValid ctx) elems
    where
        isElemValid :: Ctx -> ElemSegment -> ValidationResult
        isElemValid ctx (ElemSegment tableIdx offset funs) =
            let check = runChecker ctx $ do
                    isConstExpression offset
                    t <- getExpressionType offset
                    return $ isArrowMatch (empty ==> I32) t
            in
            let isIniterValid = case check of
                    Left err -> err
                    Right eq -> if eq then Valid else TypeMismatch
            in
            let tableImports = filter isTableImport imports in
            let isTableIndexValid =
                    if tableIdx < (fromIntegral $ length tableImports + length tables)
                    then Valid
                    else IndexOutOfRange
            in
            let funsLength = fromIntegral $ length functions in
            let isFunsValid = foldMap (\i -> if i < funsLength then Valid else IndexOutOfRange) funs in
            isIniterValid <> isFunsValid <> isTableIndexValid

datasShouldBeValid :: Validator
datasShouldBeValid m@Module { datas, mems, imports } =
    let ctx = ctxFromModule [] [] Nothing m in
    foldMap (isDataValid ctx) datas
    where
        isDataValid :: Ctx -> DataSegment -> ValidationResult
        isDataValid ctx (DataSegment memIdx offset _) =
            let check = runChecker ctx $ do
                    isConstExpression offset
                    t <- getExpressionType offset
                    return $ isArrowMatch (empty ==> I32) t
            in
            let isOffsetValid = case check of
                    Left err -> err
                    Right eq -> if eq then Valid else TypeMismatch
            in
            let memImports = filter isMemImport imports in
            if memIdx < (fromIntegral $ length memImports + length mems)
            then isOffsetValid
            else IndexOutOfRange

startShouldBeValid :: Validator
startShouldBeValid Module { start = Nothing } = Valid
startShouldBeValid m@Module { start = Just (StartFunction idx) } =
    let types = getFuncTypes m in
    let i = fromIntegral idx in
    if length types > i
    then if FuncType [] [] == types !! i then Valid else InvalidStartFunctionType
    else IndexOutOfRange

exportsShouldBeValid :: Validator
exportsShouldBeValid Module { exports, imports, functions, mems, tables, globals } =
    areExportNamesUnique <> foldMap isExportValid exports
    where
        funcImports = filter isFuncImport imports
        tableImports = filter isTableImport imports
        memImports = filter isMemImport imports
        globalImports = filter isGlobalImport imports

        isFuncImport (Import _ _ (ImportFunc _)) = True
        isFuncImport _ = False

        isGlobalImport (Import _ _ (ImportGlobal _)) = True
        isGlobalImport _ = False

        isExportValid :: Export -> ValidationResult
        isExportValid (Export _ (ExportFunc funIdx)) =
            isIndexValid funIdx $ length funcImports + length functions
        isExportValid (Export _ (ExportTable tableIdx)) =
            isIndexValid tableIdx $ length tableImports + length tables
        isExportValid (Export _ (ExportMemory memIdx)) =
            isIndexValid memIdx $ length memImports + length mems
        isExportValid (Export _ (ExportGlobal globalIdx)) =
            isIndexValid globalIdx $ length globalImports + length globals

        areExportNamesUnique :: ValidationResult
        areExportNamesUnique =
            case foldl' go (Set.empty, []) exports of
                (_, []) -> Valid
                (_, dup) -> DuplicatedExportNames dup
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
        isImportValid (Import _ _ (ImportFunc typeIdx)) = isIndexValid typeIdx $ length types
        isImportValid (Import _ _ (ImportTable _)) = Valid -- checked in tables section
        isImportValid (Import _ _ (ImportMemory _)) = Valid -- checked in mems section
        isImportValid (Import _ _ (ImportGlobal (Const _))) = Valid
        isImportValid (Import _ _ (ImportGlobal (Mut _))) = ImportedGlobalIsNotConst

validate :: Validator
validate mod = foldMap ($ mod) validators
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
