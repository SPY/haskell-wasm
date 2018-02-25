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
import Data.List (foldl', isPrefixOf)
import qualified Data.Text.Lazy as TL
import Data.Maybe (fromMaybe, maybeToList)
import Data.Monoid ((<>))
import Numeric.Natural (Natural)

import Control.Monad.State.Lazy (StateT, evalStateT, get, put)
import Control.Monad.Reader (ReaderT, runReaderT, withReaderT, ask)
import Control.Monad.Except (Except, runExcept, throwError)

import Debug.Trace as Debug

data ValidationResult =
    DuplicatedExportNames [String]
    | InvalidTableType
    | MoreThanOneMemory
    | MoreThanOneTable
    | IndexOutOfRange
    | ResultTypeDoesntMatch
    | NoTableInModule
    | NoMemoryInModule
    | TypeMismatch
    | Valid
    deriving (Show, Eq)

instance Monoid ValidationResult where
    mempty = Valid
    mappend Valid vr = vr
    mappend vr Valid = vr
    mappend vr _ = vr

isValid :: ValidationResult -> Bool
isValid Valid = True
isValid _ = False

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
            let leftTail = takeWhile (/= Any) $ reverse l in
            let rightTail = takeWhile (/= Any) $ reverse r in
            leftTail `isPrefixOf` rightTail || rightTail `isPrefixOf` leftTail
        isEndMatch (Any:l) r =
            let leftTail = takeWhile (/= Any) $ reverse l in
            let rightTail = takeWhile (/= Any) $ reverse r in
            leftTail `isPrefixOf` rightTail || rightTail `isPrefixOf` leftTail
        isEndMatch l (Any:r) =
            let leftTail = takeWhile (/= Any) $ reverse l in
            let rightTail = takeWhile (/= Any) $ reverse r in
            leftTail `isPrefixOf` rightTail || rightTail `isPrefixOf` leftTail
        isEndMatch l r = l == r

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
    else Debug.trace ("block err " ++ show  t ++ " - " ++ show blockType) $throwError TypeMismatch
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
    if all (== r) rs
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
unify (f `Arrow` t) (f' `Arrow` t') = unify' (f `Arrow` reverse t) (reverse f' `Arrow` t')
    where
        unify' :: Arrow -> Arrow -> Checker Arrow
        unify' (f `Arrow` []) (f' `Arrow` t') =
            return $ (reverse f' ++ f) `Arrow` t'
        unify' (f `Arrow` t) ([] `Arrow` t') =
            return $ f `Arrow` (reverse t ++ t')
        unify' (f `Arrow` (Val v':t)) ((Val v:f') `Arrow` t') =
            if v == v'
            then unify' (f `Arrow` t) (f' `Arrow` t')
            else Debug.trace ("type err " ++ show  v' ++ " - " ++ show v) $ throwError TypeMismatch
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
            return $ f `Arrow` t'
        unify' (f `Arrow` _) ((Any:_) `Arrow` t') =
            return $ f `Arrow` t'

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

ctxFromModule :: [ValueType] -> [Maybe ValueType] -> Maybe ValueType -> Module -> Ctx
ctxFromModule locals labels returns Module {types, functions, tables, mems, globals} =
    Ctx {
        types,
        funcs = map ((types !!) . fromIntegral . funcType) functions,
        tables = map (\(Table t) -> t) tables,
        mems = map (\(Memory l) -> l) mems,
        globals = map (\(Global g _) -> g) globals,
        locals,
        labels,
        returns
    }

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
            else Debug.trace ("fun err " ++ show  arr ++ " - " ++ show (empty ==> results)) $ TypeMismatch

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
        isValidTableType (TableType (Limit min max) _) = if min <= fromMaybe min max then Valid else InvalidTableType

        isTableImport Import { desc = ImportTable _ } = True
        isTableImport _ = False

shouldBeAtMostOneMemory :: Validator
shouldBeAtMostOneMemory Module { imports, mems } =
    let memImports = filter isMemImport imports in
    if length memImports + length mems <= 1
    then Valid
    else MoreThanOneMemory
    where
        isMemImport Import { desc = ImportMemory _ } = True
        isMemImport _ = False

exportNamesShouldBeDifferent :: Validator
exportNamesShouldBeDifferent Module { exports } =
    case foldl' go (Set.empty, []) exports of
        (_, []) -> Valid
        (_, dup) -> DuplicatedExportNames dup
    where
        go :: (Set.Set TL.Text, [String]) -> Export -> (Set.Set TL.Text, [String])
        go (set, dup) (Export name _) =
            if Set.member name set
            then (set, show name : dup)
            else (Set.insert name set, dup)

validate :: Validator
validate mod = foldMap ($ mod) validators
    where
        validators :: [Validator]
        validators = [
                tablesShouldBeValid,
                shouldBeAtMostOneMemory,
                exportNamesShouldBeDifferent,
                functionsShouldBeValid
            ]
