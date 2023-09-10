{
{-# LANGUAGE FlexibleContexts #-}

module Language.Wasm.Lexer (
    Lexeme(..),
    Token(..),
    AlexPosn(..),
    FloatRep(..),
    NaN(..),
    scanner,
    asFloat,
    asDouble,
    doubleFromInteger
) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import qualified Data.ByteString.Lazy.UTF8 as LBSUtf8
import Control.Monad (when)
import Numeric.IEEE (infinity, nan, nanWithPayload)
import Language.Wasm.FloatUtils (makeNaN, doubleToFloat, wordToFloat, wordToDouble)
import Data.Word (Word8, Word64)
import Data.List (isPrefixOf)
import Text.Read (readEither)
import Data.Bits
import Numeric (showHex)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

}

%wrapper "monadUserState-bytestring"

$digit     = [0-9]
$hexdigit  = [$digit a-f A-F]
$lower     = [a-z]
$upper     = [A-Z]
$alpha     = [$lower $upper]
$namepunct = [\! \# \$ \% \& \' \* \+ \- \. \/ \: \< \= \> \? \@ \∖ \^ \_ \` \| \~]
$idchar    = [$digit $alpha $namepunct]
$space     = [\  \x09 \x0A \x0D]
$linechar  = [^ \x09]
$sign      = [\+ \-]
$doublequote = \"

@keyword     = $lower $idchar*
@reserved    = $idchar+
@linecomment = ";;" $linechar* \x0A
@startblockcomment = "(;"
@endblockcomment = ";)"
@num = $digit (\_? $digit+)*
@hexnum = $hexdigit (\_? $hexdigit+)*
@id = "$" $idchar+
@floatfrac = @num "." (@num)?
@exp = [Ee] $sign? @num
@scientificint = @num @exp
@scientificfloat = @floatfrac @exp
@float = @floatfrac | @scientificint | @scientificfloat
@hexfloatfrac = "0x" @hexnum "." (@hexnum)?
@hexexp = [Pp] $sign? @num
@hexscientificint = "0x" @hexnum @hexexp
@hexscientificfloat = @hexfloatfrac @hexexp
@hexfloat = @hexfloatfrac | @hexscientificint | @hexscientificfloat
@nanhex = "nan:0x" @hexnum

tokens :-

<0> $space                                ;
<0> "nan"                                 { constToken $ TFloatLit $ BinRep (abs nan) }
<0> "+nan"                                { constToken $ TFloatLit $ BinRep (abs nan) }
<0> "-nan"                                { constToken $ TFloatLit $ BinRep nan }
<0> "nan:canonical"                       { constToken $ TFloatLit $ NanRep Canonical }
<0> "nan:arithmetic"                      { constToken $ TFloatLit $ NanRep Arithmetic }
<0> $sign? @nanhex                        { parseNanSigned }
<0> "inf"                                 { constToken $ TFloatLit $ BinRep inf }
<0> "+inf"                                { constToken $ TFloatLit $ BinRep inf }
<0> "-inf"                                { constToken $ TFloatLit $ BinRep minusInf }
<0> @keyword                              { tokenStr TKeyword }
<0> @linecomment                          ;
<0> @id                                   { tokenStr TId }
<0> "("                                   { constToken TOpenBracket }
<0> ")"                                   { constToken TCloseBracket }
<0> $sign? @hexfloat                      { parseHexFloat }
<0> $sign? @num                           { parseDecimalSignedInt }
<0> $sign? "0x" @hexnum                   { parseHexalSignedInt }
<0> $sign? @float                         { parseDecFloat }
<0, blockComment> @startblockcomment      { startBlockComment }
<blockComment> [.\n]                      ;
<blockComment> @endblockcomment           { endBlockComment }
<0> $doublequote                          { startStringLiteral }
<stringLiteral> \\ $hexdigit $hexdigit    { appendDoubleHexChar }
<stringLiteral> \\t                       { appendCharToStringLiteral '\x09' }
<stringLiteral> \\n                       { appendCharToStringLiteral '\x0A' }
<stringLiteral> \\r                       { appendCharToStringLiteral '\x0D' }
<stringLiteral> \\\"                      { appendCharToStringLiteral '\x22' }
<stringLiteral> \\\'                      { appendCharToStringLiteral '\x27' }
<stringLiteral> \\\\                      { appendCharToStringLiteral '\x5C' }
<stringLiteral> \\n\{ @hexnum \}          { appendHexEscapedChar }
<stringLiteral> $doublequote              { endStringLiteral }
<stringLiteral> . / {isAllowedStringChar} { appendFromHead }
<0> @reserved                             { tokenStr TReserved }

{

{- Lexem Helpers -}

defaultStartCode :: Int
defaultStartCode = 0

-- inner string literal character predicate
isAllowedStringChar :: user -> AlexInput -> Int -> AlexInput -> Bool
isAllowedStringChar _userState (_pos, _rest, inp, _) _len _nextInp =
    let Just (char, _) = LBSUtf8.decode inp in
    let code = Char.ord char in
    code >= 0x20 && code /= 0x7f && char /= '"' && char /= '\\'

minusNaN, inf, minusInf :: Double
minusNaN = negate nan
inf = infinity
minusInf = -infinity

parseSign :: (Num a) => LBS.ByteString -> ((a -> a), Int64, Maybe Bool)
parseSign str =
    let Just (ch, _) = LBSUtf8.decode str in
    case ch of
        '-' -> (negate, 1, Just True)
        '+' -> (abs, 1, Just False)
        otherwise -> (abs, 0, Nothing)

{-# SPECIALIZE parseSign :: LBS.ByteString -> ((Integer -> Integer), Int64, Maybe Bool) #-}
{-# SPECIALIZE parseSign :: LBS.ByteString -> ((Double -> Double), Int64, Maybe Bool) #-}

parseHexalSignedInt :: AlexAction Lexeme
parseHexalSignedInt = token $ \(pos, _, s, _) len -> 
    let (sign, slen, nat) = parseSign s in
    let num = readHexFromPrefix (len - 2 - slen) $ LBSUtf8.drop (2 + slen) s in
    Lexeme (Just pos) $ TIntLit nat $ sign num

parseNanSigned :: AlexAction Lexeme
parseNanSigned = token $ \(pos, _, s, _) len -> 
    let (sign, slen) = case LBSUtf8.decode s of
            Just ('-', _) -> (False, 1)
            Just ('+', _) -> (True, 1)
            otherwise -> (True, 0)
    in
    let num = readHexFromPrefix (len - 6 - slen) $ LBSUtf8.drop (6 + slen) s in
    Lexeme (Just pos) $ TFloatLit $ NanRep $ NanHex sign $ fromIntegral num

parseDecimalSignedInt :: AlexAction Lexeme
parseDecimalSignedInt = token $ \(pos, _, s, _) len ->
    let (sign, slen, nat) = parseSign s in
    let num = readDecFromPrefix (len - slen) $ LBSUtf8.drop slen s in
    Lexeme (Just pos) $ TIntLit nat $ sign num

parseDecFloat :: AlexAction Lexeme
parseDecFloat = token $ \(pos, _, s, _) len ->
    Lexeme (Just pos) $ TFloatLit $ DecRep $ filter (/= '_') $ takeChars len s

expAsInt :: String -> Int
expAsInt [] = 0
expAsInt ('+' : rest) = expAsInt rest
expAsInt ('-' : rest) = negate $ expAsInt rest
expAsInt str = read str

readDecFloat :: String -> Either String Float
readDecFloat str =
    let (sign, rest) = case str of
            ('+':rest) -> (abs, rest)
            ('-':rest) -> (negate, rest)
            rest -> (abs, rest)
    in
    let (val, exp) = splitBy (\c -> c == 'E' || c == 'e') rest in
    let (int, frac) = splitBy (== '.') val in
    let nullIfEmpty str = if null str then "0" else str in
    let expInt = expAsInt $ nullIfEmpty exp in
    if expInt > 38
    then Left $ "constant out of range"
    else fmap sign $ readEither $ nullIfEmpty int ++ "." ++ nullIfEmpty frac ++ "e" ++ nullIfEmpty exp

readDecDouble :: String -> Either String Double
readDecDouble str =
    let (sign, rest) = case str of
            ('+':rest) -> (abs, rest)
            ('-':rest) -> (negate, rest)
            rest -> (abs, rest)
    in
    let (val, exp) = splitBy (\c -> c == 'E' || c == 'e') rest in
    let (int, frac) = splitBy (== '.') val in
    let nullIfEmpty str = if null str then "0" else str in
    let expInt = expAsInt $ nullIfEmpty exp in
    if expInt > 308
    then Left $ "constant out of range"
    else fmap sign $ readEither $ nullIfEmpty int ++ "." ++ nullIfEmpty frac ++ "e" ++ nullIfEmpty exp

parseHexFloat :: AlexAction Lexeme
parseHexFloat = token $ \(pos, _, s, _) len ->
    Lexeme (Just pos) $ TFloatLit $ HexRep $ filter (/= '_') $ takeChars len s

readHexFloat :: (Integral w, Bits w) => (w -> f) -> Int -> Int -> Int -> String -> Either String f
readHexFloat toFloat sz expLimit manitisaSize str = do
    let (sign, '0':'x':rest) = case str of
            ('+':rest) -> (0, rest)
            ('-':rest) -> (1 `shiftL` (sz - 1), rest)
            rest -> (0, rest)
    let (val, expStr) = splitBy (\c -> c == 'P' || c == 'p') rest
    let (intRaw, fracRaw) = splitBy (== '.') val
    let int = dropWhile (== '0') intRaw
    let fracWithZeros = int ++ (reverse $ dropWhile (== '0') $ reverse fracRaw)
    let frac = dropWhile (== '0') fracWithZeros
    let exp = expAsInt expStr + length int * 4 - (length $ takeWhile (== '0') fracWithZeros) * 4
    if length frac == 0
    then return $ toFloat sign
    else do
        let fracBits = reverse $ dropWhile (== False) $ reverse $ toBits frac
        let exp' = exp - (length $ takeWhile (== False) fracBits) - 1
        let bits = dropWhile (== False) fracBits
        let budget = min (manitisaSize + 1) $ (expLimit + manitisaSize) + exp'
        let (bits', a, exp'') = if length bits <= budget
                then (bits, 0, exp')
                else do
                    let rounded = take budget bits
                    let rest = drop budget bits
                    if head rest == True && (length rest > 1 || (length rounded > 0 && last rounded == True))
                    then do
                        if length rounded > 0 && all (== True) rounded
                        then ([True], 0, exp' + 1)
                        else (rounded, 1, exp')
                    else (rounded, 0, exp')
        e <- if exp'' > expLimit then Left "const out of range"
                else if exp'' < (negate $ expLimit + manitisaSize) then return $ negate $ expLimit + manitisaSize + 1
                else return exp''
        if e >= (negate $ expLimit - 1)
        then return $ toFloat $ sign .|. ((fromIntegral $ e + expLimit) `shiftL` manitisaSize) .|. ((fromBits (tail bits') + a) `shiftL` (manitisaSize + 1 - length bits'))
        else do
            let shift = expLimit + manitisaSize - length bits' - abs e
            if shift < 0
            then return $ toFloat sign
            else return $ toFloat $ sign .|. ((fromBits bits' + a) `shiftL` shift)

type BitString = [Bool]

toBits :: String -> BitString
toBits = concat . map (asBits . readHexFromChar)
    where
        asBits :: Word8 -> BitString
        asBits w = [
                if w .&. 8 == 0 then False else True,
                if w .&. 4 == 0 then False else True,
                if w .&. 2 == 0 then False else True,
                if w .&. 1 == 0 then False else True
            ]

fromBits :: (Integral i) => BitString -> i
fromBits = foldr (\b acc -> acc * 2 + if b then 1 else 0) 0 . reverse . dropWhile (== False)

asFloat :: FloatRep -> Either String Float
asFloat (BinRep d) = Right $ doubleToFloat d
asFloat (HexRep s) = readHexFloat wordToFloat 32 127 23 s
asFloat (DecRep s) = readDecFloat s
asFloat (NanRep Canonical) = Right nan
asFloat (NanRep Arithmetic) = Right nan
asFloat (NanRep (NanHex isPos payload)) =
    if payload >= 1 && payload < 2 ^ 23
    then return $ doubleToFloat $ (if isPos then id else negate) $ makeNaN payload
    else Left "constant out of range"

asDouble :: FloatRep -> Either String Double
asDouble (BinRep d) = Right d
asDouble (HexRep s) = readHexFloat wordToDouble 64 1023 52 s
asDouble (DecRep s) = readDecDouble s
asDouble (NanRep Canonical) = Right nan
asDouble (NanRep Arithmetic) = Right nan
asDouble (NanRep (NanHex isPos payload)) =
    if payload >= 1 && payload < 2 ^ 52
    then return $ (if isPos then id else negate) $ makeNaN payload
    else Left "constant out of range"

doubleFromInteger :: Integer -> Either String Double
doubleFromInteger int = asDouble . HexRep . ((if int < 0 then "-0x" else "0x") ++) . flip showHex "" $ abs int

startBlockComment :: AlexAction Lexeme
startBlockComment _inp _len = do
    depth <- getLexerCommentDepth
    if depth <= 0
    then do
        alexSetStartCode blockComment
        setLexerCommentDepth 1
    else
        setLexerCommentDepth (depth + 1)
    alexMonadScan

endBlockComment :: AlexAction Lexeme
endBlockComment _inp _len = do
    depth <- getLexerCommentDepth
    if depth == 1
    then do
        alexSetStartCode defaultStartCode
        setLexerCommentDepth 0
    else
        setLexerCommentDepth (depth - 1)
    alexMonadScan

startStringLiteral :: AlexAction Lexeme
startStringLiteral (_, prev, _, _) _len = do
    when (prev `notElem` "() \x09\x0A\x0D")
        $ alexError "string literal should start after space or parent character"
    alexSetStartCode stringLiteral
    setLexerStringFlag True
    alexMonadScan

appendCharToStringLiteral :: Char -> AlexAction Lexeme
appendCharToStringLiteral chr _inp _len = do
    addCharToLexerStringValue chr
    alexMonadScan

appendFromHead :: AlexAction Lexeme
appendFromHead (_pos, _rest, inp, _) _len = do
    let Just (first, _) = LBSUtf8.decode inp
    addCharToLexerStringValue first
    alexMonadScan

appendDoubleHexChar :: AlexAction Lexeme
appendDoubleHexChar (_pos, _rest, inp, _) _len = do
    addCharCodeToLexerStringValue $ fromIntegral $ readHexFromPrefix 2 $ LBSUtf8.drop 1 inp
    alexMonadScan

-- TODO: add a predicate with code ranges check
-- if 𝑛 < 0xD800 ∨ 0xE000 ≤ 𝑛 < 0x110000
appendHexEscapedChar :: AlexAction Lexeme
appendHexEscapedChar (pos, _rest, inp, _) len = do
    let code = readHexFromPrefix (len - 3) $ LBSUtf8.drop 2 inp
    if code < 0xD800 || (code >= 0xE000 && code < 0x110000)
    then do
        addCharToLexerStringValue $ Char.chr $ fromIntegral code
        alexMonadScan
    else
        alexError $ "Character code should be in valid UTF range (code < 0xD800 || (code >= 0xE000 && code < 0x110000)): " ++ show pos

endStringLiteral :: AlexAction Lexeme
endStringLiteral (pos, _, _inp, _) _len = do
    alexSetStartCode defaultStartCode
    setLexerStringFlag False
    str <- LBS.pack . reverse <$> getLexerStringValue
    setLexerStringValue []
    return $ Lexeme (Just pos) $ TStringLit str

tokenStr :: (LBS.ByteString -> Token) -> AlexAction Lexeme
tokenStr f = token $ \(pos, _, s, _) len -> (Lexeme (Just pos) $ f $ LBS.take len s)

constToken :: Token -> AlexAction Lexeme
constToken tok = token $ \(pos, _, _, _) _len -> (Lexeme (Just pos) tok)

{- End Lexem Helpers -}

data FloatRep
    = BinRep Double
    | DecRep String
    | HexRep String
    | NanRep NaN
    deriving (Show, Eq)

data NaN
    = Canonical
    | Arithmetic
    | NanHex Bool Word64
    deriving (Show, Eq)

data Token = TKeyword LBS.ByteString
    | TIntLit {- Natural -} (Maybe Bool) Integer
    | TFloatLit FloatRep
    | TStringLit LBS.ByteString
    | TId LBS.ByteString
    | TOpenBracket
    | TCloseBracket
    | TReserved LBS.ByteString
    | EOF
    deriving (Show, Eq)

data Lexeme = Lexeme { pos :: Maybe AlexPosn, tok :: Token } deriving (Show, Eq)

data AlexUserState = AlexUserState {
        lexerCommentDepth :: Int,
        lexerStringValue  :: [Word8],
        lexerIsString     :: Bool
    }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
        lexerCommentDepth  = 0,
        lexerIsString      = False,
        lexerStringValue   = []
    }

getLexerCommentDepth :: Alex Int
getLexerCommentDepth = Alex $ \s@AlexState{alex_ust=ust} ->
    Right (s, lexerCommentDepth ust)

setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth ss = Alex $ \s ->
    Right (s{ alex_ust=(alex_ust s){ lexerCommentDepth = ss } }, ())

getLexerStringFlag :: Alex Bool
getLexerStringFlag = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerIsString ust)

setLexerStringFlag :: Bool -> Alex ()
setLexerStringFlag isString = Alex $ \s ->
    Right (s{ alex_ust=(alex_ust s){ lexerIsString = isString } }, ())

getLexerStringValue :: Alex [Word8]
getLexerStringValue = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringValue ust)

setLexerStringValue :: [Word8] -> Alex ()
setLexerStringValue ss = Alex $ \s ->
    Right (s{ alex_ust=(alex_ust s){ lexerStringValue = ss } }, ())

addCharToLexerStringValue :: Char -> Alex ()
addCharToLexerStringValue c = Alex $ \s ->
    let ust = alex_ust s in
    Right (s{ alex_ust = ust{ lexerStringValue = (reverse $ LBS.unpack $ LBSUtf8.fromString [c]) ++ lexerStringValue ust } }, ())

addCharCodeToLexerStringValue :: Word8 -> Alex ()
addCharCodeToLexerStringValue c = Alex $ \s ->
    let ust = alex_ust s in
    Right (s{ alex_ust = ust{ lexerStringValue = c : lexerStringValue ust } }, ())

alexEOF = return $ Lexeme Nothing EOF

takeChars :: Int64 -> LBS.ByteString -> String
takeChars n str = reverse $ go n str []
    where
        go :: Int64 -> LBS.ByteString -> String -> String
        go 0 _ acc = acc
        go n str acc = case LBSUtf8.uncons str of
            Just (c, rest) -> go (n - 1) rest (c : acc)
            Nothing -> acc

readHexFromChar :: (Num a) => Char -> a
readHexFromChar chr =
    case chr of 
        '0' -> 0 
        '1' -> 1 
        '2' -> 2 
        '3' -> 3 
        '4' -> 4 
        '5' -> 5 
        '6' -> 6 
        '7' -> 7 
        '8' -> 8 
        '9' -> 9 
        'A' -> 10 
        'B' -> 11 
        'C' -> 12 
        'D' -> 13 
        'E' -> 14 
        'F' -> 15
        'a' -> 10 
        'b' -> 11 
        'c' -> 12 
        'd' -> 13 
        'e' -> 14 
        'f' -> 15
        otherwise -> 0

{-# SPECIALIZE readHexFromChar :: Char -> Integer #-}
{-# SPECIALIZE readHexFromChar :: Char -> Double #-}

readFromPrefix :: Int -> Int64 -> LBS.ByteString -> Integer
readFromPrefix base n bstr
    | base <= 16 =
        let str = filter (/= '_') $ takeChars n bstr in
        let len = length str in
        sum $ zipWith (\i c -> readHexFromChar c * (fromIntegral base ^ fromIntegral (len - i))) [1..] str
    | otherwise = error "base has to be less than or equal 16"

readHexFromPrefix :: Int64 -> LBS.ByteString -> Integer
readHexFromPrefix = readFromPrefix 16

readDecFromPrefix :: Int64 -> LBS.ByteString -> Integer
readDecFromPrefix = readFromPrefix 10

splitBy :: (Char -> Bool) -> String -> (String, String)
splitBy pred str =
    case break pred str of
        (left, (_ : rest)) -> (left, rest)
        res -> res

scanner :: LBS.ByteString -> Either String [Lexeme]
scanner str = runAlex str loop
    where
        loop :: Alex [Lexeme]
        loop = do
            lex <- alexMonadScan
            case lex of
                Lexeme _ EOF -> do
                    strFlag <- getLexerStringFlag
                    when strFlag $ alexError "End of file reached before string literal end"
                    commentDepth <- getLexerCommentDepth
                    when (commentDepth > 0) $ alexError "End of file reached before block comment end"
                    return [lex]
                otherwise -> (lex :) <$> loop
}