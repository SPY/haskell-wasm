{
module Language.Wasm.Lexer ( 
    scan,
    alexScan
) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSChar8
import qualified Data.Char as Char

}

%wrapper "monadUserState-bytestring"

$digit     = [0-9]
$hexdigit  = [$digit a-f A-F]
$lower     = [a-z]
$upper     = [A-Z]
$alpha     = [$lower $upper]
$namepunct = [\! \# \$ \% \& \′ \* \+ \− \. \/ \: \< \= \> \? \@ \∖ \^ \_ \` \| \~]
$idchar    = [$digit $alpha $namepunct]
$space     = [\  \x09 \x0A \x0D]
$linechar  = [^ \x09]

@keyword     = $lower $idchar*
@reserved    = $idchar+
@linecomment = ";;" $linechar* \x0A
@startblockcomment = "(;"
@endblockcomment = "(;"
@num = $digit (\_? $digit*)
@hexnum = $hexdigit (\_? $hexdigit*)
@unsignedint = @num | "0x" @hexnum

tokens :-

<0> $space                                ;
<0> @keyword                              { tokenStr TKeyword }
<0> @linecomment                          ;
<0> "("                                   { constToken TOpenBracket }
<0> "("                                   { constToken TCloseBracket }
<0> @num                                  { parseDecimalUnsignedInt }
<0> "0x" @hexnum                          { parseHexalUnsignedInt }
<0, blockComment> @startblockcomment      { startBlockComment }
<blockComment> [.\n]                      ;
<blockComment> @endblockcomment           { endBlockComment }
<0> \"                                    { startStringLiteral }
<stringLiteral> \\ $hexdigit $hexdigit    { appendDoubleHexChar }
<stringLiteral> \\t                       { appendCharToStringLiteral '\x09' }
<stringLiteral> \\n                       { appendCharToStringLiteral '\x0A' }
<stringLiteral> \\r                       { appendCharToStringLiteral '\x0D' }
<stringLiteral> \\\"                      { appendCharToStringLiteral '\x22' }
<stringLiteral> \\\'                      { appendCharToStringLiteral '\x27' }
<stringLiteral> \\\\                      { appendCharToStringLiteral '\x5C' }
<stringLiteral> \\n\{ @hexnum \}          { appendHexEscapedChar }
<stringLiteral> . / {isAllowedStringChar} { appendFromHead }
<stringLiteral> \"                        { endStringLiteral }
<0> @reserved                             { tokenStr TReserved }

{

{- Lexem Helpers -}

defaultStartCode :: Int
defaultStartCode = 0

-- inner string literal character predicate
isAllowedStringChar :: user -> AlexInput -> Int -> AlexInput -> Bool
isAllowedStringChar _userState _prevInp _len (_pos, _rest, inp, _) =
    let char = LBSChar8.head inp in
    let code = Char.ord char in
    code >= 0x20 && code /= 0x7f && char /= '"' && char /= '\\'

parseHexalUnsignedInt :: AlexAction Lexeme
parseHexalUnsignedInt = token $ \(pos, _, s, _) len -> 
    let num = readHexFromPrefix (fromIntegral len - 2) $ dropChars 2 s in
    Lexeme pos $ TUnsignIntLit $ fromIntegral num

parseDecimalUnsignedInt :: AlexAction Lexeme
parseDecimalUnsignedInt = token $ \(pos, _, s, _) len -> 
    let num = readDecFromPrefix (fromIntegral len) $ dropChars 2 s in
    Lexeme pos $ TUnsignIntLit $ fromIntegral num

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
startStringLiteral _inp _len = do
    alexSetStartCode stringLiteral
    setLexerStringFlag True
    alexMonadScan

appendCharToStringLiteral :: Char -> AlexAction Lexeme
appendCharToStringLiteral chr _inp _len = do
    addCharToLexerStringValue chr
    alexMonadScan

appendFromHead :: AlexAction Lexeme
appendFromHead (_pos, _rest, inp, _) _len = do
    addCharToLexerStringValue $ LBSChar8.head inp
    alexMonadScan

appendDoubleHexChar :: AlexAction Lexeme
appendDoubleHexChar (_pos, _rest, inp, _) _len = do
    addCharToLexerStringValue $ Char.chr $ readHexFromPrefix 2 $ dropChars 1 inp
    alexMonadScan

-- TODO: add a predicate with code ranges check
-- if 𝑛 < 0xD800 ∨ 0xE000 ≤ 𝑛 < 0x110000
appendHexEscapedChar :: AlexAction Lexeme
appendHexEscapedChar (pos, _rest, inp, _) len = do
    let code = readHexFromPrefix (fromIntegral len - 3) $ dropChars 2 inp
    if code < 0xD800 || (code >= 0xE000 && code < 0x110000)
    then do
        addCharToLexerStringValue $ Char.chr code
        alexMonadScan
    else
        alexError $ "Character code should be in valid UTF range (code < 0xD800 || (code >= 0xE000 && code < 0x110000)): " ++ show pos

endStringLiteral :: AlexAction Lexeme
endStringLiteral (pos, _, _inp, _) _len = do
    alexSetStartCode defaultStartCode
    setLexerStringFlag False
    str <- getLexerStringValue
    return $ Lexeme pos $ TStringLit str

tokenStr :: (LBS.ByteString -> Token) -> AlexAction Lexeme
tokenStr f = token $ \(pos, _, s, _) len -> (Lexeme pos $ f $ LBS.take len s)

constToken :: Token -> AlexAction Lexeme
constToken tok = token $ \(pos, _, _, _) _len -> (Lexeme pos tok)

{- End Lexem Helpers -}

data Token = TKeyword LBS.ByteString
    | TUnsignIntLit Integer
    | TSignIntLit Integer
    | TFloatLit Double
    | TStringLit LBS.ByteString
    | TId LBS.ByteString
    | TOpenBracket
    | TCloseBracket
    | TReserved LBS.ByteString
    | EOF
    deriving (Show, Eq)

data Lexeme = Lexeme { pos :: AlexPosn, tok :: Token } deriving (Show, Eq)

data AlexUserState = AlexUserState {
        lexerCommentDepth :: Int,
        lexerStringValue  :: LBS.ByteString,
        lexerIsString     :: Bool
    }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
        lexerCommentDepth  = 0,
        lexerIsString      = False,
        lexerStringValue   = LBS.empty
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

getLexerStringValue :: Alex LBS.ByteString
getLexerStringValue = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringValue ust)

setLexerStringValue :: LBS.ByteString -> Alex ()
setLexerStringValue ss = Alex $ \s ->
    Right (s{ alex_ust=(alex_ust s){ lexerStringValue = ss } }, ())

addCharToLexerStringValue :: Char -> Alex ()
addCharToLexerStringValue c = Alex $ \s ->
    let ust = alex_ust s in
    Right (s{ alex_ust = ust{ lexerStringValue = LBSChar8.cons c (lexerStringValue ust) } }, ())

alexEOF = return $ Lexeme undefined EOF

takeChars :: Int -> LBS.ByteString -> String
takeChars n str = reverse $ go n str []
    where
        go :: Int -> LBS.ByteString -> String -> String
        go 0 _ acc = acc
        go n str acc =
            let Just (ch, rest) = LBSChar8.uncons str in
            go (n - 1) rest (ch : acc)

dropChars :: Int -> LBS.ByteString -> LBS.ByteString
dropChars 0 str = str
dropChars n str =
    let Just (_, rest) = LBSChar8.uncons str in
    dropChars (n - 1) rest

readHexFromChar :: Char -> Int
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

readFromPrefix :: Int -> Int -> LBS.ByteString -> Int
readFromPrefix base n bstr
    | base <= 16 =
        let str = filter (/= '_') $ takeChars n bstr in
        let len = length str in
        sum $ zipWith (\i c -> readHexFromChar c * (base ^ len - i)) [1..] str
    | otherwise = error "base has to be less than or equal 16"

readHexFromPrefix :: Int -> LBS.ByteString -> Int
readHexFromPrefix = readFromPrefix 16

readDecFromPrefix :: Int -> LBS.ByteString -> Int
readDecFromPrefix = readFromPrefix 10

scan :: LBS.ByteString -> [Lexeme]
scan = undefined

}