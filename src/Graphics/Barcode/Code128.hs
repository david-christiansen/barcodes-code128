-- | A library for generating barcodes in Code 128. Details of the
-- encoding are available at <http://en.wikipedia.org/wiki/Code_128>.
-- The width of the stripes and the height of the barcode are
-- configurable. Two modes of operation are supported:
--
--     * Constructing a PDF directly in a 'ByteString', and
--     * Constructing an action in 'HPDF''s 'Draw' monad, for composing
--     the barcode with other information in a PDF.
module Graphics.Barcode.Code128 ( BarcodeConfig(..)
                                , Err(CantEncode)
                                , barcodeSize
                                , barcodePDF
                                , drawBarcode) where

import Control.Applicative ((<$>), pure)

import Data.ByteString.Lazy (ByteString)
import Data.Char (isDigit, digitToInt)
import Data.Foldable (foldlM)
import Data.Maybe (listToMaybe)

import Graphics.PDF

-------------------
-- Public interface
-------------------

-- | Controls the size of the generated barcode. All units are in
-- HPDF's internal sizing.
data BarcodeConfig = BarcodeConfig
  { height :: Double -- ^ The height of the generated barcode
  , barWidth :: Double -- ^ The width of the narrowest possible bar
  }

-- | Errors that can occur while constructing a barcode
data Err = CantEncode Char -- ^ The returned character cannot be encoded in Code 128
         | InternalError String -- ^ An internal error occurred, indicating a bug in the library
  deriving (Show)


-- | Determine the width and height of rendering a particular string
-- as a barcode in a particular configuration.
barcodeSize :: BarcodeConfig -> [Char] -> Either Err (Double, Double)
barcodeSize config str = do encoded <- encode str
                            return (width (length encoded), height config)
  where width i = quietZone +
                  codeWidth config * fromIntegral i +
                  quietZone
        quietZone = codeWidth config -- minimum according to standard

-- | Encode a string and construct an action in 'Draw' that will
-- render it as a barcode. The action, if created successfully, will
-- return the opposite corner of the barcode. An error is returned if the string
-- could not be encoded as a Code128 barcode.
drawBarcode :: BarcodeConfig -- ^ How to render the barcode
            -> String -- ^ The string to encode
            -> Point -- ^ The origin (lower left) of the barcode on the page
            -> Either Err (Draw Point)
drawBarcode config str start@(_ :+ y) =
  do codes <- encode str
     return $ do (x' :+ _) <- quietZone start
                 (x'' :+ _) <- drawChars config codes (x' :+ y)
                 quietZone (x'' :+ y)
  where quietWidth = codeWidth config
        quietZone (x :+ y) = do fillColor white
                                setWidth 0
                                let corner2 = (( x + quietWidth) :+
                                               (y + height config))
                                fill $ Rectangle (x :+ y)
                                                 corner2
                                return corner2

-- | Generate PDF data containing the barcode for a given string, with
-- the tightest possible bounding box. If the string cannot be encoded
-- in Code 128, an IOError is thrown.
barcodePDF :: BarcodeConfig -> String -> IO ByteString
barcodePDF config str =
  case barcodeSize config str of
    Left err     -> fail (show err)
    Right (w, h) ->
      pdfByteString standardDocInfo { compressed = False}
                    (PDFRect 0 0 (ceiling w) (ceiling h)) $ do
                      p <- addPage Nothing
                      drawWithPage p $ do
                        case drawBarcode config str (0 :+ 0) of
                          Left err   -> fail (show err)
                          Right code -> code


-----------------
-- Implementation
-----------------

-- | Barcode stripes come in four widths
data Code128Digit = One | Two | Three | Four
  deriving (Eq, Ord, Show)

-- | A single code in the scheme consists of six stripes, or the
-- special seven-stripe stop code
data Code128Code = Code128Code Code128Digit Code128Digit
                               Code128Digit Code128Digit
                               Code128Digit Code128Digit
                 | StopCode
  deriving (Eq, Ord, Show)


-- | The raw codes have a number of potential semantic
-- interpretations, depending on their position in the code
data Code128Interp = CharValue Char
                   | FNC1
                   | FNC2
                   | FNC3
                   | FNC4
                   | CodeA
                   | CodeB
                   | CodeC
                   | ShiftA
                   | ShiftB
                   | Digits Int Int -- ^ Each Int between 0 and 9 inclusive
                   | Stop
                   | StartA
                   | StartB
                   | StartC
  deriving (Eq, Show)

-- | A particular symbol has three potential interpretations,
-- depending on the present coding scheme, and a value for
-- checksumming
data Code128Symbol = Code128Symbol
  { code :: Code128Code
  , value :: Int
  , codeA :: Code128Interp
  , codeB :: Code128Interp
  , codeC :: Code128Interp
  }
  deriving (Show, Eq)

-- | The Code 128 symbol table
code128SymbolTable :: [Code128Symbol]
code128SymbolTable =
  [ Code128Symbol (Code128Code Two One Two Two Two Two) 0 (CharValue ' ') (CharValue ' ') (Digits 0 0)
  , Code128Symbol (Code128Code Two Two Two One Two Two) 1 (CharValue '!') (CharValue '!') (Digits 0 1)
  , Code128Symbol (Code128Code Two Two Two Two Two One) 2 (CharValue '"') (CharValue '"') (Digits 0 2)
  , Code128Symbol (Code128Code One Two One Two Two Three) 3 (CharValue '#') (CharValue '#') (Digits 0 3)
  , Code128Symbol (Code128Code One Two One Three Two Two) 4 (CharValue '$') (CharValue '$') (Digits 0 4)
  , Code128Symbol (Code128Code One Three One Two Two Two) 5 (CharValue '%') (CharValue '%') (Digits 0 5)
  , Code128Symbol (Code128Code One Two Two Two One Three) 6 (CharValue '&') (CharValue '&') (Digits 0 6)
  , Code128Symbol (Code128Code One Two Two Three One Two) 7 (CharValue '\'') (CharValue '\'') (Digits 0 7)
  , Code128Symbol (Code128Code One Three Two Two One Two) 8 (CharValue '(') (CharValue '(') (Digits 0 8)
  , Code128Symbol (Code128Code Two Two One Two One Three) 9 (CharValue ')') (CharValue ')') (Digits 0 9)
  , Code128Symbol (Code128Code Two Two One Three One Two) 10 (CharValue '*') (CharValue '*') (Digits 1 0)
  , Code128Symbol (Code128Code Two Three One Two One Two) 11 (CharValue '+') (CharValue '+') (Digits 1 1)
  , Code128Symbol (Code128Code One One Two Two Three Two) 12 (CharValue ',') (CharValue ',') (Digits 1 2)
  , Code128Symbol (Code128Code One Two Two One Three Two) 13 (CharValue '-') (CharValue '-') (Digits 1 3)
  , Code128Symbol (Code128Code One Two Two Two Three One) 14 (CharValue '.') (CharValue '.') (Digits 1 4)
  , Code128Symbol (Code128Code One One Three Two Two Two) 15 (CharValue '/') (CharValue '/') (Digits 1 5)
  , Code128Symbol (Code128Code One Two Three One Two Two) 16 (CharValue '0') (CharValue '0') (Digits 1 6)
  , Code128Symbol (Code128Code One Two Three Two Two One) 17 (CharValue '1') (CharValue '1') (Digits 1 7)
  , Code128Symbol (Code128Code Two Two Three Two One One) 18 (CharValue '2') (CharValue '2') (Digits 1 8)
  , Code128Symbol (Code128Code Two Two One One Three Two) 19 (CharValue '3') (CharValue '3') (Digits 1 9)
  , Code128Symbol (Code128Code Two Two One Two Three One) 20 (CharValue '4') (CharValue '4') (Digits 2 0)
  , Code128Symbol (Code128Code Two One Three Two One Two) 21 (CharValue '5') (CharValue '5') (Digits 2 1)
  , Code128Symbol (Code128Code Two Two Three One One Two) 22 (CharValue '6') (CharValue '6') (Digits 2 2)
  , Code128Symbol (Code128Code Three One Two One Three One) 23 (CharValue '7') (CharValue '7') (Digits 2 3)
  , Code128Symbol (Code128Code Three One One Two Two Two) 24 (CharValue '8') (CharValue '8') (Digits 2 4)
  , Code128Symbol (Code128Code Three Two One One Two Two) 25 (CharValue '9') (CharValue '9') (Digits 2 5)
  , Code128Symbol (Code128Code Three Two One Two Two One) 26 (CharValue ':') (CharValue ':') (Digits 2 6)
  , Code128Symbol (Code128Code Three One Two Two One Two) 27 (CharValue ';') (CharValue ';') (Digits 2 7)
  , Code128Symbol (Code128Code Three Two Two One One Two) 28 (CharValue '<') (CharValue '<') (Digits 2 8)
  , Code128Symbol (Code128Code Three Two Two Two One One) 29 (CharValue '=') (CharValue '=') (Digits 2 9)
  , Code128Symbol (Code128Code Two One Two One Two Three) 30 (CharValue '>') (CharValue '>') (Digits 3 0)
  , Code128Symbol (Code128Code Two One Two Three Two One) 31 (CharValue '?') (CharValue '?') (Digits 3 1)
  , Code128Symbol (Code128Code Two Three Two One Two One) 32 (CharValue '@') (CharValue '@') (Digits 3 2)
  , Code128Symbol (Code128Code One One One Three Two Three) 33 (CharValue 'A') (CharValue 'A') (Digits 3 3)
  , Code128Symbol (Code128Code One Three One One Two Three) 34 (CharValue 'B') (CharValue 'B') (Digits 3 4)
  , Code128Symbol (Code128Code One Three One Three Two One) 35 (CharValue 'C') (CharValue 'C') (Digits 3 5)
  , Code128Symbol (Code128Code One One Two Three One Three) 36 (CharValue 'D') (CharValue 'D') (Digits 3 6)
  , Code128Symbol (Code128Code One Three Two One One Three) 37 (CharValue 'E') (CharValue 'E') (Digits 3 7)
  , Code128Symbol (Code128Code One Three Two Three One One) 38 (CharValue 'F') (CharValue 'F') (Digits 3 8)
  , Code128Symbol (Code128Code Two One One Three One Three) 39 (CharValue 'G') (CharValue 'G') (Digits 3 9)
  , Code128Symbol (Code128Code Two Three One One One Three) 40 (CharValue 'H') (CharValue 'H') (Digits 4 0)
  , Code128Symbol (Code128Code Two Three One Three One One) 41 (CharValue 'I') (CharValue 'I') (Digits 4 1)
  , Code128Symbol (Code128Code One One Two One Three Three) 42 (CharValue 'J') (CharValue 'J') (Digits 4 2)
  , Code128Symbol (Code128Code One One Two Three Three One) 43 (CharValue 'K') (CharValue 'K') (Digits 4 3)
  , Code128Symbol (Code128Code One Three Two One Three One) 44 (CharValue 'L') (CharValue 'L') (Digits 4 4)
  , Code128Symbol (Code128Code One One Three One Two Three) 45 (CharValue 'M') (CharValue 'M') (Digits 4 5)
  , Code128Symbol (Code128Code One One Three Three Two One) 46 (CharValue 'N') (CharValue 'N') (Digits 4 6)
  , Code128Symbol (Code128Code One Three Three One Two One) 47 (CharValue 'O') (CharValue 'O') (Digits 4 7)
  , Code128Symbol (Code128Code Three One Three One Two One) 48 (CharValue 'P') (CharValue 'P') (Digits 4 8)
  , Code128Symbol (Code128Code Two One One Three Three One) 49 (CharValue 'Q') (CharValue 'Q') (Digits 4 9)
  , Code128Symbol (Code128Code Two Three One One Three One) 50 (CharValue 'R') (CharValue 'R') (Digits 5 0)
  , Code128Symbol (Code128Code Two One Three One One Three) 51 (CharValue 'S') (CharValue 'S') (Digits 5 1)
  , Code128Symbol (Code128Code Two One Three Three One One) 52 (CharValue 'T') (CharValue 'T') (Digits 5 2)
  , Code128Symbol (Code128Code Two One Three One Three One) 53 (CharValue 'U') (CharValue 'U') (Digits 5 3)
  , Code128Symbol (Code128Code Three One One One Two Three) 54 (CharValue 'V') (CharValue 'V') (Digits 5 4)
  , Code128Symbol (Code128Code Three One One Three Two One) 55 (CharValue 'W') (CharValue 'W') (Digits 5 5)
  , Code128Symbol (Code128Code Three Three One One Two One) 56 (CharValue 'X') (CharValue 'X') (Digits 5 6)
  , Code128Symbol (Code128Code Three One Two One One Three) 57 (CharValue 'Y') (CharValue 'Y') (Digits 5 7)
  , Code128Symbol (Code128Code Three One Two Three One One) 58 (CharValue 'Z') (CharValue 'Z') (Digits 5 8)
  , Code128Symbol (Code128Code Three Three Two One One One) 59 (CharValue '[') (CharValue '[') (Digits 5 9)
  , Code128Symbol (Code128Code Three One Four One One One) 60 (CharValue '\\') (CharValue '\\') (Digits 6 0)
  , Code128Symbol (Code128Code Two Two One Four One One) 61 (CharValue ']') (CharValue ']') (Digits 6 1)
  , Code128Symbol (Code128Code Four Three One One One One) 62 (CharValue '^') (CharValue '^') (Digits 6 2)
  , Code128Symbol (Code128Code One One One Two Two Four) 63 (CharValue '_') (CharValue '_') (Digits 6 3)
  , Code128Symbol (Code128Code One One One Four Two Two) 64 (CharValue '\NUL') (CharValue '`') (Digits 6 4)
  , Code128Symbol (Code128Code One Two One One Two Four) 65 (CharValue '\SOH') (CharValue 'a') (Digits 6 5)
  , Code128Symbol (Code128Code One Two One Four Two One) 66 (CharValue '\STX') (CharValue 'b') (Digits 6 6)
  , Code128Symbol (Code128Code One Four One One Two Two) 67 (CharValue '\ETX') (CharValue 'c') (Digits 6 7)
  , Code128Symbol (Code128Code One Four One Two Two One) 68 (CharValue '\EOT') (CharValue 'd') (Digits 6 8)
  , Code128Symbol (Code128Code One One Two Two One Four) 69 (CharValue '\ENQ') (CharValue 'e') (Digits 6 9)
  , Code128Symbol (Code128Code One One Two Four One Two) 70 (CharValue '\ACK') (CharValue 'f') (Digits 7 0)
  , Code128Symbol (Code128Code One Two Two One One Four) 71 (CharValue '\BEL') (CharValue 'g') (Digits 7 1)
  , Code128Symbol (Code128Code One Two Two Four One One) 72 (CharValue '\BS') (CharValue 'h') (Digits 7 2)
  , Code128Symbol (Code128Code One Four Two One One Two) 73 (CharValue '\HT') (CharValue 'i') (Digits 7 3)
  , Code128Symbol (Code128Code One Four Two Two One One) 74 (CharValue '\LF') (CharValue 'j') (Digits 7 4)
  , Code128Symbol (Code128Code Two Four One Two One One) 75 (CharValue '\VT') (CharValue 'k') (Digits 7 5)
  , Code128Symbol (Code128Code Two Two One One One Four) 76 (CharValue '\FF') (CharValue 'l') (Digits 7 6)
  , Code128Symbol (Code128Code Four One Three One One One) 77 (CharValue '\CR') (CharValue 'm') (Digits 7 7)
  , Code128Symbol (Code128Code Two Four One One One Two) 78 (CharValue '\SO') (CharValue 'n') (Digits 7 8)
  , Code128Symbol (Code128Code One Three Four One One One) 79 (CharValue '\SI') (CharValue 'o') (Digits 7 9)
  , Code128Symbol (Code128Code One One One Two Four Two) 80 (CharValue '\DLE') (CharValue 'p') (Digits 8 0)
  , Code128Symbol (Code128Code One Two One One Four Two) 81 (CharValue '\DC1') (CharValue 'q') (Digits 8 1)
  , Code128Symbol (Code128Code One Two One Two Four One) 82 (CharValue '\DC2') (CharValue 'r') (Digits 8 2)
  , Code128Symbol (Code128Code One One Four Two One Two) 83 (CharValue '\DC3') (CharValue 's') (Digits 8 3)
  , Code128Symbol (Code128Code One Two Four One One Two) 84 (CharValue '\DC4') (CharValue 't') (Digits 8 4)
  , Code128Symbol (Code128Code One Two Four Two One One) 85 (CharValue '\NAK') (CharValue 'u') (Digits 8 5)
  , Code128Symbol (Code128Code Four One One Two One Two) 86 (CharValue '\SYN') (CharValue 'v') (Digits 8 6)
  , Code128Symbol (Code128Code Four Two One One One Two) 87 (CharValue '\ETB') (CharValue 'w') (Digits 8 7)
  , Code128Symbol (Code128Code Four Two One Two One One) 88 (CharValue '\CAN') (CharValue 'x') (Digits 8 8)
  , Code128Symbol (Code128Code Two One Two One Four One) 89 (CharValue '\EM') (CharValue 'y') (Digits 8 9)
  , Code128Symbol (Code128Code Two One Four One Two One) 90 (CharValue '\SUB') (CharValue 'z') (Digits 9 0)
  , Code128Symbol (Code128Code Four One Two One Two One) 91 (CharValue '\ESC') (CharValue '{') (Digits 9 1)
  , Code128Symbol (Code128Code One One One One Four Three) 92 (CharValue '\FS') (CharValue '|') (Digits 9 2)
  , Code128Symbol (Code128Code One One One Three Four One) 93 (CharValue '\GS') (CharValue '}') (Digits 9 3)
  , Code128Symbol (Code128Code One Three One One Four One) 94 (CharValue '\RS') (CharValue '~') (Digits 9 4)
  , Code128Symbol (Code128Code One One Four One One Three) 95 (CharValue '\US') (CharValue '\DEL') (Digits 9 5)
  , Code128Symbol (Code128Code One One Four Three One One) 96 FNC3 FNC3 (Digits 9 6)
  , Code128Symbol (Code128Code Four One One One One Three) 97 FNC2 FNC2 (Digits 9 7)
  , Code128Symbol (Code128Code Four One One Three One One) 98 ShiftB ShiftA (Digits 9 8)
  , Code128Symbol toC 99 CodeC CodeC (Digits 9 9)
  , Code128Symbol toB 100 CodeB FNC4 CodeB
  , Code128Symbol toA 101 FNC4 CodeA CodeA
  , Code128Symbol (Code128Code Four One One One Three One) 102 FNC1 FNC1 FNC1
  , Code128Symbol startA 103 StartA StartA StartA
  , Code128Symbol startB 104 StartB StartB StartB
  , Code128Symbol startC 105 StartC StartC StartC
  ]

-- | The three start codes determine the initial scheme
startA, startB, startC :: Code128Code
startA = Code128Code Two One One Four One Two
startB = Code128Code Two One One Two One Four
startC = Code128Code Two One One Two Three Two

-- | The coding change characters are valid only in the other schemes
toA, toB, toC :: Code128Code
toA = Code128Code Three One One One Four One
toB = Code128Code One One Four One Three One
toC = Code128Code One One Three One Four One

-- | Interpret a code as a list of digits
toBars :: Code128Code -> [Code128Digit]
toBars (Code128Code x1 x2 x3 x4 x5 x6) = [x1, x2, x3, x4, x5, x6]
toBars StopCode = [Two, Three, Three, One, One, One, Two]

digitWidth :: Code128Digit -> Int
digitWidth One   = 1
digitWidth Two   = 2
digitWidth Three = 3
digitWidth Four  = 4


codeWidth :: BarcodeConfig -> Double
codeWidth config = barWidth config * 11

bool :: a -> a -> Bool -> a
bool x _ False = x
bool _ y True = y

symbolForCode :: Code128Code -> Either Err Code128Symbol
symbolForCode c = getSym code128SymbolTable
  where getSym [] = Left . InternalError $ "No symbol for " ++ show c
        getSym (sym:table) | c == code sym = Right sym
                           | otherwise     = getSym table

symbolForValue :: Int -> Either Err Code128Symbol
symbolForValue i = getSym code128SymbolTable
  where getSym [] = Left . InternalError $ "No symbol with value " ++ show i
        getSym (sym:table) | i == value sym = Right sym
                           | otherwise      = getSym table

checksum :: [Code128Code] -> Either Err Code128Code
checksum str = do syms <- mapM symbolForCode str
                  let total = sum $ zipWith (*) (map value syms) (1:[1..])
                      check = total `mod` 103
                  checkSym <- symbolForValue check
                  return (code checkSym)

-- | Whether a character can be encoded in code set A
inA :: Char -> Maybe Code128Code
inA c = code <$> (listToMaybe . filter ((CharValue c ==) . codeA)) code128SymbolTable


-- | Whether a character can be encoded in code set B
inB :: Char -> Maybe Code128Code
inB c = code <$> (listToMaybe . filter ((CharValue c ==) . codeB)) code128SymbolTable

-- | Whether a pair of characters can be encoded in code set C
inC :: Char -> Char -> Maybe Code128Code
inC d1 d2 | isDigit d1, isDigit d2 = getC (Digits (digitToInt d1) (digitToInt d2)) code128SymbolTable
          | otherwise = Nothing
  where getC i table = code <$> (listToMaybe . filter ((i==) . codeC)) table


data EncoderState = None | StateA | StateB | StateC

encode' :: EncoderState -> [Char] -> Either Err [Code128Code]
encode' _ [] = pure []
-- It's worth encoding in code C at the beginning if there's at least four digits at the start
encode' None (w:x:y:z:rest) | Just c1 <- inC w x
                            , Just c2 <- inC y z = ([startC, c1, c2] ++ ) <$> encode' StateC rest
-- Otherwise prefer code B because lowercase letters are more likely than control codes
encode' None (x:xs) | Just b <- inB x = ([startB, b] ++) <$> encode' StateB xs
                    | Just a <- inA x = ([startA, a] ++) <$> encode' StateA xs
                    | otherwise = Left (CantEncode x)

-- In state A, switch to C if there's a run of at least 6 digits in the middle
encode' StateA (u:v:w:x:y:z:rest) | Just c1 <- inC u v
                                  , Just c2 <- inC w x
                                  , Just c3 <- inC y z
                                  =  ([toC, c1, c2, c3] ++) <$> encode' StateC rest
-- Also switch to C if there's precisely 4 digits remaining
encode' StateA [w,x,y,z] | Just c1 <- inC w x
                         , Just c2 <- inC y z = pure [toC, c1, c2]
-- Otherwise attempt to stay in A, but switch to B if necessary
encode' StateA (x:rest) | Just a <- inA x = (a :) <$> encode' StateA rest
                        | Just b <- inB x = ([toB, b] ++) <$> encode' StateB rest
                        | otherwise = error "invalid input string" -- todo error monad

-- In state B, switch to C if there's a run of at least 6 digits in the middle
encode' StateB (u:v:w:x:y:z:rest) | Just c1 <- inC u v
                                  , Just c2 <- inC w x
                                  , Just c3 <- inC y z
                                  = ([toC, c1, c2, c3] ++) <$> encode' StateC rest
-- Also switch to C if there's precisely 4 digits remaining
encode' StateB [w,x,y,z] | Just c1 <- inC w x
                         , Just c2 <- inC y z = pure [toC, c1, c2]
-- Otherwise attempt to stay in B, but switch to A if necessary
encode' StateB (x:rest) | Just b <- inB x = (b :) <$> encode' StateB rest
                        | Just a <- inA x = ([toA, a] ++) <$> encode' StateA rest
                        | otherwise = Left (CantEncode x)

-- TODO: We should really prevent this, by only switching to C if the prefix of digits is even-length
encode' StateC [x] | Just b <- inB x = pure [toB, b]
                   | Just a <- inA x = pure [toA, a]
                   | otherwise = Left (CantEncode x)
-- If in state C, attempt to stay there, otherwise prefer B over A
encode' StateC (x:y:rest) | Just c <- inC x y = (c :) <$> encode' StateC rest
                          | Just b <- inB x   = ([toB, b] ++) <$> encode' StateB (y:rest)
                          | Just a <- inA x   = ([toA, a] ++) <$> encode' StateA (y:rest)
                          | otherwise         = Left (CantEncode x)

checksummed :: [Code128Code] -> Either Err [Code128Code]
checksummed xs = do cs <- checksum xs
                    return $ xs ++ [cs, StopCode]

encode :: [Char] -> Either Err [Code128Code]
encode str = encode' None str >>= checksummed

drawChar :: BarcodeConfig -> Point -> Code128Code -> Draw Point
drawChar config at c = snd <$> foldlM drawBar (False, at) (toBars c)
  where drawBar (color, topLeft@(left :+ top)) bar = do
          strokeColor $ bool black white color
          fillColor $ bool black white color
          setWidth 0
          let left' = left + fromIntegral (digitWidth bar) * barWidth config
          fill . Rectangle topLeft $
            (left' :+ (top + height config))
          return $ (not color, (left' :+ top))

drawChars :: BarcodeConfig -> [Code128Code] -> Point -> Draw Point
drawChars config codes start = foldlM (drawChar config) start codes

-- Simple test, for use when developing the library
test :: String -> IO ()
test str = do let config = BarcodeConfig 40 1
                  Right (w, h) = barcodeSize config str
                  rect = PDFRect 0 0 (ceiling w + 20) (ceiling h + 20)
              runPdf "demo.pdf" standardDocInfo { compressed = False } rect $ do
                p <- addPage Nothing
                drawWithPage p $ do
                  fillColor red
                  setWidth 0
                  fill $ Rectangle (0 :+ 0) (600 :+ 100)
                  case drawBarcode (BarcodeConfig 40 1) str (10 :+ 10) of
                    Left err -> error (show err)
                    Right code -> code
