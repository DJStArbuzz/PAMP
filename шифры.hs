data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)
type Bits = [Bool]

secretMessage :: String
secretMessage = "vstrechaemsya v stolovke"

disposableNote :: String
disposableNote = "bn<SMNDL<WASND<fBNSFlkASNFkjASNFLKDC ASzfknas fkj"

encodeNdecode :: String -> String
encodeNdecode = disposableNoteEncode disposableNote

disposableNoteEncode :: String -> String -> String
disposableNoteEncode dmsg smsg = map bitsToChar bitList
  where bitList = disposableNoteEncode' dmsg smsg

disposableNoteEncode' :: String -> String -> [Bits]
disposableNoteEncode' dsmg smsg = map
  (\(a, b) -> a `xor` b)
  (zip dBits sBits)
  where sBits = map charToBits smsg
        dBits = map charToBits dsmg

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if remainder == 0
               then False : intToBits' nextVal
               else True : intToBits' nextVal
  where remainder = n `mod` 2
        nextVal = n `div` 2

maxBits :: Int 
maxBits = length (intToBits' maxBound)

charToBits :: Char -> Bits
charToBits ch = intToBits (fromEnum ch)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

bitsToInt :: Bits -> Int
bitsToInt bits = foldl combine 0 bitsWithPows
  where combine acc (x, pow) = if x then acc + 2^pow else acc
        size = length numToBits
        numToBits = dropWhile not bits
        bitsWithPows = zip numToBits [size - 1, size - 2 .. 0]

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reverse (intToBits' n)
  where
    bitsRev = reverse (intToBits' n)
    missing = maxBits - length bitsRev
    leadingFalses = replicate missing False

xor :: [Bool] -> [Bool] -> [Bool]
xor lst1 lst2 = map xorPair (zip lst1 lst2)

xorPair :: (Bool, Bool)-> Bool
xorPair (val1, val2) = xorBool val1 val2

xorBool :: Bool -> Bool -> Bool
xorBool val1 val2 = (val1 || val2) && (not (val1 && val2))

vigenereEncode :: String -> String -> String
vigenereEncode key msg = zipWith encodePair (cycle key) msg
  where encodePair k c = shiftChar (fromEnum k) c 

vigenereDencode :: String -> String -> String
vigenereDencode key msg = zipWith decodePair (cycle key) msg
  where decodePair k c = shiftChar(- fromEnum k) c

shiftChar :: Int -> Char -> Char 
shiftChar offset ch = toEnum shifted
  where n = 1 + maxCharNumber
        shifted = (fromEnum ch + offset) `mod` n 

msgEncoder :: String -> String
msgEncoder msg = map rotCharEncoder msg

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n ch = toEnum rotation
  where half = n `div` 2
        offset = if even n 
                 then fromEnum ch + half
                 else 1 + fromEnum ch + half
        rotation = offset `mod` n

fourLetterMsgEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterMsgEncoder msg = map rotFLA msg
  where n = 1 + fromEnum (maxBound :: FourLetterAlphabet)
        rotFLA = rotN n

rotCharEncoder :: Char -> Char
rotCharEncoder ch = rotN n ch
  where n = 1 + maxCharNumber

rotN :: (Enum a) => Int -> a -> a
rotN n ch = toEnum rotation
  where halfAlphabet = n `div` 2
        offset = fromEnum ch + halfAlphabet
        rotation = offset `mod` n

maxCharNumber :: Int
maxCharNumber = fromEnum (maxBound :: Char)

message :: [FourLetterAlphabet]
message = [L2, L1, L4, L1, L1, L3, L4]
