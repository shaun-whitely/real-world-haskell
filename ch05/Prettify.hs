module Prettify
    (
      Doc,
      empty,
      string,
      text,
      double,
      char,
      line,
      compact,
      series,
      (<>)
    )
where

import Numeric
import Data.Bits
import Data.Char (ord)
import Prelude hiding ((<>))

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
    deriving (Show, Eq)

empty :: Doc
empty = Empty

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double = Text . show

char :: Char -> Doc
char = Char

line :: Doc
line = Line

compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
            case d of
                Empty        -> transform ds
                Char c       -> c : transform ds
                Text s       -> s ++ transform ds
                Line         -> '\n' : transform ds
                a `Concat` b -> transform (a:b:ds)
                _ `Union` b  -> transform (b:ds)

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

(<>) :: Doc -> Doc -> Doc
x <> Empty = x
Empty <> y = y
x <> y = x `Concat` y

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
    Just r -> text r
    Nothing | mustEscape c -> hexEscape c
            | otherwise -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\',b])

smallHex :: Int -> Doc
smallHex x  = text "\\u"
           <> text (replicate (4 - length h) '0')
           <> text h
    where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
    where d = ord c

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                       . fsep . punctuate (char ',') . map item

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _) = flatten x
flatten other = other

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds
