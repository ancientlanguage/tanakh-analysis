{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- Taken from https://www.itscj.ipsj.or.jp/iso-ir/006.pdf

module AsciiGraphicSet where

import Data.Text (Text)
import Data.String.Interpolate.IsString (i)

definition :: Text
definition = [i|
2/1 Exclamation mark
2/2 Quotation mark (diaresis)
2/3 Number sign
2/4 Dollar sign
2/5 Percent
2/6 Ampersand
2/7 Apostrophe (closing single quotation mark, acute accent)
2/8 Left parenthesis
2/9 Right parenthesis
2/10 Asterisk
2/11 Plus
2/12 Comma (cedilla)
2/13 Hyphen (minus)
2/14 Full stop (period, decimal point)
2/15 Solidus (slant)
3/0 Digit zero
3/1 Digit one
3/2 Digit two
3/3 Digit three
3/4 Digit four
3/5 Digit five
3/6 Digit six
3/7 Digit seven
3/8 Digit eight
3/9 Digit nine
3/10 Colon
3/11 Semi-colon
3/12 Less than sign
3/13 Equal sign
3/14 Greater than sign
3/15 Question mark
4/0 Commercial at
4/1 Capital letter A
4/2 Capital letter B
4/3 Capital letter C
4/4 Capital letter D
4/5 Capital letter E
4/6 Capital letter F
4/7 Capital letter G
4/8 Capital letter H
4/9 Capital letter I
4/10 Capital letter J
4/11 Capital letter K
4/12 Capital letter L
4/13 Capital letter M
4/14 Capital letter N
4/15 Capital letter O
5/0 Capital letter P
5/1 Capital letter Q
5/2 Capital letter R
5/3 Capital letter S
5/4 Capital letter T
5/5 Capital letter U
5/6 Capital letter V
5/7 Capital letter W
5/8 Capital letter X
5/9 Capital letter Y
5/10 Capital letter Z
5/11 Left square bracket
5/12 Reverse solidus (Reverse slant)
5/13 Right square bracket
5/14 Upward arrow head (circumflex accent)
5/15 Underline
6/0 Grave accent (opening single quotation mark)
6/1 Small letter a
6/2 Small letter b
6/3 Small letter c
6/4 Small letter d
6/5 Small letter e
6/6 Small letter f
6/7 Small letter g
6/8 Small letter h
6/9 Small letter i
6/10 Small letter j
6/11 Small letter k
6/12 Small letter l
6/13 Small letter m
6/14 Small letter n
6/15 Small letter o
7/0 Small letter p
7/1 Small letter q
7/2 Small letter r
7/3 Small letter s
7/4 Small letter t
7/5 Small letter u
7/6 Small letter v
7/7 Small letter w
7/8 Small letter x
7/9 Small letter y
7/10 Small letter z
7/11 Left curly bracket (left brace)
7/12 Vertical line
7/13 Right curly bracket (right brace)
7/14 Tilde (overline, general accent)
  |]
