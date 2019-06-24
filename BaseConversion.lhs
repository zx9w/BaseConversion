> {-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

> module BaseConversion
> where

import qualified Data.ByteString as B

> import qualified Data.ByteString.Char8 as C
> import Data.Char
> import Data.List


Here is the base16, base32 and base64 reference: https://tools.ietf.org/html/rfc4648

This program will implement each of these in a somewhat nice way.

------ Utilities and Internal Datatypes ------

This is an internal check function the external one will use type aliases.
 => Nope we don't have first class types in haskell.

Okay I found a different solution:

> data Base = Base16 | Base32 | Base64 deriving (Enum, Show, Ord, Eq)

> mkSyms :: Base -> String
> mkSyms Base16 = "0123456789ABCDEF"
> mkSyms Base32 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567="
> mkSyms Base64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_="

Base 32 and 64 require a further check to see if it has a legal padding.

> mkPad :: Base -> [Int]
> mkPad Base16 = [0]
> mkPad Base32 = [0,1,3,4,6]
> mkPad Base64 = [0,1,2]

This function will check the padding

> checkPadding :: Base -> String -> Bool
> checkPadding base str = (padPlacement str) && (padCount base str)
>   where
>     padPlacement s = 0 == (length $ (filter (/='=') (dropWhile (/='=') s)))
>     padCount b s = length (dropWhile (/='=') s) `elem` (mkPad b)


Note: flip id == \x f -> f x ~~ That is to say this is actually super simple.

> check' :: Base -> String -> Bool
> check' base cstr = 0 == (length $ foldl (flip id) cstr $ filters $ mkSyms base)
>   where
>     filters :: String -> [String -> String]
>     filters str = (filter . (/=)) <$> str

> check :: Base -> String -> Bool
> check base str = (check' base str) && (checkPadding base str)

I need to refactor this checking logic, I originally didn't think to make the Enum of bases.


---- Bits ----

Next we want an internal representation of the bits.

We will need to make sure the size of the bit vector is correct.

> mkSize :: Base -> Int
> mkSize Base16 = 4
> mkSize Base32 = 5
> mkSize Base64 = 6

Maybe at some point I will be clever again and figure out a way to extend the Base type.

> byteSize :: Int
> byteSize = 8

Note to reader:

Obviously there is some fabulous library out there to do all this..
However the point of this exercise is to write it myself.

The most obvious way is to use Bools, but with a seperate type I can 'show' it differently.

> data Bit = Zero | One deriving (Eq, Ord, Enum)
> instance Show Bit where
>   show One = "1"
>   show Zero = "0"


I will just make an increment function instead of trying to derive Enum for Bits = [Bit].

> increment :: [Bit] -> Maybe [Bit]
> increment [] = Nothing
> increment [Zero] = Just [One]
> increment (x:xs) | (allOne xs) = if x == Zero
>                                  then Just (One : (take (length xs) $ repeat Zero))
>                                  else Nothing
>                  | otherwise = (x :) <$> (increment xs)

Actually I realised I never use this increment thing...

> allOne :: [Bit] -> Bool
> allOne [] = True
> allOne (x:xs) | x == Zero = False
>               | otherwise = allOne xs

Now I can use a conversion toInt and fromInt to get a Enum for Bits.

> toInt :: [Bit] -> Int
> toInt []        = error "Bitvector is empty!"
> toInt [Zero]    = 0
> toInt [One]     = 1
> toInt (One:xs)  = 2^(length xs) + (toInt xs)
> toInt (Zero:xs) = toInt xs

> fromInt' :: Int -> [Bit] -- TODO: Make a quickcheck for toInt' . fromInt'
> fromInt' 0 = [Zero]
> fromInt' 1 = [One]
> fromInt' x | even x    = (fromInt' $ x `quot` 2) ++ [Zero]
>            | otherwise = (fromInt' $ (x-1) `quot` 2) ++ [One]

This should always return true (and it does).

> testIntConversion :: Bool
> testIntConversion = (==) [1..100000] $ (toInt . fromInt') <$> [1..100000]

I need to pad the bit-vectors to fit the format.

This could be done automatically with dependent types.

For now a generic padding function and some bookkeeping will have to suffice.

> padding :: Int -> [Bit] -> [Bit]
> padding size xs = if (length xs < size) then padding size (Zero : xs) else xs

This is maybe a bit too fancy but it's still easy enough to understand.

> toInts :: Int -> [Bit] -> [Int]
> toInts _    [] = []
> toInts size xs = (((:) . toInt . take size) <*> ((toInts size) . drop size)) xs

This wrapper function pads the bitvector to align with expectation.

> fromInt :: Int -> Int -> [Bit]
> fromInt size = (padding size) . fromInt'

It is good to remember that Int is just a placeholder for Enum.
When Haskell has proper first class types I can make a better Bit representation.

> fromInts :: Int -> [Int] -> [Bit]
> fromInts size ints = foldr ((++) . (fromInt size)) [] $ filter (/=(2^size)) ints


---- Data ----

Now we can start using these functions to decode and encode data.

The main workhorse will be this function:

> changeSize :: Int -> Int -> [Int] -> [Int]
> changeSize fromSize toSize ints = toInts toSize (fromInts fromSize ints)

As well as the decoding bootstrap function.

> mkParser :: Base -> (String -> Maybe [Int])
> mkParser base = \str -> if check base str
>                         then sequence (conv base str)
>                         else Nothing
>   where
>     conv b s = foldr ((:). flip elemIndex (mkSyms b)) [] s

Now we can get enums that are equivalent to bytes from any Base.

> changeBaseToBytes :: Base -> [Int] -> [Int]
> changeBaseToBytes base ints = changeSize (mkSize base) byteSize ints

> changeBytesToBase :: [Int] -> Base -> [Int]
> changeBytesToBase bytes base = changeSize byteSize (mkSize base) bytes

So the next step is to change them into "Words" that we can write to file for example.

> decode :: Base -> String -> Maybe C.ByteString
> decode base str = C.pack <$>(fmap chr)<$>(changeBaseToBytes base)<$>((mkParser base) str)

Okay so decoding now works like a charm.

However our existing machinery is can't handle encoding because it needs to pad correctly.

The problem is with how we do padding.

> encode :: Base -> C.ByteString -> Maybe String
> encode base bytes = (mkWriter base) (changeBytesToBase (ord <$> (C.unpack bytes)) base)

I don't want my program to panic for no reason...

> (!!!) :: [a] -> Int -> Maybe a
> (!!!) list index | index < length list = Just (list!!index)
>                  | otherwise           = Nothing

If the Ints correspond to a base then this will return a base encoded string

> mkWriter :: Base -> ([Int] -> Maybe String)
> mkWriter b | b == Base16 = (mkString Base16)
>            | b == Base32 = (\xs ->(pad 5)=<<(mkString Base32 xs))
>            | b == Base64 = (\xs ->(pad 4)=<<(mkString Base64 xs))
>   where
>     mkString :: Base -> [Int] -> Maybe String
>     mkString base ints = sequence (((!!!) (mkSyms base)) <$> ints)
>     pad :: Int -> String -> Maybe String
>     pad int str = Just (str ++ (replicate (mod (length str) int) '='))
> mkWriter _ = error "impossible for there to be a different base."

This is not perfect because Base64 has two alphabets and I'm also missing the Hex alpha.
