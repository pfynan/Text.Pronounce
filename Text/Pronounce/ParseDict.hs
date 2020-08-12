{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Text.Pronounce.ParseDict
Description : Module for parsing the CMU Dictionary
Copyright   : (c) Noah Goodman, 2018
License     : BSD3
Stability   : experimental

This module has functions for parsing the CMU pronouncing dictionary, and exports the 
@CMUdict@ type and the function @initDict@ to the main module "Text.Pronounce"
-}


module Text.Pronounce.ParseDict 
    ( EntryWord
    , Phones
    , CMUdict
    , initDict
    , stdDict
    , parseDict
    , parseLine
    , myDict
    ) where

import Paths_pronounce
import System.FilePath
-- import Text.ParserCombinators.ReadP
import Data.Char
import Data.Text.Encoding
import Data.Binary (decodeFile, decode)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed
import Control.DeepSeq
import qualified Data.Attoparsec.Text as A
import Control.Applicative
import Data.Either
import Control.Monad
-- | Represents an entry word in the cmu pronouncing dictionary (simply an alias
-- for @Text@ to improve type specificity and readability
type EntryWord = T.Text

-- | Represents a string containing the phonetic breakdown of a word, in a
-- similar manner to the @EntryWord@ type
type Phones = T.Text

-- | A Map from @EntryWord@s to lists of possible pronunciations (@Phones@), serving as our
-- representation of the CMU Pronouncing Dictionary
type CMUdict = Map.Map EntryWord [Phones]

-- | Initializes the cmu pronunctiation dictionary into our program, given an
-- optional file name of the dictionary
initDict :: Maybe FilePath -> Bool -> IO CMUdict
initDict path usesBin = case usesBin of
    True ->
        case path of 
          Just p ->
              return undefined
              -- return . Map.mapKeys decodeUtf8 . fmap (map decodeUtf8) =<< decodeFile p
          Nothing ->
              return undefined
              -- return . Map.mapKeys decodeUtf8 . fmap (map decodeUtf8) =<< return (decode . BL.fromStrict $ $(embedFile "cmubin"))
    False ->
        case path of 
          Just p -> 
              return . parseDict =<< T.readFile p
          Nothing -> 
              -- return . parseDict =<< T.readFile =<< getDataFileName "cmuutf"
              return . parseDict . decodeUtf8 $ $(embedFile "cmuutf")

myDict :: CMUdict
myDict = force . parseDict . decodeUtf8 $ $(embedFile "cmuutf")


-- | Default settings for @initDict@
stdDict :: IO CMUdict
stdDict = initDict Nothing True

-- | Go through all the entries in the dictionary, parsing, and inserting into
-- the map data structure
parseDict :: T.Text -> CMUdict
-- parseDict = Map.fromListWith (++) . map packAndParse . filter ((/= ';') . T.head) . T.lines
--     where packAndParse = (\(a,b) -> (T.pack a, [T.pack b])) . fst . head . readP_to_S parseLine . T.unpack
parseDict = Map.fromListWith (++) . rights . map (force . A.parseOnly parseLine) . filter ((/= ';') . T.head) . T.lines

-- | Parses a line in the dictionary, returning as @(key,val)@ pair, ignoring
-- parenthetical part if it exists
parseLine :: A.Parser (T.Text, [T.Text])
parseLine = (\a b -> (a, [b])) <$> word <* A.string "  "
                <*> A.takeText

word = wordChunk <* (optional paren)

wordChunk = fst <$> A.match (optional (A.char '(') *> chunk1)

chunk1 = A.takeWhile (\a -> a /= ' ' && a /= '(')


-- Helper function to parse numbers in between parentheses
paren :: A.Parser T.Text
paren = A.char '(' *> A.takeWhile isDigit <* A.char ')'
