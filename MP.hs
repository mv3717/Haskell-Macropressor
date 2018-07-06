module MP where

import System.Environment

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"

-- Returns the list of items whose associated string matches the search string.
lookUp :: String -> [(String, a)] -> [a]
lookUp search listOfPairs
  = [word2 | (word1, word2) <- listOfPairs, search == word1]

-- Breaks up a string into its specified separators and a list of the words
-- in that string.
split :: [Char] -> String -> (String, [String])
split sep [] = ("",[""])
split sep (w1 : remWords1)
  | elem w1 sep  = (w1 : separators, "" : (w2 : remWords2))
  | otherwise    = (separators, (w1 : w2) : remWords2)
    where
      (separators, w2 : remWords2) = split sep remWords1

-- Combines the components of a string from its constituent separator
-- characters and words, as generated by a call to split.
combine :: String -> [String] -> [String]
combine "" (word1 : remWords) = (word1 : remWords)
combine (sep1 : remSep) (word1 : remWords)
  = word1 : (sep1 : "") : (combine remSep remWords)

-- Takes the contents of an information file in the form of a list of
-- lines (each line is a string), and returns a list of
-- keyword/definition pairs.
getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs [] = []
getKeywordDefs info
  = (keyWord, concat(combine remSeps keywordValue)) : getKeywordDefs remPairs
    where
      (sep1 : remSeps, keyWord : keywordValue) = split separators pair1
      (pair1 : remPairs) = filter (/="") info
      -- Note: we remove here the empty strings with the filter function.
      -- This isn't an issue for the tests but it is when we want to use it
      -- after extracting multiple keys from the info file (extension)

-- Takes the contents of a text file and an info file and combines them
-- using the above functions to build a string representing the output file.
expand :: FileContents -> FileContents -> FileContents
expand text info
  = concat(combine seps (changeWords words listOfKeys))
    where
      (seps, words)     = split separators text
      listOfKeys        = getKeywordDefs keyWords
      (seps', keyWords) = split "\n" info

-- Takes a list of words and if the word is a key word then it swaps it
-- with the key word value associated; otherwise it returns the same word.
changeWords :: [String] -> KeywordDefs -> [String]
changeWords [] listOfKeys = []
changeWords (firstWord:remainingWords) listOfKeys
  = (lookUpWord firstWord listOfKeys) : (changeWords remainingWords listOfKeys)

-- This is a helper function for changeWords. It searches for the key word
-- and returns the value associated if it exists.
lookUpWord :: String -> KeywordDefs -> String
lookUpWord search listOfPairs
  | newWord /= [] = head newWord
  | otherwise     = search
    where
      newWord = [word | (key, word) <- listOfPairs, search == key]

-- Pre: we are assuming that there is always at least one '#'' in the info file.
extendedExpand :: FileContents -> FileContents -> FileContents
extendedExpand text [] = []
extendedExpand text info
  = expand text firstSet ++ "\n-----\n" ++ extendedExpand text remainingSets
    where
      firstSet      = take ((digitPosition '#' info) - 1) info
      remainingSets = drop ((digitPosition '#' info)) info

digitPosition :: Char -> String -> Int
digitPosition digit (l1:remLetters)
  | digit == l1 = 1
  | otherwise   = 1 + digitPosition digit remLetters

main :: IO ()
-- The provided main program which uses your functions to merge a
-- template and source file.
main = do
  args <- getArgs
  main' args

  where
    main' :: [String] -> IO ()
    main' [template, source, output] = do
      t <- readFile template
      i <- readFile source
      writeFile output (expand t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")