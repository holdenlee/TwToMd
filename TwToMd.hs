module Main where

import Text.Parsec
import Text.Parsec.String
import Data.Time.Clock
import Utilities
import System.Directory
import System.IO
import System.Environment
import Data.Foldable
import Data.Either
import Text.Printf
import Data.List
import System.FilePath

getDate :: IO String
getDate = fmap (take 10 . show) getCurrentTime
-- YYYY-MM-DD

replaceParser :: Parser String -> Parser String
replaceParser p = 
    try (do
          s <- p
          rest <- replaceParser p
          return (s++rest))
         <|>
         (do
           c <- anyChar
           rest <- replaceParser p
           return ([c]++rest))
         <|>
         (eof >> return "")

simpleR :: String -> String -> Parser String
simpleR  a b = do
    string a
    return b

justParse :: Parser String -> String -> String
justParse p contents = fromRight (parse p "error" contents)

replaceParse p contents = justParse (replaceParser p) contents

italicsReplacer :: Parser String
italicsReplacer = do
  string "//"
  text <- many1 (noneOf "/")
  string "//"
  return (printf "*%s*" text)

boldReplacer :: Parser String
boldReplacer = do
  string "''"
  text <- many1 (noneOf "'")
  string "''"
  return (printf "**%s**" text)

quoteReplacer :: Parser String
quoteReplacer = do
  newline
  s <- many1 (char '"' >> newline)
  --newlines don't count as characters
  return $ replicate ((length s)) '"'
  --debugShow

linkReplacer :: Parser String
linkReplacer = do
  string "[["
  text1 <- many1 (noneOf "|]")
  char '|'
  text2 <- many1 (noneOf "]")
  string "]]"
  return (printf "[%s](%s)" text1 text2)

wikilinkReplacer :: Parser String
wikilinkReplacer = do
  string "[["
  text <- many1 (noneOf "]")
  string "]]"
  return (printf "[%s](%s.html)" text text)

escapeReplacer :: Parser String
escapeReplacer = do
  string "\"\"\""
  text <- many1 (noneOf "\"")
  string "\"\"\""
  return text

listReplacer :: Parser String
listReplacer = do
  n <- newline
  s <- many1 (oneOf "*#")
  return ([n] ++ (replicate (4*(length s - 1)) ' ') ++
            case (last s) of
              '*' -> "+"
              '#' -> "1.")

headingReplacer :: Parser String
headingReplacer = do
  n <- newline
  s <- many1 (char '!')
  return ([n] ++ (replicate (length s) '#'))

{-
extractTitleAndTags :: Parser (String, [String])
extractTitleAndTags = do
  title <- many1 (noneOf "/n")
  let x = do
-}
extractTitleAndTags :: String -> (String, String, [String])
extractTitleAndTags s = 
    let
        l = lines s
        title = head l
        body = unlines $ init $ tail l
        lastwords = words $ last l
        tags = if head lastwords == "tags:" then tail lastwords else []
    in (title, body, tags)

--https://mail.haskell.org/pipermail/haskell-cafe/2006-January/013911.html
header = 
    unlines ["---",
             "title: %s",
             "published: %s",
             "modified: %s",
             "tags: %s",
             "type: uncategorized",
             "showTOC: True",
             "---"]

twToMd :: String -> String -> String
twToMd date s = 
    let 
        (title, body, tags) = extractTitleAndTags s
        tagString = intercalate ", " tags
        simpReplacers = [simpleR "\n&\n#8217;" "'",
                         simpleR "\n&\n#8220;" "\"",
                         simpleR "\n&\n#8221;" "\"",
                         simpleR "\n&\n#8230;" "..."]
        s' = body |> foldIterate replaceParse (simpReplacers++[listReplacer, italicsReplacer, boldReplacer, quoteReplacer, linkReplacer, wikilinkReplacer, headingReplacer, escapeReplacer])
    in
      (printf header title date date tagString)++"\n"++s'

getAbsDirectoryContents :: FilePath -> IO [FilePath]
getAbsDirectoryContents dir =
    getDirectoryContents dir >>= mapM (canonicalizePath . (dir </>))

mainF :: FilePath -> FilePath -> IO ()
mainF dir dest = do
  date <- getDate
  files <- getAbsDirectoryContents dir
  let txtFiles = filter ((==".txt") . takeExtension) files
  for_ txtFiles (\f -> do
                 let fname = takeFileName f
                 contents <- readFile fname
                 let converted = twToMd date contents
                 writeFile (dest </> (replaceExtension fname "md")) converted)

main = do
  args <- getArgs
  mainF (args!!0) (args!!1)

