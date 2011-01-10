{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad
import Data.Char
import Data.Either
import System.IO
import System.Environment
import System.Exit
import Text.Regex.Posix
import Text.Regex.Posix.String

--------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------
data Rule = Rule {
    column :: ColumnSpec, 
    regex :: Regex, 
    replacement :: String,
    regexStr :: String
  }
instance Show Rule where
  show r = col ++ " /" ++ reg ++ "/ " ++ "{" ++ rep ++ "}"
    where col = show $ column r
          reg = show $ regexStr r
          rep = show $ replacement r

data ColumnSpec = Any | Column ![Int]

instance Show ColumnSpec where
  show Any        = "*"
  show (Column i) = show i


type Script = [Rule]

--------------------------------------------------------------------------
-- Script Parsing
--------------------------------------------------------------------------
data ParseError = ParseError {lineNum :: Int, message :: String}
  deriving(Read, Show)

ruleRegex :: String
ruleRegex = 
  "^([[:digit:]]+(,[[:digit:]]+)*|[*])[[:space:]]+/(.*)/[[:space:]]+[{](.*)[}]$"

parseScript :: FilePath -> IO (Either [ParseError] Script)
parseScript inputFile = do
  input   <- readFile inputFile
  mbRegex <- compile defaultCompOpt defaultExecOpt ruleRegex
  case mbRegex of
    Left (_, msg) -> return $ Left [(ParseError 0 msg)]
    Right ruleParser -> do
      let inputLines    = zip lexed [1..]
          lexed         = filter (not . isComment) (lines input)
          isComment   s = case s of '#':_ -> True; "" -> True; _-> False
      parsed <- mapM (parseLine ruleParser) inputLines
      let (errs, rule) = partitionEithers parsed
      case errs of
        [] -> return $ Right rule
        _  -> return $ Left  errs

-- one rule for each column specified
parseLine :: Regex -> (String, Int) -> IO (Either ParseError Rule)
parseLine ruleParser (line, lineNo)  = do
  mbMatch <- regexec ruleParser line
  case mbMatch of
    Left (_,err) -> return $ makeError ("Regex error: "++err)
    Right (Just (_,_,_,[cols,_,regexp,replace])) -> makeRule cols regexp replace
    Right _      -> do return $ makeError ("Unable to parse line: "++line)
  where 
  makeRule :: String -> String -> String -> IO (Either ParseError Rule)
  makeRule cols regexp replace = do
    case colSpec of
      Nothing -> return $ makeError "Unable to parse column spec"
      Just cs -> do
        matchSpec <- mkRegex regexp
        case matchSpec of 
          Nothing -> return $ makeError "Unable to parse regex"
          Just re -> return $ Right $ Rule cs re replace regexp
    where colSpec   = parseCols cols
  makeError err = Left (ParseError lineNo err)
  mkRegex str = do 
    regexp <- compile defaultCompOpt defaultExecOpt str
    case regexp of
      Left _  -> return Nothing
      Right r -> return (Just r)

parseCols :: String -> Maybe ColumnSpec
parseCols "*" = Just Any
parseCols cs  = fmap (Column) $ sequence $ map tryReadInt (split ',' cs)

split       :: Eq a => a -> [a] -> [[a]]
split _ []  =  []
split c s   =  let (l, s') = break (== c) s
                in  l : case s' of
                          []      -> []
                          (_:s'') -> split c s''

tryReadInt :: String -> Maybe Int
tryReadInt a = case reads a of [(i,[])] -> Just i; _ -> Nothing

--------------------------------------------------------------------------
-- Script Running
--------------------------------------------------------------------------
runScript :: Script -> Handle -> Handle -> IO ()
runScript script inh outh = do
  input <- lines `liftM` hGetContents inh
  mapM_ (\l -> processLine script l >>= hPutStrLn outh) input

processLine :: Script -> String -> IO String
processLine script line = do
  processed <- mapM (processField script) fields
  return $ unwords processed
  where
  fields = zip (words line) [1..]

processField :: Script -> (String, Int) -> IO String
processField script f@(field, _col) = do
  mbRule <- findFirst script f
  case mbRule of
    Nothing -> return field
    Just (r, groups) -> return $ substitute groups (replacement r)

findFirst :: Script -> (String, Int) -> IO (Maybe (Rule, [String]))
findFirst []     _ = return Nothing
findFirst (r:rs) f@(s, col) 
  | not ((column r) `spansColumn` col) = findFirst rs f
  | otherwise = do
  res <- regexec (regex r) s
  case res of
    Right (Just (_,_,_,ms)) -> return $ Just (r, ms)
    _                       -> findFirst rs f

spansColumn :: ColumnSpec -> Int -> Bool
spansColumn Any _         = True
spansColumn (Column cs) c = c `elem` cs

substitute :: [String] -> String -> String
substitute groups string = go string
  where
  go []                     = []
  go ('$':c:cs) | isDigit c = groups `safeIndex` (digitToInt c - 1) ++ go cs
  go ('$':'$':cs)           = '$' : go cs
  go (c:cs)                 =  c  : go cs

  safeIndex []     _   = ""
  safeIndex (x:_)  0   = x
  safeIndex (_:xs) !n  = safeIndex xs (n-1)
 
--------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------
main :: IO ()
main = do
  script <- parseScriptOrDie
  --mapM_ (putStrLn . show) script
  runScript script stdin stdout
  return ()

parseScriptOrDie :: IO Script
parseScriptOrDie = do
  [scriptFile] <- parseArgs
  script <- parseScript scriptFile
  case script of
    Left err -> putStrLn (show err) >> exitFailure
    Right s  -> return s

parseArgs :: IO [String]
parseArgs = do
  args <- getArgs
  case args of
    [_] -> return args
    _  -> putStrLn "usage: fawk <script>" >> exitFailure

