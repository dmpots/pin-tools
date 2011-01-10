{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad
import Data.Char
import Data.ByteString.Lazy.Char8(ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Either
import System.IO
import System.Environment
import System.Exit
import Text.Regex.Posix
--import Text.Regex.Posix.String
import qualified Text.Regex.Posix.String as SR
import Text.Regex.Posix.ByteString.Lazy

--------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------
data Rule = Rule {
    column :: ColumnSpec, 
    regex :: Regex, 
    replacement :: ByteString,
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
  mbRegex <- SR.compile defaultCompOpt defaultExecOpt ruleRegex
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
  mbMatch <- SR.regexec ruleParser line
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
          Just re -> return $ Right $ Rule cs re (B.pack replace) regexp
    where colSpec   = parseCols cols
  makeError err = Left (ParseError lineNo err)
  mkRegex str = do 
    regexp <- compile defaultCompOpt defaultExecOpt (B.pack str)
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
  input <- B.lines `liftM` B.hGetContents inh
  mapM_ (\l -> processLine script l >>= output >> output endl) input
  where 
  output s = B.hPut outh s
  endl     = B.pack "\n"

processLine :: Script -> ByteString -> IO ByteString
processLine script line = do
  processed <- mapM (processField script) fields
  return $ B.unwords processed
  where
  fields = zip (B.words line) [1..]

processField :: Script -> (ByteString, Int) -> IO ByteString
processField script f@(field, _col) = do
  mbRule <- findFirst script f
  case mbRule of
    Nothing -> return field
    Just (r, groups) -> return $ subs groups (replacement r)

findFirst :: Script -> (ByteString, Int) -> IO (Maybe (Rule, [ByteString]))
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

subs :: [B.ByteString] -> B.ByteString -> B.ByteString
subs groups rhs = B.concat (go rhs)
  where
  go str | B.null str = []
  go str = let (b,a) = B.break (=='$') str
               (new,rest) = sub a
           in  b : new : go rest
  sub s 
    | B.length s >= 2 && isDigit (B.index s 1) = 
      (groups `safeIndex` (digitToInt (B.index s 1) - 1), B.drop 2 s)
    | otherwise = (B.empty, s)
 
  safeIndex []     _   = B.empty
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

