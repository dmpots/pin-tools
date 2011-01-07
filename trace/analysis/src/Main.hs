module Main where

import qualified Data.IntervalMap.FingerTree as Map
import Data.List
import Data.Maybe
import Data.Word
import System.Environment
import System.Exit
import System.IO
import System.Process
import Numeric

type Address = Word
type Label   = String
type Symtab  = Map.IntervalMap Address Label

main = do
  [nmFile] <- parseArgs
  symtab   <- createNameMap nmFile
  trace    <- hGetContents stdin
  mapM_ (\l -> putStrLn (processLine symtab l)) $ lines trace

processLine :: Symtab -> String -> String
processLine symtab line@('#':_) = line
processLine symtab line =
  case words line of
    [addr]                    -> unwords [addr, label addr]
    [addr,jumpKind,imageName] -> unwords [addr, label addr,jumpKind,imageName]

  where
    label addr = maybe "<EXTERN>" id (lookupAddr addr symtab)

lookupAddr :: String -> Symtab -> Maybe Label
lookupAddr a symtab = do
  addr <- tryReadHex a
  listToMaybe $ map snd (Map.search addr symtab)

createNameMap :: FilePath -> IO Symtab
createNameMap nmFile = do
  nmOut <- readFile nmFile
  return $ foldl' add Map.empty (lines nmOut)
  where
    add m line =
      case words line of
        [symbol, aL, aH] -> maybe m id (insert aL aH symbol m)
        _                -> m
    insert aL aH symbol m
      | aL `seq` aH `seq` symbol `seq` False = Nothing
      | otherwise =  do
          l <- tryReadHex aL
          h <- tryReadHex aH
          Just $ Map.insert (Map.Interval l h) symbol m

tryReadHex :: String -> Maybe Address
tryReadHex a = case readHex (drop0x a) of 
                  [(addr,[])] -> Just addr
                  _ -> Nothing
  where
    drop0x ('0':'x':a) = a
    drop0x          a  = a

parseArgs :: IO [String]
parseArgs = do
  args <- getArgs
  case args of
    [_] -> return args
    _  -> putStrLn "usage: label-trace <exe.symtab>" >> exitFailure
