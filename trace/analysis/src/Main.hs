module Main where

import Data.List
import qualified Data.IntMap as Map
import System.Environment
import System.Exit
import System.Process
import Numeric

type Address = Int
type Label   = String
type Symtab  = Map.IntMap Label

main = do
  [trace, nmFile] <- parseArgs
  symtab      <- createNameMap nmFile
  t <- readFile trace
  mapM_ (\l -> putStrLn (processLine symtab l)) $ lines t

processLine :: Symtab -> String -> String
processLine symtab line@('#':_) = line
processLine symtab line =
  case lookupAddr line symtab of
    Just label -> label
    Nothing    -> line

lookupAddr :: String -> Symtab -> Maybe Label
lookupAddr a symtab = do
  addr <- tryReadHex a
  Map.lookup addr symtab

createNameMap :: FilePath -> IO Symtab
createNameMap nmFile = do
  --nmOut <- readProcess "nm" ["-n", exe] ""
  nmOut <- readFile nmFile
  return $ makeMap nmOut
  where
    makeMap :: String -> Symtab
    makeMap out =
      foldl' (\m line -> case words line of
                            [a,t,l] -> 
                              maybe m (\a -> Map.insert a l m) (tryReadHex a)
                            _       -> m) 
      Map.empty 
      (lines out)

tryReadHex :: String -> Maybe Int
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
    [t,e] -> return args
    _     -> putStrLn "usage: label-trace <trace> <exe>" >> exitFailure
  
