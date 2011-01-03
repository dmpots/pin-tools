module Main where

import qualified Data.IntervalMap.FingerTree as Map
import Data.List
import Data.Maybe
import Data.Word
import System.Environment
import System.Exit
import System.Process
import Numeric

type Address = Word
type Label   = String
type Symtab  = Map.IntervalMap Address Label

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
  listToMaybe $ map snd (Map.search addr symtab)

createNameMap :: FilePath -> IO Symtab
createNameMap nmFile = do
  nmOut <- readFile nmFile
  let (m, sym, addrLow) = makeMap nmOut
      m'  = Map.insert (Map.point addrLow) sym m
      m'' = Map.insert (Map.Interval (addrLow + 1) maxBound) "ABOVE" m'
  return m''
  where
    makeMap :: String -> (Symtab, Label, Address)
    makeMap out =
      foldl' (\prev@(m,sym,addrLow) line -> 
                case words line of
                  [a,t,l] -> maybe prev (add addrLow sym m l)(tryReadHex a)
                  _       -> prev)
      (Map.empty, "BELOW", 0)
      (lines out)
      where
        add aL nL iMap nH aH
          | aL `seq` aH `seq` nL `seq` nH `seq` False = undefined
          | otherwise = (Map.insert (Map.Interval aL aH) nL iMap, nH, aH)

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
    [t,e] -> return args
    _     -> putStrLn "usage: label-trace <trace.out> <nm.out>" >> exitFailure
  
