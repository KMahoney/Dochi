module Main where

import System.IO
import System.Environment
import System.Console.GetOpt

import Parse



-- filename utils

basename = reverse . dropWhile (== '.') . dropWhile (/= '.') . reverse
addslash dir = if ((head $ reverse dir) == '/') then dir else (dir ++ "/")



-- options

data Options = Options { make :: Bool
                       , debug :: Bool
                       , verbose :: Bool }


defaultOptions = Options { make = False
                         , debug = False
                         , verbose = False }

options :: [OptDescr (Options -> Options)]
options = [ Option ['m'] ["make"] (NoArg (\o -> o {make=True})) "Compile input"
          , Option [] ["debug"] (NoArg (\o -> o {debug=True})) "Debug info"
          , Option ['v'] ["verbose"] (NoArg (\o -> o {verbose=True})) "Verbose mode" ]

getopts :: [String] -> IO (Options, [String])
getopts args = 
    case getOpt Permute options args of
      (o,n,[]) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> error $ concat errs ++ usageInfo header options

header = "Usage: dochi [OPTION...] input"


-- main entry

main = do
  args <- getArgs
  (opts, files) <- getopts args

  case (length files) of
    0 -> interactive
    _ -> mapM_ runFile files


-- interactive

interactive :: IO ()
interactive =  putStrLn "interactive"


-- run file

runFile :: String -> IO ()
runFile name = do
  content <- readFile name
  case (dochiParse name content) of
    Left err -> hPrint stderr err >> error "Parse Error"
    Right p -> putStrLn $ show p
