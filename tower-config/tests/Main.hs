{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Ivory.Tower.Config
import           Ivory.Tower.Config.Preprocess
import           Ivory.Tower.Config.Document
import           Ivory.Tower.Config.Options
import           Ivory.Tower.Config.TOML
import           System.Exit
import           Data.List (intercalate)
import qualified Data.ByteString.Char8 as B
import qualified Ivory.Tower.Options as O

data ClockConfig = ClockConfig Integer Integer deriving (Eq, Show)

clockConfigParser :: ConfigParser ClockConfig
clockConfigParser = do
  xtal_mhz   <- subsection "xtalMHz"   integer
  sysclk_mhz <- subsection "sysclkMHz" integer
  return (ClockConfig xtal_mhz sysclk_mhz)

sample1 :: B.ByteString
sample1 = B.pack $ unlines
  [ "bar = \"hello world\""
  , "[clockconfig.sub1.sub2]"
  , "foo = 121212"
  , "[clockconfig.sub1]"
  , "foo = 11111"
  , "[clockconfig]"
  , "xtalMHz = 8"
  , "sysclkMHz = 168"
  , "[clockconfig.pll]"
  , "foo = 999"
  , "[clockconfig.sub2]"
  , "foo = [ 22222, \"heterogenous array\"]"
  , "[clockconfig.sub1]"
  , "foo = \"overridden\""
  ]

testParse :: B.ByteString -> ConfigParser a -> Maybe a
testParse bs p = do
  doc <- tomlParse bs
  case runConfigParser p (Left doc) of
    Right v -> Just v
    Left _ -> Nothing

parseCC :: B.ByteString -> Maybe ClockConfig
parseCC s = testParse s
          $ subsection "clockconfig" clockConfigParser

getpllfoo :: B.ByteString -> Maybe Integer
getpllfoo s = testParse s
            $ subsection "clockconfig"
            $ subsection "pll"
            $ subsection "foo"
            $ integer

getsub1foo :: B.ByteString -> Maybe String
getsub1foo s = testParse s
             $ subsection "clockconfig"
             $ subsection "sub1"
             $ subsection "foo"
             $ string

main :: IO ()
main = do
  test "parseCC"
       (parseCC sample1 `equality` Just (ClockConfig 8 168))

  test "subsectionval"
       (getpllfoo sample1 `equality` Just 999)

  test "subsectionval override"
       (getsub1foo sample1 `equality` Just "overridden")

  test "show"
       (ppValue (Left parsed) `equality` canonical)

  trivialfile
  multiincludefile
  optionparsing
  exitSuccess
  where
  equality a b = (a,b)
  parsed = case tomlParse sample1 of
            Just a -> a
            Nothing -> error "parsing failed"
  canonical = intercalate "\n"
    [ "bar = \"hello world\""
    , "[clockconfig]"
    , "sysclkMHz = 168"
    , "xtalMHz = 8"
    , "[clockconfig.pll]"
    , "foo = 999"
    , "[clockconfig.sub1]"
    , "foo = \"overridden\""
    , "[clockconfig.sub1.sub2]"
    , "foo = 121212"
    , "[clockconfig.sub2]"
    , "foo = [ 22222, \"heterogenous array\" ]"
    ]

  test n (a,b) | a == b = putStrLn (n ++ ": Passed")
               | otherwise = do putStrLn (n ++ ": Failed\n" ++
                                       "\tlhs was " ++ show a ++ "\n" ++
                                       "\trhs was " ++ show b)
                                exitFailure


trivialfile :: IO ()
trivialfile = do
  putStrLn "get trivial.config: "
  f <- getPreprocessedFile "trivial.config" ["./tests/resources1"]
  case f of
    Right bs -> case check bs of
      Just True -> putStrLn "Passed"
      a -> putStrLn ("Failed check of trivial.config: got " ++ show a)
           >> exitFailure
    Left e -> putStrLn ("Failed with error: " ++ e) >> exitFailure
  where
  check s = testParse s
          $ subsection "trivial"
          $ subsection "foo"
          $ bool

multiincludefile :: IO ()
multiincludefile = do
  putStrLn "get root.config: "
  f <- getDocument "root.config" ["./tests/resources1", "./tests/resources2"]
  case f of
    Right v -> case runConfigParser parser (Left v) of
      Right ("at root",True, (2 :: Integer),"in child3") -> putStrLn "Passed"
      Right res -> err ("Wrong result when parsing root.config: " ++ show res)
      Left e -> err ("Failed to parse root.config: got " ++ show e)
    Left e -> err ("Failed with error: " ++ e)
  where
  err s = putStrLn s >> exitFailure
  parser = do
    rp    <- subsection "rootsection"
           $ subsection "root_property"
           $ string
    c1foo <- subsection "child1"
           $ subsection "foo"
           $ bool
    c2foo <- subsection "child2"
           $ subsection "foo"
           $ integer
    c3foo <- subsection "child3"
           $ subsection "foo"
           $ string
    return (rp, c1foo, c2foo, c3foo)

optionparsing :: IO ()
optionparsing = do
  putStrLn "parse options:\n"
  (cfgopts, extras) <- getCfgOpts topts
  d <- getDocument (cfgopts_configfile cfgopts)
                   (cfgopts_configpath cfgopts)
  case (d, O.topts_args extras) of
    (Right _, []) -> putStrLn "Passed"
    (Left e, _) -> putStrLn ("Failed with error: " ++ e) >> exitFailure
    (_, rs) -> putStrLn ("Failed to parse options: " ++ unwords rs) >> exitFailure
  where
  topts = O.TOpts
    { O.topts_args = [ "--conf-file=root.config"
                     , "--conf-path=./tests/resources1"
                     , "--conf-path=./tests/resources2"
                     ]
    , O.topts_error = error "option parsing fail"
    }
