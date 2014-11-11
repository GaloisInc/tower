{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Tower.Config
import           System.Exit
import           Data.List (intercalate)
import qualified Data.ByteString.Char8 as B

data ClockConfig = ClockConfig Integer Integer deriving (Eq, Show)

instance Configurable ClockConfig where
  fromConfig a = do
    xtal_mhz   <- element "xtalMHz"   a
    sysclk_mhz <- element "sysclkMHz" a
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

parseCC :: B.ByteString -> Maybe ClockConfig
parseCC s = do
  doc <- parse s
  element "clockconfig" doc

getpllfoo :: B.ByteString -> Maybe Integer
getpllfoo s = do
  doc <- parse s
  cc <- element "clockconfig" doc
  pll <- element "pll" cc
  element "foo" pll

getsub1foo :: B.ByteString -> Maybe String
getsub1foo s = do
  doc <- parse s
  cc <- element "clockconfig" doc
  sub1 <- element "sub1" cc
  element "foo" sub1

main :: IO ()
main = do
  test "parseCC"
       (parseCC sample1 `equality` Just (ClockConfig 8 168))

  test "subsectionval"
       (getpllfoo sample1 `equality` Just 999)

  test "subsectionval override"
       (getsub1foo sample1 `equality` Just "overridden")

  test "show"
       (ppValue parsed `equality` canonical)

  trivialfile
  multiincludefile
  exitSuccess
  where
  equality a b = (a,b)
  parsed = case parse sample1 of Just a -> a; Nothing -> error "parsing failed"
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
  f <- getConfigFile "trivial.config" ["./tests/resources1"]
  case f of
    Right bs -> case check bs of
      Just True -> putStrLn "Passed"
      a -> putStrLn ("Failed to parse trivial.config: got " ++ show a) >> exitFailure
    Left e -> putStrLn ("Failed with error: " ++ e) >> exitFailure
  where
  check s = do
    doc <- parse s
    trivial <- element "trivial" doc
    element "foo" trivial


multiincludefile :: IO ()
multiincludefile = do
  putStrLn "get root.config: "
  f <- getConfigFile "root.config" ["./tests/resources1", "./tests/resources2"]
  case f of
    Right bs -> case check bs of
      Just ("at root",True, (2 :: Integer),"in child3") -> putStrLn "Passed"
      a -> putStrLn ("Failed to parse root.config: got " ++ show a) >> exitFailure
    Left e -> putStrLn ("Failed with error: " ++ e) >> exitFailure
  where
  check s = do
    doc <- parse s
    root <- element "rootsection" doc
    rp <- element "root_property" root
    c1 <- element "child1" doc
    c1foo <- element "foo" c1
    c2 <- element "child2" doc
    c2foo <- element "foo" c2
    c3 <- element "child3" doc
    c3foo <- element "foo" c3
    return (rp, c1foo, c2foo, c3foo)

