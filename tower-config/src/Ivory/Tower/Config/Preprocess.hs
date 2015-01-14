
module Ivory.Tower.Config.Preprocess
  ( getPreprocessedFile
  ) where

import Data.Either (lefts, rights)
import qualified Data.ByteString.Char8 as B
import System.FilePath
import System.Directory

data Line
  = Literal B.ByteString
  | Include Importance FilePath
  deriving (Eq, Show)

data Importance
  = Mandatory
  | Optional
  deriving (Eq, Show)

directive :: String -> B.ByteString -> Maybe FilePath
directive tag l =
  if and [ l /= B.empty, h == B.empty, t' == B.empty ]
     then Just (B.unpack h')
     else Nothing
  where
  (h , t ) = beforeafter (B.pack ("#" ++ tag ++ " \"")) l
  (h', t') = beforeafter (B.pack "\"") t
  beforeafter s m = let (b, a) = B.breakSubstring s m
                        a' = B.drop (B.length s) a
                    in (b,a')

getLines :: FilePath -> IO [Line]
getLines f = do
  b <- B.readFile f
  let ls = B.lines b
  return (map mkln ls)
  where
  mkln b = case directive "include" b of
    Just s -> Include Mandatory s
    Nothing -> case directive "optional" b of
      Just s -> Include Optional s
      Nothing -> Literal b

findInclude :: FilePath -> [FilePath] -> IO (Maybe FilePath)
findInclude _ [] = return Nothing
findInclude f (p:ps) = do
  e <- doesFileExist fp
  if e then return (Just fp)
       else findInclude f ps
  where fp = p </> f

-- This has terrible worst case complexity because unlines is strict.
-- I suspect config files will be small enough that it wont matter.
-- XXX this will loop forever on circular dependencies
getPreprocessedFile :: FilePath -> [FilePath]
                    -> IO (Either String B.ByteString)
getPreprocessedFile root path = aux (Include Mandatory root)
  where
  run f = do
    ls <- getLines f
    bs <- mapM aux ls
    case (lefts bs) of
      [] -> return (Right (B.unlines (rights bs)))
      es -> return (Left ("In file " ++ f ++ ": " ++ unlines es))
  aux (Literal b) = return (Right b)
  aux (Include i p) = do
    found <- findInclude p path
    case found of
      Just f' -> run f'
      Nothing -> case i of
        Mandatory -> return (Left ("could not find include named " ++ p))
        Optional -> return (Right (B.empty))


