
module Tower.Config.File
  ( getConfigFile
  ) where

import Data.Either (lefts, rights)
import qualified Data.ByteString.Char8 as B
--import Tower.Config.Types
import System.FilePath
import System.Directory

data Line = Literal B.ByteString | Include FilePath

includeDirective :: B.ByteString -> Maybe FilePath
includeDirective l =
  case (h == B.empty, t' == B.empty) of
    (True, True) -> Just (B.unpack h')
    _ -> Nothing
  where
  (h , t ) = beforeafter (B.pack "#include \"") l
  (h', t') = beforeafter (B.pack "\"") t
  beforeafter s m = let (b, a) = B.breakSubstring s m
                        a' = B.drop (B.length s) a
                    in (b,a')

getLines :: FilePath -> IO [Line]
getLines f = do
  b <- B.readFile f
  let ls = B.lines b
  return (map mkln ls)
  where mkln b = case includeDirective b of
                    Just s -> Include s
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
-- Also, this will loop forever on circular dependencies
getConfigFile :: FilePath -> [FilePath] -> IO (Either String B.ByteString)
getConfigFile root path = aux (Include root)
  where
  run f = do
    ls <- getLines f
    bs <- mapM aux ls
    case (lefts bs) of
      [] -> return (Right (B.unlines (rights bs)))
      es -> return (Left ("In file " ++ f ++ ": " ++ unlines es))
  aux (Literal b) = return (Right b)
  aux (Include i) = do
    found <- findInclude i path
    case found of
      Just f' -> run f'
      Nothing -> return (Left ("could not find include named " ++ i))


