import           Control.Monad.Extra (concatMapM)
import           Data.List           (elemIndex)
import           System.Directory    (doesDirectoryExist, listDirectory)

todo = error "Todo"

orElse :: a -> Maybe a -> a
orElse _ (Just x) = x
orElse x _        = x

outputType ext =
  case ext of
    "erl"  -> "beam"
    "ts"   -> "js"
    "java" -> "class"
    "cs"   -> "dll"
    "fs"   -> "dll"
    _      -> "o"

compileCommand ext =
  case ext of
    "hs"    -> Just "ghc $(HASKELL_FLAGS) "
    "c"     -> Just "cc $(CFLAGS) "
    "cpp"   -> Just "c++ $(CXXFLAGS) "
    "rs"    -> Just "rustc $(RUST_FLAGS) "
    "erl"   -> Just "erl -compile $(ERLANG_FLAGS) "
    "java"  -> Just "javac $(JAVA_FLAGS) "
    "scala" -> Just "scalac $(SCALA_FLAGS) "
    "cs"    -> Just "rosyln $(CS_FLAGS) "
    _       -> Nothing

splitName :: String -> (String, String)
splitName name = splitAt finalDot name
  where
    reverseLength x = length name - x
    finalDot = orElse 0 . fmap reverseLength . elemIndex '.' . reverse $ name

generateMakeRule :: String -> String
generateMakeRule filename =
  case compiler of
    Just comp -> concat [output, ": ", filename, "\n\t", comp, filename]
    Nothing   -> ""
  where
    (name, ext) = splitName filename
    output = name ++ outputType ext
    compiler = compileCommand ext

getRecursiveFiles :: FilePath -> IO [FilePath]
getRecursiveFiles path = do
  isFolder <- doesDirectoryExist path
  here <- listDirectory path
  if isFolder
    then concatMapM getRecursiveFiles here
    else return [path]

main :: IO ()
main =
  putStrLn . unlines . filter (/= []) . map generateMakeRule =<<
  listDirectory "."
