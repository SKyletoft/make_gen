import           Control.Monad       (mapM)
import           Control.Monad.Extra (concatMapM)
import           Data.List           (elemIndex)
import           Data.Maybe          (catMaybes, mapMaybe)
import           System.Directory    (doesDirectoryExist, listDirectory)

todo = error "Todo"

data RuleInfo =
  RuleInfo String String String String
  deriving (Show, Eq)

orElse :: a -> Maybe a -> a
orElse _ (Just x) = x
orElse x _        = x

outputType ext =
  case ext of
    "erl"   -> "beam"
    "ts"    -> "js"
    "java"  -> "class"
    "scala" -> "class"
    "cs"    -> "dll"
    "fs"    -> "dll"
    _       -> "o"

compileCommand ext =
  case ext of
    "hs"    -> Just "ghc $(HASKELL_FLAGS) -c -o "
    "c"     -> Just "cc $(CFLAGS) -c -o "
    "cpp"   -> Just "c++ $(CXXFLAGS) -c -o "
    "rs"    -> Just "rustc $(RUST_FLAGS) -c -o "
    "erl"   -> Just "erl -compile $(ERLANG_FLAGS) -o "
    "java"  -> Just "javac $(JAVA_FLAGS) -o "
    "scala" -> Just "scalac $(SCALA_FLAGS) -o "
    "cs"    -> Just "rosyln $(CS_FLAGS) -o "
    _       -> Nothing

splitName :: String -> (String, String)
splitName name = splitAt finalDot name
  where
    reverseLength x = length name - x
    finalDot = orElse 0 . fmap reverseLength . elemIndex '.' . reverse $ name

printGenerateMakeRule :: RuleInfo -> String
printGenerateMakeRule (RuleInfo input compiler output _) =
  concat [output, ": ", input, "\n\t", compiler, output, " ", input, "\n"]

makeRule :: String -> Maybe RuleInfo
makeRule filename =
  fmap (\comp -> RuleInfo filename comp output outExt) compiler
  where
    (name, ext) = splitName filename
    outExt = outputType ext
    output = name ++ outExt
    compiler = compileCommand ext

getRecursiveFiles :: FilePath -> IO [FilePath]
getRecursiveFiles path = do
  isFolder <- doesDirectoryExist path
  let addPathTo file = path ++ '/' : file
  if isFolder
    then concatMapM getRecursiveFiles
      . map addPathTo
      =<< listDirectory path
    else return [path]

stripThisFolder :: FilePath -> FilePath
stripThisFolder ('.':'/':xs) = xs
stripThisFolder xs           = xs

join :: String -> [String] -> String
join mid (a:b:cs) = a ++ mid ++ join mid (b : cs)
join _ [a]        = a
join _ []         = []

cleanRule :: [RuleInfo] -> String
cleanRule =
  ("clean:\n\t-rm link\n" ++) .
  unlines . map (("\t-rm " ++) . (\(RuleInfo _ _ a _) -> a))

linkRule :: [RuleInfo] -> String
linkRule rules = concat ["link: ", inputs, "\n\tcc ", inputs, " -o link"]
  where
    inputs =
      unwords .
      map (\(RuleInfo _ _ output _) -> output) .
      filter (\(RuleInfo _ _ _ ext) -> ext == "o") $
      rules

phony :: String
phony = ".PHONY:\n\tclean"

main :: IO ()
main = do
  files <- fmap (map stripThisFolder) . getRecursiveFiles $ "."
  let rules = mapMaybe makeRule files
  let clean = cleanRule rules
  let link = linkRule rules
  putStrLn . unlines . (++ [link, clean, phony]) . map printGenerateMakeRule $
    rules
