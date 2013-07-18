{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import System.Environment
import System.Exit
import System.Process
import System.IO
import System.FilePath
import Text.Printf
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as T
import Data.List
import Data.Functor
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Data.Maybe
import qualified Data.Text.Lazy.IO as Text
import qualified Data.Text.Lazy as Text
import Data.Char
import Data.Either
import System.Console.GetOpt
import Text.Blaze.Html5 hiding (head, option)
import Text.Blaze.Html5.Attributes hiding (id, title, style)
import qualified Text.Blaze.Html5 as HTML
import qualified Text.Blaze.Html5.Attributes as HTML
import Text.Blaze.Renderer.Text (renderMarkup)
import Data.Monoid

options =
    [ Option [] ["package"] (ReqArg Left "PKG") "package of interest"
    , Option [] ["html"] (NoArg (Right True))   "html output"
    ]
    

descr = usageInfo "Usage: import-report [OPTION...] pkg-id/mod1.imports pkg-id/mod2.imports ...\n" options
    ++ unlines [
        "",
        "This program analyizes Haskell import statements.",
        "",
        "The --package argument can restrict the set of modules we are interested in.",
        "It will use ghc-pkg to find out the list of modules in that package.",
        "",
        "The remaining parameters should be paths to *.imports files as created by",
        "ghc -ddump-minimal-imports. It is asssumed that the directory name of these",
        "files is their package name.",
        "",
        "It will then generate a CSV value file with these columns:",
        "module,identifier,number of packages using it",
        "An artificial entry for module Anything is included to contain the total number",
        "of packages.",
        "",
        "With --html, a shiny HTML report is generated instead."
        ]

modulesOf :: String -> IO [String]
modulesOf pkg = do
    let opts = ["field",pkg,"exposed-modules"]
    output <- readProcess "ghc-pkg" opts ""
    when (null (words output)) $ do
        putStr $ "Empty output of ghc-pkg " ++ unwords opts
        exitFailure
    when (head (words output) /= "exposed-modules:") $ do
        putStr $ "Failed to parse output of ghc-pkg " ++ unwords opts
        exitFailure
    return (tail (words output))


type Import = (String, Maybe String)

-- Map from package
type ImportMap = Map String (Set Import)

-- Map from id
type ImportStats = [(Import, Int)]

-- Fix haskell (_ may be the beginning of an identifier)
T.TokenParser {..} = T.makeTokenParser (haskellDef {
    T.identStart = letter <|> oneOf "_",
    T.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~" <|> satisfy (not.isAscii),
    T.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~" <|> satisfy (not.isAscii),
    T.reservedNames = []
    })

parseModuleName :: Parser String
parseModuleName = intercalate "." <$> identifier `sepBy` char '.'

parseOp :: Parser String
parseOp =
    ((\s -> "(" ++ s ++ ")") <$> (lexeme $ between (char '(') (char ')') operator)) <?> "operator"

parseName :: Parser String
parseName = do
    optional (reserved "type")
    parseOp <|> identifier

parseId :: Parser [String]
parseId = do
    i <- parseName
    more <- option [] (parens (commaSep (reservedOp ".." <|> void parseName)))
    return $ if null more then [i] else [i, i++"(..)"]

importParser :: Parser [Import]
importParser = do
    reserved "import"
    optional $ reserved "safe"
    optional $ reserved "qualified"
    optional $ stringLiteral
    mod <- parseModuleName
    optional $ reserved "as" >> parseModuleName
    ids <- concat <$> parens (commaSep parseId)
    return $ (mod, Nothing) : [ (mod, Just id) | id <- ids ]


importsParser :: Parser [Import]
importsParser = between whiteSpace eof (concat <$> many importParser)

readImports :: FilePath -> IO ImportMap
readImports fn = do
    let pkgName = last $ init $ splitDirectories $ fn
    contents <- Text.unpack <$> Text.readFile fn
    case parse importsParser fn contents of
        Left err -> do
            hPrintf stderr "Failed to parse import specification:\n"
            hPutStrLn stderr (show err)
            exitFailure
        Right imps -> do
            let imps = either (error.show) id $ parse importsParser fn contents
            return $ M.singleton pkgName $ S.fromList imps

printCSV :: [String] -> [String] -> ImportStats -> IO ()
printCSV _ pkg stats = do
    printf "%s,%s,%d\n" "Anything" "" (length pkg)
    forM_ stats $ \((m,i),n) -> 
        printf "%s,%s,%d\n" m (fromMaybe "" i) (n::Int)
    

printHTML :: [String] -> [String] -> ImportStats -> IO ()
printHTML interesting pkgs stats = do
    Text.putStr $ renderMarkup $ do
    docType
    html $ do
        HTML.head $ do
            title $ toMarkup "Import analyis"
            link ! rel (toValue "stylesheet") ! type_ (toValue "text/css") ! href (toValue "style.css")
            script ! src (toValue "jquery.js") $ mempty
            script ! src (toValue "script.js") $ mempty
        body $ HTML.div ! HTML.id (toValue "content") $ do
            h1 $ toMarkup "Import analysis"
            p $ toMarkup $ "This page analyses the import statements of " ++ show (length pkgs) ++ " Haskell packages and counts the number of imports of each module and symbol from the list of interesting packages (in this case: " ++ concat (intersperse ", " interesting) ++ ")."
            table $ do
                thead $ tr $ do
                    th mempty
                    th (toMarkup "Module/symbol")
                    th (toMarkup "used by")
                    th mempty
                tbody $ do
                    modRow "Anything" (length pkgs)
                    mconcat [ case i of
                                Just i -> idRow i n
                                Nothing -> modRow m n
                            | ((m,i),n) <- stats ]
            p $ do
                toMarkup $ "These packages were analized in the making of this report: "
                toMarkup $ concat $ intersperse ", " pkgs
            HTML.div ! HTML.id (toValue "footer") $ do
                p $ do
                    toMarkup "Report generated by "
                    a ! href (toValue "mailto:mail@joachim-breitner.de") $
                        toMarkup "Joachim Breitner"
                    toMarkup ". "
                    a ! href (toValue "http://darcs.nomeata.de/import-analizer") $ 
                        toMarkup "Source"
                    toMarkup " available."
 where
    collapse = toMarkup ""
    modRow m n = 
        tr ! class_ (toValue "modrow") $ do
            td ! class_ (toValue "expander") $ collapse
            td ! class_ (toValue "name")     $ toMarkup m
            td ! class_ (toValue "count")    $ toMarkup n
            td ! class_ (toValue "graph")    $ mempty
    idRow i n = 
        tr ! class_ (toValue "idrow") ! hidden (toValue "hidden")  $ do
            td mempty
            td ! class_ (toValue "name")     $ toMarkup i
            td ! class_ (toValue "count")    $ toMarkup n
            td ! class_ (toValue "graph")    $ mempty

main = do
    argv <- getArgs
    (pkgs, html, importFiles) <- case getOpt RequireOrder options argv of
        (o,n,[]) | not (null n) -> return (lefts o, or (rights o), n)  
        (o,_,e) -> do
            hPutStr stderr (concat e)
            hPutStr stderr descr
            exitFailure
            
    interestingModules <- S.fromList . concat <$> mapM modulesOf pkgs
    unless (null pkgs) $
        hPrintf stderr "Found %d interesting modules.\n" (S.size interestingModules)

    importMap <- M.unionsWith (S.union) <$> mapM readImports importFiles
    hPrintf stderr "Analized imports from %d packages.\n" (M.size importMap)
    -- Invert the stats
    let stats = M.unionsWith (+) [
            M.singleton imp 1
            |   imps <- M.elems importMap
            ,   imp <- S.toList imps
            ]
    hPrintf stderr "Found %d imported symbols in total.\n" (M.size stats)
    let filtered =
            if null pkgs
            then stats
            else  M.filterWithKey (\(m,_) _ -> m `S.member` interestingModules) stats
    unless (null pkgs) $
        hPrintf stderr "Found %d imported symbols from selected packages.\n" (M.size filtered)

    let sorted = flip sortBy (M.toAscList filtered) $ \((m1,i1),n1) ((m2,i2),n2) ->
            if m1 == m2 then
                if isNothing i1 then LT else
                if isNothing i2 then GT else
                n2 `compare` n1
            else (filtered M.! (m2,Nothing)) `compare` (filtered M.! (m1,Nothing))

    (if html then printHTML else printCSV) pkgs (M.keys importMap) sorted
