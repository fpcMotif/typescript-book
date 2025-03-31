module MergeFiles
  ( AppConfig (..)
  , findMergeableFiles
  , mergeFilesContent
  , writeMergedFile
  , MergedFileResult (..)
  ) where

import Control.Exception (IOException, catch, try)
import Control.Monad (filterM, forM)
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), takeExtension, takeFileName, takeDirectory)
import System.IO (IOMode (WriteMode), hClose, openFile)

-- | Configuration for the application
data AppConfig = AppConfig
  { inputDir  :: FilePath
  , outputFile :: FilePath
  } deriving (Show, Eq)

-- | Result of reading and merging files
data MergedFileResult = MergedFileResult
    { mergedContent :: T.Text
    , processedFiles :: [FilePath] -- Relative paths of successfully processed files
    , skippedFiles :: [(FilePath, String)] -- Files skipped due to errors (relative path, error reason)
    } deriving (Show, Eq)

-- | Finds all .txt and .md files in the top level of the given directory.
-- Returns relative paths from the input directory.
findMergeableFiles :: FilePath -> IO (Either String [FilePath])
findMergeableFiles dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure $ Left $ "Input directory does not exist: " ++ dir
    else do
      result <- try (listDirectory dir) :: IO (Either IOException [FilePath])
      case result of
        Left e -> pure $ Left $ "Error reading directory " ++ dir ++ ": " ++ show e
        Right entries -> do
          let fullPaths = map (dir </>) entries
          files <- filterM doesFileExist fullPaths
          let mergeable = filter isMergeableFile files
          -- Make paths relative to the input directory for consistency
          let relativePaths = map takeFileName mergeable
          pure $ Right $ sort relativePaths -- Sort alphabetically
  where
    isMergeableFile :: FilePath -> Bool
    isMergeableFile fp = case map toLower (takeExtension fp) of
                           ".txt" -> True
                           ".md" -> True
                           _ -> False
    toLower :: Char -> Char
    toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + (fromEnum 'a' - fromEnum 'A')) else c

-- | Reads files, formats content with headers/footers, and adds a Table of Contents.
mergeFilesContent :: FilePath -> [FilePath] -> IO MergedFileResult
mergeFilesContent baseDir relativeFiles = do
  results <- forM relativeFiles $ \relPath -> do
    let fullPath = baseDir </> relPath
    let filename = T.pack $ takeFileName relPath
    contentResult <- try (TIO.readFile fullPath) :: IO (Either IOException T.Text)
    pure (relPath, filename, contentResult)

  let toc = T.unlines $
        [ "<!-- Table of Contents -->"
        , "<!-- ===================== -->"
        ] ++ map (T.pack . ("- " ++)) relativeFiles
          ++ ["<!-- ===================== -->", ""]

  let processed = [(path, file, content) | (path, file, Right content) <- results]
  let skipped = [(path, show err) | (path, _, Left err) <- results]

  let formattedContent = T.concat $
        map (\(_, fname, content) -> formatFileContent fname content) processed

  pure MergedFileResult
    { mergedContent = toc <> T.strip formattedContent -- Add TOC and trim final whitespace
    , processedFiles = map (\(path, _, _) -> path) processed
    , skippedFiles = skipped
    }

formatFileContent :: T.Text -> T.Text -> T.Text
formatFileContent filename content =
  T.unlines
    [ ""
    , T.concat ["<!-- ========== Start: ", filename, " ========== -->"]
    , ""
    , T.strip content -- Trim whitespace from individual file content
    , ""
    , T.concat ["<!-- ========== End: ", filename, " ========== -->"]
    , ""
    ]

-- | Writes the merged content to the specified output file.
-- Creates the output directory if it doesn't exist.
writeMergedFile :: FilePath -> T.Text -> IO (Either String ()) -- Use Text for content
writeMergedFile outFile content = do
  let outDir = takeDirectory outFile
  -- Ensure the output directory exists
  createResult <- try (createDirectoryIfMissing True outDir) :: IO (Either IOException ())
  case createResult of
    Left e -> pure $ Left $ "Failed to create output directory " ++ outDir ++ ": " ++ show e
    Right _ -> do
      -- Write the file
      writeResult <- try $ do
          handle <- openFile outFile WriteMode
          TIO.hPutStr handle content -- Use TIO.hPutStr for Text
          hClose handle
      case writeResult of
        Left e -> pure $ Left $ "Failed to write to output file " ++ outFile ++ ": " ++ show e
        Right _ -> pure $ Right () 