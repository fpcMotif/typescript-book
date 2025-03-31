module Main (main) where

import MergeFiles
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified Data.Text as T
import Data.Foldable (traverse_)

-- | Command line options parser
optsParser :: Parser AppConfig
optsParser = AppConfig
    <$> strOption
        ( long "input"
       <> short 'i'
       <> metavar "INPUT_DIR"
       <> value "." -- Default value
       <> help "Directory containing .txt and .md files (defaults to current directory)"
       <> showDefault )
    <*> strOption
        ( long "output"
       <> short 'o'
       <> metavar "OUTPUT_FILE"
       <> help "Path to the merged output file (required)" )

-- | Parser info for command line options
parserInfo :: ParserInfo AppConfig
parserInfo = info (optsParser <**> helper)
  ( fullDesc
 <> progDesc "Merges all .txt and .md files from INPUT_DIR into OUTPUT_FILE, adding a table of contents and file headers."
 <> header "merge-files-haskell - A file merging utility" )

-- | Print an error message to stderr and exit.
dieWithError :: String -> IO a
dieWithError msg = do
    hPutStrLn stderr $ "Error: " ++ msg
    exitFailure

main :: IO ()
main = do
    config <- execParser parserInfo
    putStrLn $ "Input directory: " ++ inputDir config
    putStrLn $ "Output file: " ++ outputFile config

    -- Find files
    findResult <- findMergeableFiles (inputDir config)
    files <- case findResult of
        Left errMsg -> dieWithError errMsg
        Right [] -> do
            putStrLn $ "No .txt or .md files found in directory: " ++ inputDir config
            pure [] -- Continue gracefully, resulting in an empty output file with TOC
        Right foundFiles -> do
            putStrLn $ "Found " ++ show (length foundFiles) ++ " files to merge:"
            traverse_ (putStrLn . ("  - " ++)) foundFiles
            pure foundFiles

    -- Merge content
    mergeResult <- mergeFilesContent (inputDir config) files

    -- Report skipped files
    if not (null (skippedFiles mergeResult))
        then do
            hPutStrLn stderr "\nWarnings:"
            traverse_ (\(fp, err) -> hPutStrLn stderr $ "  - Could not read file " ++ fp ++ ". Skipping. Error: " ++ err) (skippedFiles mergeResult)
        else pure ()

    -- Write output file
    writeResult <- writeMergedFile (outputFile config) (mergedContent mergeResult)
    case writeResult of
        Left errMsg -> dieWithError errMsg
        Right _ -> do
             let processedCount = length (processedFiles mergeResult)
             if processedCount > 0 || null files -- Success if processed files or no files initially found
                 then putStrLn $ "\nSuccessfully merged " ++ show processedCount ++ " files into " ++ outputFile config
                 else putStrLn $ "\nNo files were successfully merged into " ++ outputFile config -- Case where all found files failed to read 