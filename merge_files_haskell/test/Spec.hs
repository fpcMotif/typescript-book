module Main (main) where

import MergeFiles ( MergedFileResult(..), formatFileContent )
import Test.HUnit
import qualified Data.Text as T

-- Test case for formatting a single file's content
testFormatFileContent :: Test
testFormatFileContent = TestCase $ do
    let filename = T.pack "example.txt"
    let content = T.pack "  Line 1\nLine 2  "
    let expected = T.unlines
            [ ""
            , "<!-- ========== Start: example.txt ========== -->"
            , ""
            , "Line 1\nLine 2" -- Content should be trimmed
            , ""
            , "<!-- ========== End: example.txt ========== -->"
            , ""
            ]
    assertEqual "for formatFileContent," expected (formatFileContent filename content)

-- Test case for empty content
testFormatFileContentEmpty :: Test
testFormatFileContentEmpty = TestCase $ do
    let filename = T.pack "empty.md"
    let content = T.pack "  \n  \t  "
    let expected = T.unlines
            [ ""
            , "<!-- ========== Start: empty.md ========== -->"
            , ""
            , "" -- Empty content after trimming
            , ""
            , "<!-- ========== End: empty.md ========== -->"
            , ""
            ]
    assertEqual "for formatFileContent with empty/whitespace content," expected (formatFileContent filename content)

-- Combine tests into a suite
tests :: Test
tests = TestList
    [ TestLabel "testFormatFileContent" testFormatFileContent
    , TestLabel "testFormatFileContentEmpty" testFormatFileContentEmpty
    -- Add more tests here for findMergeableFiles (mocking fs?), mergeFilesContent (mocking fs?)
    ]

main :: IO ()
main = do
    counts <- runTestTT tests
    if errors counts + failures counts == 0
        then putStrLn "Tests passed!"
        else putStrLn "Tests failed!"
    -- Consider exiting with non-zero code on failure for CI
    -- exitWith $ if errors counts + failures counts == 0 then ExitSuccess else ExitFailure 1 