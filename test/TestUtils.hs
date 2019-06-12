module TestUtils where

import System.Directory (removeFile)
import System.IO (openFile, IOMode(..))

import Test.Tasty.HUnit (Assertion)

-- | Test that an IO action prints out exactly the set of lines
-- we expect it to. The action must take a handle. We'll pass a
-- handle to a temporary file and then delete it.
testIOPrint :: (Handle -> IO ()) -> [String] -> Assertion
testIOPrint action expectedOutput = do
  let testFile = "temp_test_file"
  writeHandle <- openFile testFile WriteMode
  hClose writeHandle
  readHandle <- openFile testFile ReadMode
  output <- lines <$> hGetContents readHandle
  hClose readHandle
  removeFile testFile
  output @?= expectedOutput
