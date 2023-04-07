{-# LANGUAGE QuasiQuotes #-}

-- | The module of this name differs as between Windows and non-Windows builds.
-- This is the non-Windows version.
module PathAbsExamples
  ( pathAbsDirExample
  , pathAbsFileExample
  , pathAbsFileExamples
  ) where

import           Path ( Abs, Dir, File, Path, absdir, absfile )

pathAbsDirExample :: Path Abs Dir
pathAbsDirExample = [absdir|/home/jane/my-project-dir|]

pathAbsFileExample :: Path Abs File
pathAbsFileExample = [absfile|/home/jane/my-project-dir/example.ext|]

pathAbsFileExamples :: [Path Abs File]
pathAbsFileExamples =
  [ [absfile|/home/jane/my-project-dir/example1.ext|]
  , [absfile|/home/jane/my-project-dir/example2.ext|]
  , [absfile|/home/jane/my-project-dir/example3.ext|]
  ]
