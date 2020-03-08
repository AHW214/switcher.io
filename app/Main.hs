module Main where


--------------------------------------------------------------------------------
import           Control.Monad         (filterM, foldM, forM_, unless, when)
import           Data.Char             (toLower)
import           System.Directory      (doesFileExist, getCurrentDirectory,
                                        listDirectory, renameFile)
import           System.Environment    (getArgs)
import           System.Exit           (exitFailure, exitSuccess)
import           System.FilePath       (FilePath, isExtensionOf)


--------------------------------------------------------------------------------
import           Option                (getExtension, parseOptions)
import           Random                (randomRSequence, shuffleList)


--------------------------------------------------------------------------------
partitionM :: (a -> IO Bool) -> [ a ] -> IO ( [ a ], [ a ] )
partitionM predicate =
  foldM update ( [], [] )
  where
    update ( pass, fail ) x = do
      res <- predicate x
      return $
        if res then
          ( x:pass, fail )
        else
          ( pass, x:fail )


--------------------------------------------------------------------------------
findFilesWhere :: (FilePath -> IO Bool) -> FilePath -> IO [ FilePath ]
findFilesWhere predicate dir = do
  items <- listDirectory dir
  ( files, dirs ) <- partitionM doesFileExist items

  filterM predicate files


--------------------------------------------------------------------------------
findFilesWithExt :: String -> FilePath -> IO [ FilePath ]
findFilesWithExt ext =
  findFilesWhere (return . isExtensionOf ext)


--------------------------------------------------------------------------------
switchFileNames :: [ FilePath ] -> IO ()
switchFileNames files =
   zip files
   <$> shuffleList files
   >>= mapM_ switcheroo
    where
      switcheroo ( file1, file2 ) = do
        temp <- makeTempName 10

        renameFile file1 temp
        renameFile file2 file1
        renameFile temp file2

      makeTempName =
        randomRSequence ( 'A', 'z' )


--------------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  opts <- parseOptions args
  ext <- getExtension opts

  dir <- getCurrentDirectory
  files <- findFilesWithExt ext dir

  let len = length files

  when (len <= 0) $ do
    putStrLn $ "No files with extension '" ++ ext ++ "' found"
    exitSuccess

  putStrLn $ "Found " ++ show len ++ " files with extension '" ++ ext ++ "'\n"
            ++ "Type 'y(es)' to confirm the switcheroo"

  response <- map toLower <$> getLine

  unless (response `elem` [ "y", "yes" ]) $ do
    putStrLn "Aborting..."
    exitSuccess

  putStrLn "Swapping..."
  switchFileNames files
  putStrLn "Done!"
