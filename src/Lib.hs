module Lib
  ( addPathToFileName
  , isMp4
  , getExtension
  , mp4FilesFilter
  , getM4a
  , getMp3
  , getTxt
  , getFnameWithoutExt
  , ffmpegPath
  , srcPath
  , dstPath
  , cmdM4a
  , cmdTom4a
  , getMp3FileWithPath
  , convertM4aToMp3Command
  , convertM4aToMp3CommandM4aFile
  , convertAll
  , getMetadataLines
  , getTitleLineFromMetaDataFile
  , parseTitleLine
  , getTitleFromMetadataFile
  , dropLastPart
  , getMp3FileName
  , dropFullPath
  , getMp3FileNameT
  , convertM4aToMp3CommandT
  ) where

import           Data.List
import           System.Directory
import           System.IO
import           System.Process

ffmpegPath = "C:/ffmpeg-20170125-2080bc3-win64-static/bin/ffmpeg.exe"

srcPath = "c:/mp4"

dstPath = "c:/mp3"

titleToName = True

convertAll = do
  files <- getDirectoryContents srcPath
  let mp4Files = mp4FilesFilter files
  let commandsToM4a = map cmdTom4a mp4Files
  mapM_ print commandsToM4a
  mapM_ system commandsToM4a
  let m4aFiles = map getM4a mp4Files
  let m4aFilesWithPath = map addSrcPath m4aFiles
  mapM_ print m4aFilesWithPath
  let txtFiles = map getTxt mp4Files
  let txtFilesWithPath = map addSrcPath txtFiles
  mapM_ print txtFilesWithPath
  if titleToName
    then do
      print "Creating metadata files..."
      let commandsToTxt = map cmdToTxt mp4Files
      mapM_ print commandsToTxt
      mapM_ system commandsToTxt
      mp3ListT <- mapM getMp3FileNameT txtFilesWithPath
      mapM_ print mp3ListT
      let cmd = map convertM4aToMp3CommandT mp3ListT
      mapM_ print cmd
--       mapM_ runCommand cmd
      mapM_ system cmd
      mapM_ removeFile txtFilesWithPath
    else do
      print "Just build mp3 files list based on filenames..."
      let mp3FilesT = map getMp3FileName m4aFilesWithPath
      mapM_ print mp3FilesT
      let cmd = map convertM4aToMp3CommandT mp3FilesT
      mapM_ print cmd
      mapM_ system cmd
  mapM_ removeFile m4aFilesWithPath

mp4FilesFilter = filter isMp4

isMp4 x = getExtension x == ".mp4"

cmdTom4a x = cmdM4a x ++ ['"'] ++ srcPath ++ "/" ++ getM4a x ++ ['"']

cmdM4a x =
  ffmpegPath ++
  " -y -i " ++ ['"'] ++ srcPath ++ "/" ++ x ++ ['"'] ++ " -vn -c:a copy "

getExtension x = getExtensionSub (length x - 1) x

getExtensionSub 0 x = x
getExtensionSub n x =
  if x !! n == '.'
    then drop n x
    else getExtensionSub (n - 1) x

dropFullPath x = dropFullPathSub (length x - 1) x

dropFullPathSub 0 x = x
dropFullPathSub n x =
  if x !! n == '/'
    then drop (n + 1) x
    else dropFullPathSub (n - 1) x

addSrcPath = addPathToFileName srcPath

addDstPath = addPathToFileName dstPath


cmdToTxt x = cmdTxt x ++ ['"'] ++ srcPath ++ "/" ++ getTxt x ++ ['"']

cmdTxt x =
  ffmpegPath ++
  " -y -i " ++ ['"'] ++ srcPath ++ "/" ++ x ++ ['"'] ++ " -f ffmetadata "

getM4a x = getFnameWithoutExt x ++ ".m4a"

getMp3 x = getFnameWithoutExt x ++ ".mp3"

getTxt x = getFnameWithoutExt x ++ ".txt"

getFnameWithoutExt = dropLastPart '.'

dropLastPart c [] = ""
dropLastPart c x =
  if last x == c
    then init x
    else dropLastPart c $ init x

getMp3FileWithPath dstPath fname = addPathToFileName dstPath $ getMp3 fname

convertM4aToMp3CommandM4aFile m4afile =
  convertM4aToMp3Command
    (addSrcPath m4afile)
    (getMp3FileWithPath dstPath m4afile)

convertM4aToMp3Command m4afile mp3file =
  ffmpegPath ++
  " -y -i " ++
  ['"'] ++
  m4afile ++ ['"'] ++ " -acodec libmp3lame " ++ ['"'] ++ mp3file ++ ['"']

convertM4aToMp3CommandT = uncurry convertM4aToMp3Command

getTitleFromMetadataFile fileName =
  parseTitleLine <$> getTitleLineFromMetaDataFile fileName

parseTitleLine [] = ""
parseTitleLine x  = tail $ dropWhile (/= '=') x

getMp3FileName mp4Files =
  (mp4Files, addDstPath $ getMp3 $ dropFullPath mp4Files)

getMp3FileNameT x = decideMp3T x <$> getTitleFromMetadataFile x

decideMp3T x ""    = (getM4a x, getMp3FileWithPath dstPath $ dropFullPath x)
decideMp3T x title = (getM4a x, addDstPath title ++ ".mp3")

getTitleLineFromMetaDataFile x =
  myhead . filter (isInfixOf "title") <$> getMetadataLines x

myhead []     = ""
myhead (x:xs) = x

getMetadataLines metadataFileName = do
  content <- readFile metadataFileName
  return $ lines content

addPathToFileName path x = path ++ "/" ++ x
