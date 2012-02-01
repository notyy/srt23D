import System.IO
import System.Directory
import System.FilePath
import Data.List(groupBy,intercalate)

main = do
        files <- getDirectoryContents "."
        let srtFiles = filter is2dsrtFile files
        mapM_ to3D srtFiles
        putStrLn "done"

is2dsrtFile :: String -> Bool
is2dsrtFile file = let (filename,ext) = splitExtension file
                       fileSuffix = (reverse . take 3) (reverse filename)
                   in ext == ".srt" && (fileSuffix /= "-3D")

to3D :: FilePath -> IO ()       
to3D file = do
        contents <- readFile file
        let (filename,ext) = splitExtension file
        writeFile (filename ++ "-3D" ++ ext) (unlines $ offsetAll $ lines contents)


offsetAll :: [String] -> [String]
offsetAll xs = intercalate [""] $ map unformat $ reorgnize xs

reorgnize :: [String] -> [(String,String,String)]
reorgnize xs = let xs' = extractBlock xs
                   mIndex = length xs'
               in  (map (fst . offset1 mIndex) xs') ++ (map (snd . offset1 mIndex) xs')
                   

offset1 :: Int -> (String, String, String) -> ((String,String,String),(String,String,String))
offset1 mIndex (index,time,content) = let rIndex = show $ (read index :: Int) + mIndex
                                          left = (index, time, "{\\pos(96,255)\\fscx50}" ++ content)
                                          right = (rIndex, time, "{\\pos(288,255)\\fscx50}" ++ content)
                                      in (left, right)

extractBlock :: [String] -> [(String,String,String)]
extractBlock xs = let xs' = groupByEmptyLine xs 
                  in map format xs'
                  where groupByEmptyLine = (filter (/=[""])) . (groupBy (\x y -> if (x/= "" && y /= "") then True else False))

format :: [String] -> (String,String,String)
format (index:time:contents) = (index,time,unlines contents)

unformat :: (String,String,String) -> [String]
unformat (index,time,contents) = index:time:lines contents

tripleFst :: (a, b, c) -> a
tripleFst (x, y, z) = x 

-- test -- 
l1 = ["1", "13:00 --> 14:00", "content A","content B", "", "2", "14:00 -> 15:00", "content C"]


