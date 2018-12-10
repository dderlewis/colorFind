import Prelude as P
import Graphics.Image as I
import Text.Bytedump
import Graphics.Image.Interface

main :: IO (String)
main = do
  putStrLn "Please enter the file path to the image you would like to use: "
  line <- getLine
  color <- fileInput line
  return ("The average color of your image is: " ++ (color))

  
fileInput :: FilePath -> IO [Char]
fileInput f = do
  cluster <- readImageRGB VU f    --imports given image in RGB format
  --displayImage cluster
  let img = toLists (toWord8I cluster)  --converts image to list of pixels
  let final =  P.map (P.map toPixelsX) $ img --converts list to desired pixel type
  let final2 = P.map(P.map toComponents) $ (final!!0)  -- breaks pixels down into individual RGB components
  let final3 = rgb2Color final2  -- converts RGB format to 6-digit hex-color-code
  let final4 = concat final3     -- creates a single list of the color codes versus a list for each row
  let final5 = mostCommon' final3 -- determines the most common color code present in the list
  let newImagePix = replicate 50 final5
  
  return final5

rgb2Color :: [[Word8]] -> [[Char]]
rgb2Color r = P.map colorConvert $ r

colorConvert :: [Word8] -> [Char]
colorConvert c = first ++ second ++ third  -- converts the three individual RGB components into a single hex-code
  where
    first =  hexString (c!!0)
    second = hexString (c!!1)
    third = hexString (c!!2)
  
count :: Ord a => a -> [a] -> Int
count el = length . filter (== el)

specialCompare' :: Ord a => a -> a -> [a] -> Bool
specialCompare' el1 el2 lst           -- function compares the count of two elements present in the list and 
        | cnt1 > cnt2  = True         --returns True if the first is more common
        | cnt1 < cnt2  = False
        | cnt1 == cnt2 = el1 > el2
         where cnt1 = count el1 lst
               cnt2 = count el2 lst
    
mostCommon' :: Ord a => [[a]] -> [a]
mostCommon' list = foldr1 (\x y -> if specialCompare' x y list then x else y) list  --will return the most common element of the input list


