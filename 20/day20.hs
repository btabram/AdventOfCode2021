import Data.List (foldl')
import Data.Map ((!))
import qualified Data.Map as Map
import Numeric (readBin)


type Coord = (Int, Int)
type PixelMap = Map.Map Coord Char
type ImageEnhancementAlgorithm = Map.Map Int Char

type Image = (PixelMap, -- Map of pixel coordinate -> pixel value. Assumed to always be representing a square image
              Char, -- The 'default pixel' value which the infinite number of pixels not in |PixelMap| are set to
              Int, -- The minimum coordinate value in |PixelMap|
              Int) -- The maximum coordinate value in |PixelMap|


pixelToDigit :: Char -> Char
pixelToDigit '.' = '0'
pixelToDigit '#' = '1'

pixelsToInt :: [Char] -> Int -- Convert the pixels to 0/1 and then parse as binary
pixelsToInt = fst . head . readBin . map pixelToDigit


main = do
    imageEnhancementAlgorithmStr : "" : inputImage <- fmap lines $ readFile "input.txt"
    let initialImageSize = length (inputImage !! 0)
    let initialImage = ( (charGridToPixelMap inputImage), '.', 0, initialImageSize - 1 )
    let iea = parseImageEnhancementAlgorithm imageEnhancementAlgorithmStr
    let enhancedImages = iterate (enhance iea) initialImage
    putStrLn $ "The answer to Part 1 is " ++ (show $ countLitPixels $ enhancedImages !! 2)
    putStrLn $ "The answer to Part 2 is " ++ (show $ countLitPixels $ enhancedImages !! 50)


-- The absolute coordinates don't matter so just choose the top left corner of the input to be our origin.
charGridToPixelMap :: [[Char]] -> PixelMap
charGridToPixelMap = snd . foldl' (\(j, pixelMap) row -> (j + 1, insertRow pixelMap j row)) (0, Map.empty)
    where insertRow :: PixelMap -> Int -> [Char] -> PixelMap
          insertRow pixelMap j = snd . foldl' (\(i, pixelMap) c -> (i + 1, Map.insert (i, j) c pixelMap)) (0, pixelMap)

parseImageEnhancementAlgorithm :: [Char] -> ImageEnhancementAlgorithm
parseImageEnhancementAlgorithm = snd . foldl' (\(i, iea) c -> (i + 1, Map.insert i c iea)) (0, Map.empty)


getNeighbours :: Coord -> [Coord]
getNeighbours (x, y) = [(x_n, y_n) | y_n <- [y - 1 .. y + 1], x_n <- [x - 1 .. x + 1]]

getKernelPixels :: Image -> Coord -> [Char]
getKernelPixels (pixelMap, defaultPixel, _, _) = foldr (\neighbour acc -> (getPixel neighbour) : acc) "" . getNeighbours
    where getPixel :: Coord -> Char
          getPixel key = case Map.lookup key pixelMap of Just pixel -> pixel
                                                         Nothing -> defaultPixel

getEnhancedPixel :: ImageEnhancementAlgorithm -> Image -> Coord -> Char
getEnhancedPixel iea image coord = iea ! (pixelsToInt $ getKernelPixels image coord)

enhanceImage :: ImageEnhancementAlgorithm -> Image -> Image
enhanceImage iea image@(pixelMap, a, b, c)
    = ( (Map.mapWithKey (\key _ -> getEnhancedPixel iea image key) pixelMap), a, b, c )

-- Every enhancement grows the non-default portion of the image so we need to track more pixels.
expandImage :: Image -> Image
expandImage (initialPixelMap, defaultPixel, min, max)
    = let newRows = [(x, y) | y <- [min - 1, max + 1], x <- [min - 1 .. max + 1]]
          newCols = [(x, y) | y <- [min .. max], x <- [min - 1, max + 1]]
          newPixels = newRows ++ newCols
          newPixelMap = foldl' (\pm coord -> Map.insert coord defaultPixel pm) initialPixelMap newPixels
      in (newPixelMap, defaultPixel, min - 1, max + 1)

-- Update our value for the infinite number of 'default pixels' we aren't actively tracking. For example if the 'default
-- pixel' is currently "#" then the default kernel pixels will be "#########". This maps to 111111111 in binary or 511.
updateDefaultPixel :: ImageEnhancementAlgorithm -> Image -> Image
updateDefaultPixel iea (a, '.', b, c) = (a, (iea ! 0), b, c)
updateDefaultPixel iea (a, '#', b, c) = (a, (iea ! 511), b, c)

enhance :: ImageEnhancementAlgorithm -> Image -> Image
enhance iea = (updateDefaultPixel iea) . (enhanceImage iea) . expandImage


countLitPixels :: Image -> Int
countLitPixels (pixelMap, _, _, _) = foldl' (\acc pixel -> if pixel == '#' then acc + 1 else acc) 0 pixelMap