import qualified Graphics.Image as I
import Graphics.Image.IO
import Data.List (transpose,tails)
import Control.Monad (when)
import System.Environment (getArgs)
import Debug.Trace (trace)

import Picture (Picture(..), Pixel(..), addDown, addUp, addRight, addLeft, edge, 
                cellShade, grayScale, blur, gaussian, pixelate, blackBox,
                padTop, padLeft, padRight, padBottom,
                rot90, rot180, rot270, flip1, flip2, flipH, flipV, padH, padV)


-----------------------------------------------------------------------------
--
-- Code for reading and writing images
--
-----------------------------------------------------------------------------

test :: IO ()
test = do writePicture "test.jpg" (blackBox 50 100)
          sonic <- readPicture "sonic.jpeg"
          writePicture "sonic_top.jpg" (padTop 100 sonic)
          writePicture "sonic_left.jpg" (padLeft 100 sonic)
          writePicture "sonic_right.jpg" (padRight 100 sonic)
          writePicture "sonic_bottom.jpg" (padBottom 100 sonic)
          writePicture "sonic_H.jpg" (padH 100 sonic)
          writePicture "sonic_V.jpg" (padV 100 sonic)
          writePicture "sonic_cell.jpg" (cellShade sonic)
          writePicture "sonic_gray.jpg" (grayScale sonic)


readPicture :: String -> IO Picture
readPicture file = do image <- I.readImageRGB I.VU file
                      return [[ fromPixel (I.index image (r,c))
                              | c <- [0..I.cols image-1]]
                              | r <- [0..I.rows image-1]]

writePicture :: String -> Picture -> IO ()
writePicture file = I.writeImage file
                  . I.fromListsR I.VU
                  . (map . map) toPixel

fromPixel :: I.Pixel I.RGB Double -> Pixel
fromPixel (I.PixelRGB r g b) = Pixel r g b

toPixel :: Pixel -> I.Pixel I.RGB Double
toPixel (Pixel r g b) = I.PixelRGB r g b

main :: IO ()
main = do args <- getArgs
          when (length args < 2) $ error ("Must have at least 2 arguments" ++ showHelp)
          let [input,output] = take 2 args
          image <- readPicture input
          out <- getCmd (drop 2 args) image
          writePicture output out

add :: (Picture -> Picture -> Picture) -> String -> [String] -> Picture -> IO Picture
add dir p2n xs p1 = readPicture p2n >>= getCmd xs . dir p1

getCmd :: [String] -> Picture -> IO Picture
getCmd []                    = return
getCmd ("-add":"down":i:xs)  = add addDown  i xs
getCmd ("-add":"up":i:xs)    = add addUp    i xs
getCmd ("-add":"right":i:xs) = add addRight i xs
getCmd ("-add":"left":i:xs)  = add addLeft  i xs
getCmd ("-edge":xs)          = getCmd xs . edge
getCmd ("-cell":xs)          = getCmd xs . cellShade
getCmd ("-gray":xs)          = getCmd xs . grayScale
getCmd ("-blur":xs)          = getCmd xs . blur
getCmd ("-gauss":xs)         = getCmd xs . gaussian
getCmd ("-pixelate":n:xs)    = getCmd xs . pixelate (read n)
getCmd ("-r90":xs)           = getCmd xs . rot90
getCmd ("-r180":xs)          = getCmd xs . rot180
getCmd ("-r270":xs)          = getCmd xs . rot270
getCmd ("-flip1":xs)         = getCmd xs . flip1
getCmd ("-flip2":xs)         = getCmd xs . flip2
getCmd ("-flipH":xs)         = getCmd xs . flipH
getCmd ("-flipV":xs)         = getCmd xs . flipV
getCmd ("-padH":n:xs)        = getCmd xs . padH (read n)
getCmd ("-padV":n:xs)        = getCmd xs . padV (read n)
getCmd x = error $ "invalid command: " ++ unwords x ++ "\n" ++ showHelp

showHelp :: String
showHelp = unlines $
           ["Usage:",
            "instaLam input output args",
            "-add down image  : read image and add it below the current picture",
            "-add up image    : read image and add it above the current picture",
            "-add right image : read image and add it to the left of the current picture",
            "-add left image  : read image and add it to the right of the current picture",
            "-edge            : find the edges",
            "-cell            : cell shade the image",
            "-gray            : make the image grayscale",
            "-blur            : blur the image using a flat blurring",
            "-gauss           : blur the image using Gaussian blurring",
            "-pixelate n      : pixelate the image with pixels of size nXn",
            "-r90             : rotate the image 90 degrees to the right",
            "-r180            : rotate the image 180 degrees to the right",
            "-r270            : rotate the image 270 degrees to the right",
            "-flip1           : flip the image over the first diagonal",
            "-flip2           : flip the image over the second diagonal",
            "-flipH           : flip the image horizontally (mirror the image)",
            "-flipV           : flip the image vertically",
            "-padH n          : pad the left and right of the image with n black pixels.",
            "-padV n          : pad the top and bottom of the image with n black pixels."]
