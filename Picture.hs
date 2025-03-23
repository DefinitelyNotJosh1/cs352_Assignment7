module Picture where

import Data.List (transpose,tails)
import Control.Applicative (Alternative(empty))

-- Data Structures for representing an Picture
-- A Pixel is just a triple of doubles between 0 and 1
-- let x = Pixel r g b
-- is a pixel with a red greed and blue component.
-- A Picture is a 2D list of pixels.

data Pixel = Pixel Double Double Double
 deriving Show
type Picture = [[Pixel]]

-- A large pixel (used for pixelate)
type BigPixel = [[Pixel]]

-- A single black pixel
black :: Pixel
black = Pixel 0 0 0

-----------------------------------------------------------------------------
--
-- Assignment 4: A basic image library
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--
-- Part 1: useful functions
--
-----------------------------------------------------------------------------

-- A function to scale a pixel by a given number
pixelScale :: Double -> Pixel -> Pixel
pixelScale scalar (Pixel r g b) = Pixel (scalar * r) (scalar * g) (scalar * b)

-- Add 2 pixels together componentwise
pixelAdd :: Pixel -> Pixel -> Pixel
pixelAdd (Pixel r1 g1 b1) (Pixel r2 g2 b2) = Pixel (r1 + r2) (g1 + g2) (b1 + b2)

-- get the red component of a pixel
red :: Pixel -> Double
red (Pixel r _ _) = r

-- get the green component of a pixel
green :: Pixel -> Double
green (Pixel _ g _) = g

-- get the blue component of a pixel
blue :: Pixel -> Double
blue (Pixel _ _ b) = b

-- A function that takes a pixel transformation
-- and applies it to all of the pixels in the image
picMap :: (a -> a) -> [[a]] -> [[a]]
picMap f = map (map f)

-- group a list into groups of size n.
-- example group 2 [1,2,3,4,5,6]
-- [[1,2],[3,4],[5,6]
-- if n is not positive, it should throw an error
group :: Int -> [a] -> [[a]]
-- base case: if the list is empty, return an empty list
group _ [] = []
-- recursive case: take the first n elements of the list and group them, then recurse on the rest of the list
group n xs
    | n > 0 = take n xs : group n (drop n xs)
    | otherwise = error "n should be greater than 0"

-- returns the height of an image
height :: [[a]] -> Int
height xs = length xs

-- returns the width of an image
-- If an image has no rows, then it should have a width of 0
width :: [[a]] -> Int
width [] = 0
width xs = length (head xs)

-- creates an NxM matrix of black pixels
-- N rows
-- M columns
blackBox :: Int -> Int -> [[Pixel]]
blackBox n m = replicate n (replicate m black)

-- adds n rows of black pixels to the top of the image
padTop :: Int -> Picture -> Picture
padTop n xs = replicate n (replicate (width xs) black) ++ xs

-- adds n rows of black pixels to the bottom of the image
padBottom :: Int -> Picture -> Picture
padBottom n xs = xs ++replicate n (replicate (width xs) black)

-- adds n rows of black pixels to the left of the image
padLeft :: Int -> Picture -> Picture
padLeft n xs = map (replicate n black ++) xs

-- adds n rows of black pixels to the right of the image
padRight :: Int -> Picture -> Picture
padRight n xs = map (++ replicate n black) xs

-- pad an immage to the left and the right with n columns of black pixels.
padH :: Int -> Picture -> Picture
padH n xs = padLeft n (padRight n xs)

-- pad an immage above and below with n rows of black pixels
padV :: Int -> Picture -> Picture
padV n xs = padTop n (padBottom n xs)

-- cell shades an image
cellShade :: Picture -> Picture
cellShade = picMap cellShadePixel
  where
    cellShadePixel :: Pixel -> Pixel
    cellShadePixel (Pixel r g b) = Pixel (roundToThird r) (roundToThird g) (roundToThird b)
    
    -- Helper function to round a value to the nearest third (0, 1/3, 2/3, or 1)
    roundToThird :: Double -> Double
    roundToThird x
      | x < 1/6   = 0.0    -- Round to 0
      | x < 1/2   = 1/3    -- Round to 1/3
      | x < 5/6   = 2/3    -- Round to 2/3
      | otherwise = 1.0    -- Round to 1

-- converts an image to gray scale.
grayScale :: Picture -> Picture
grayScale = picMap grayScalePixel
  where
    grayScalePixel :: Pixel -> Pixel
    grayScalePixel (Pixel r g b) = 
      let luminance = 0.299 * r + 0.587 * g + 0.114 * b
      in Pixel luminance luminance luminance


--------------------------------------------------------------------------
--
-- Assignment 8
--
--------------------------------------------------------------------------

-- compute the average color of a list of pixels
average :: [Pixel] -> Pixel
average pixels = Pixel avgRed avgGreen avgBlue
  where 
    -- convert int to integral
    len = fromIntegral $ length pixels
    avgRed = sum (map red pixels) / len
    avgGreen = sum (map green pixels) / len
    avgBlue = sum (map blue pixels) / len


-- put p2 below p1.
-- If the two pictures are not the same width,
-- you will need to add black space to the right of the smaller picture
addDown :: Picture -> Picture -> Picture
addDown p1 p2 = p1 ++ padRight (width p1 - width p2) p2

-- put p2 above p1.
-- If the two pictures are not the same width,
-- you will need to add black space to the right of the smaller picture
addUp :: Picture -> Picture -> Picture
addUp p1 p2 = padRight (width p1 - width p2) p2 ++ p1

-- put p2 to the right of p1.
-- If the two pictures are not the same height,
-- you will need to add black space below the smaller picture
addRight :: Picture -> Picture -> Picture
addRight p1 p2 
   | height p1 > height p2 = zipWith (++) p1 (padBottom (height p1 - height p2) p2)
   | height p1 < height p2 = zipWith (++) (padBottom (height p2 - height p1) p1) p2
   | otherwise = zipWith (++) p1 p2

-- put p2 to the left of p1.
-- If the two pictures are not the same height,
-- you will need to add black space below the smaller picture
addLeft :: Picture -> Picture -> Picture
addLeft p1 p2 = addRight p2 p1 -- I am a genius

-- these two functions (and transpose from the List library)
-- are all you need to complete the flip and rotate functions.
-- Every other function can be done with a composition of these three functions.
-- To figure out how, get a paper square and draw A B C D on the corners
--
-- -------------------
-- |A               B|
-- |                 |
-- |                 |
-- |                 |
-- |                 |
-- |                 |
-- |D               C|
-- -------------------
--
-- Now, flip the square around using horizontal, virtical, or transpose flips,
-- you can recreate any of the orther translations!

-- flip the image horizontally (mirror the image)
flipH :: [[a]] -> [[a]]
flipH = map reverse

-- flip the image vertically
flipV :: [[a]] -> [[a]]
flipV = reverse

-- rotate an image 90 degrees
rot90 :: [[a]] -> [[a]]
rot90 = transpose . reverse

-- rotate the image 180 degrees
rot180 :: [[a]] -> [[a]]
rot180 = flipH . flipV

-- rotate the image 270 degrees
rot270 :: [[a]] -> [[a]]
rot270 = rot180 . rot90

-- flip the image over the first diagonal
-- this is the diagonal from A to C
flip1 :: [[a]] -> [[a]]
flip1 = transpose

-- flip the image over the second diagonal
-- this is the diagonal from B to D
flip2 :: [[a]] -> [[a]]
flip2 = flipV . rot90

pixelate :: Int -> Picture -> Picture
pixelate n xs = unmakePixels $ picMap (sameColor n) (makePixels n xs)

-- really this is Int -> Picture -> [[BigPixel]]
makePixels :: Int -> [[a]] -> [[[[a]]]]
makePixels n xs = map transpose $ group n (map (group n) xs) 

-- really this is [[BigPixel]] -> Picture
unmakePixels :: [[[[a]]]] -> [[a]]
unmakePixels = map concat . concat . map transpose

sameColor :: Int -> BigPixel -> BigPixel
sameColor n x = replicate n (replicate n (average (concat x)))
--------------------------s-------------------------------------------------
--
-- Extra Credit: Convolution
--
---------------------------------------------------------------------------


matrixZipWith :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
matrixZipWith = undefined

matrixFold :: (a -> a -> a) -> a -> [[a]] -> a
matrixFold = undefined

convolve :: (b -> b -> b) -> (a -> b -> b) -> b -> [[a]] -> [[b]] -> b
convolve = undefined

-- takes a kernel and a pixel of the same size (a square matrix)
-- and multiplies each element pointwise.
-- We then add all of the pixels together to get a single pixel.
convolvePic :: [[Double]] -> [[Pixel]] -> Pixel
convolvePic = undefined

-- get a list of sliding windows from a list
-- example: 
-- windows 3 [1,2,3,4,5,6]
-- gives us
-- [[1,2,3],[2,3,4],[3,4,5],[4,5,6]]
windows :: Int -> [a] -> [[a]]
windows = undefined

convolveImage :: [[Double]] -> [[Pixel]] -> [[Pixel]]
convolveImage = undefined

-- edge detections
-- The edged tend to come out pretty faint, so I scale up everything in the
-- image by a factor of 4.
edge :: Picture -> Picture
edge = picMap (pixelScale 4) . convolveImage kernal . grayScale
 where kernal = [[ 0,-1, 0],
                 [-1, 4,-1],
                 [ 0,-1, 0]]

-- blur the image using a flat blurring.
-- This tends to blur a little more, but it doesn't look as good as gausing bluring.
blur :: Picture -> Picture
blur = convolveImage kernal
 where kernal = [[1/25,1/25,1/25,1/25,1/25],
                 [1/25,1/25,1/25,1/25,1/25],
                 [1/25,1/25,1/25,1/25,1/25],
                 [1/25,1/25,1/25,1/25,1/25],
                 [1/25,1/25,1/25,1/25,1/25]]

-- gaussian bluring
-- blur each pixel with a weighted average of all the pixels around it.
-- The weights come from a gaussian distributaion
-- (technically a binomial distribution since pictures are discrete)
gaussian :: Picture -> Picture
gaussian = convolveImage kernal
 where kernal = [[1/273, 4/273, 7/273, 4/273,1/273],
                 [4/273,16/273,27/273,16/273,4/273],
                 [7/273,26/273,41/273,26/273,7/273],
                 [4/273,16/273,27/273,16/273,4/273],
                 [1/273, 4/273, 7/273, 4/273,1/273]]

