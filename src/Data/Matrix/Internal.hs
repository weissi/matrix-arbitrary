{-
 # This file is part of matrix-arbitrary.                                      #
 #                                                                             #
 # matrix-arbitrary is free software: you can redistribute it and/or modify    #
 # it under the terms of the GNU General Public License as published by        #
 # the Free Software Foundation, either version 3 of the License, or           #
 # (at your option) any later version.                                         #
 #                                                                             #
 # matrix-arbitrary is distributed in the hope that it will be useful,         #
 # but WITHOUT ANY WARRANTY; without even the implied warranty of              #
 # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               #
 # GNU General Public License for more details.                                #
 #                                                                             #
 # A copy of the GNU General Public License resides in the `LICENSE`           #
 # file distributed along with matrix-arbitrary.                               #
 #                                                                             #
 # Copyright 2012, Johannes Weiß <weiss@tux4u.de>                              #
 #                                                                             #
 # Thanks to Alberto Ruiz for the `instance Show Matrix` code, shamelessly     #
 #   stolen from hmatrix                                                       #
-}

module Data.Matrix.Internal
    ( Matrix(..)
    , (><)
    , rows, cols, shape
    , trans
    , fromLists, toLists
    )
    where

import Data.Array.IArray (Array)
import Data.List (transpose, intersperse)
import Data.Tuple (swap)
import qualified Data.Array.IArray as A

newtype Matrix e = Matrix (Array (Int, Int) e)

(><) :: Int -> Int -> [a] -> Matrix a
(><) r c = Matrix . A.listArray ((1,1), (r,c))

shape :: Matrix a -> (Int, Int)
shape (Matrix m) = (snd . A.bounds) m

cols :: Matrix a -> Int
cols = snd . shape

rows :: Matrix a -> Int
rows = fst . shape

trans :: Matrix a -> Matrix a
trans m@(Matrix arr) = Matrix $ A.ixmap ((1,1), (swap . shape) m) swap arr

toLists :: Matrix a -> [[a]]
toLists m@(Matrix arr) =
    let doIt :: Int -> [a] -> Int -> [[a]] -> [[a]]
        doIt c es' r acc =
            if r > 0
               then let (mes, es'') = splitAt c es'
                     in doIt c es'' (r-1) (acc++[mes])
               else acc
        (mRows, mCols) = shape m
     in doIt mCols (A.elems arr) mRows []

fromLists :: [[a]] -> Matrix a
fromLists es =
    let mRows :: Int
        mRows = length es
        mCols' :: [Int]
        mCols' = map length es
        mCols :: Int
        mCols =
            let minCols = minimum mCols'
                maxCols = minimum mCols'
             in if minCols == maxCols
                   then minCols
                   else error "fromLists: applied to lists of different sizes"
     in (mRows >< mCols) $ concat es

instance (Show e) => (Show (Matrix e)) where
    show m = (sizes++) . dsp . map (map show) . toLists $ m
        where sizes = "("++show (rows m)++"><"++show (cols m)++")\n"
              dsp as =
                  let mt = transpose as
                      longs = map (maximum . map length) mt
                      mtp = zipWith (\a b -> map (pad a) b) longs mt
                      pad n str = replicate (n - length str) ' ' ++ str
                      unwords' = concat . intersperse ", "
                  in (++" ]") . (" ["++) . init . drop 2 . unlines .
                         map (" , "++) . map unwords' $ transpose mtp

instance (Eq e) => Eq (Matrix e) where
    (==) (Matrix l) (Matrix r) = l == r
-- vim: set fileencoding=utf8 :
