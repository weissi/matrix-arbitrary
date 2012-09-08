{-# LANGUAGE ScopedTypeVariables #-}

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
 # Thanks to Matthew Donadio for the shamelessly stolen code of                #
 #   - lu                                                                      #
 #   - lu_solve                                                                #
 #   - inverse                                                                 #
 #   from his dsp package (http://hackage.haskell.org/package/dsp-0.2.1)       #
-}

module Numeric.MatrixOperations
    ((|+|), (|*|), inverse
    )
    where

-- # STDLIB
import Data.Tuple (swap)
import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A

-- # LOCAL
import Data.Matrix (shape, (><), fromLists, toLists, trans)
import Data.Matrix.Internal (Matrix(Matrix))

(|+|) :: Num e => Matrix e -> Matrix e -> Matrix e
(|+|) ml@(Matrix arrl) (Matrix arrr) =
    if A.bounds arrl /= A.bounds arrr
       then error "|+| matrix addition of matrices of different shapes"
       else (uncurry (><) (shape ml) $
                zipWith (+) (A.elems arrl) (A.elems arrr))

(|*|) :: Num e => Matrix e -> Matrix e -> Matrix e
(|*|) l r =
    let rT = trans r
     in if (snd . shape) l == (fst . shape) r
           then fromLists $
                    [map (sum . zipWith (*) r) (toLists rT) | r <- (toLists l)]
           else error "|*| matrix multiplication shape error"

lu :: (Num e, Fractional e)
   => Array (Int,Int) e -- ^ A
   -> Array (Int,Int) e -- ^ LU(A)
lu a = a'
    where a' = A.array bnds [ ((i,j), luij i j) | (i,j) <- A.range bnds ]
          luij i j =
             if i>j
               then (a A.! (i,j) - sum [ a' A.! (i,k) * a' A.! (k,j)
                                       | k <- [1 ..(j-1)]
                                       ]
                    ) / a' A.! (j,j)
               else  a A.! (i,j) - sum [ a' A.! (i,k) * a' A.! (k,j)
                                       | k <- [1 ..(i-1)]
                                       ]
          bnds = A.bounds a

lu_solve :: forall e. (Num e, Fractional e)
         => Array (Int,Int) e -- ^ LU(A)
         -> Array Int e -- ^ b
         -> Array Int e -- ^ x
lu_solve a b = x
    where x = A.array (1,n) ([(n,xn)] ++ [ (i, backward i)
                                         | i <- (reverse [1..(n-1)])
                                         ]
                            )
          y :: Array Int e
          y = A.array (1,n) ([(1,y1)] ++ [ (i, forward i)  | i <- [2..n] ])
          y1         = b A.! 1
          forward  i = (b A.! i - sum [ a A.! (i,j) * y A.! j
                                      | j <- [1..(i-1)]
                                      ]
                       )
          xn         = y A.! n / a A.! (n,n)
          backward i = (y A.! i - sum [ a A.! (i,j) * x A.! j
                                      | j <- [(i+1)..n]
                                      ]
                       ) / a A.! (i,i)
          ((_,_),(n,_)) = A.bounds a

inverse :: (Num e, Fractional e) => Matrix e -> Matrix e
inverse (Matrix a0) = Matrix $ a'
    where a' = A.array (A.bounds a0) (arrange (makecols (lu a0)) 1)
          makecol i n' =
              A.array (1,n')
                [ (j, if i == j then (fromInteger 1) else (fromInteger 0))
                | j <- [1..n']
                ]
          makecols a = [ lu_solve a (makecol i n) | i <- [1..n] ]
          ((_,_),(n,_)) = A.bounds a0
          arrange []     _ = []
          arrange (m:ms) j = flatten m j ++ arrange ms (j+1)
          flatten m j = map (\(i,x) -> ((i,j),x)) (A.assocs m)

-- vim: set fileencoding=utf8 :
