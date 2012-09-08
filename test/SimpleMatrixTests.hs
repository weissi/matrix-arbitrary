{-# OPTIONS_GHC -F -pgmF htfpp #-}

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
-}

-- vim: set fileencoding=utf8 :

-- # STDLIB
import Data.List (transpose)

-- # LOCAL
import Data.Matrix

-- # HTF
import Test.Framework hiding ((><))

arbitraryShapeMatrix :: (Enum e, Num e)
                     => (Int -> Int -> Matrix e -> Bool) -> Property
arbitraryShapeMatrix f =
    forAll (choose (1, 100)) $ \r -> forAll (choose (1,100)) $ \c ->
        let m = (r >< c) [1..]
         in f r c m

prop_matrixEqualsTransTransMatrix :: Property
prop_matrixEqualsTransTransMatrix =
    arbitraryShapeMatrix $ \r c m ->
        m == (trans . trans) m

prop_matrixShape :: Property
prop_matrixShape =
    arbitraryShapeMatrix $ \r c m ->
         (r, c) == shape m

prop_matrixRows :: Property
prop_matrixRows =
    arbitraryShapeMatrix $ \r c m ->
         r == rows m

prop_matrixCols :: Property
prop_matrixCols =
    arbitraryShapeMatrix $ \r c m ->
         c == cols m

prop_matrixListsConversion :: Property
prop_matrixListsConversion =
    arbitraryShapeMatrix $ \r c m ->
         m == (fromLists . toLists) m

prop_matrixTranspose :: Property
prop_matrixTranspose =
    arbitraryShapeMatrix $ \r c m ->
        trans m == (fromLists . transpose . toLists) m

main = htfMain htf_thisModulesTests
