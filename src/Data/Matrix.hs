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

module Data.Matrix
    ( Matrix
    , (><)
    , rows, cols, shape
    , trans
    , fromLists, toLists
    )
    where

import Data.Matrix.Internal

-- vim: set fileencoding=utf8 :
