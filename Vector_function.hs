-- Vector Functions


type Euclid2D = (Double,Double)

{- -----------------------------------------------------------------
 - getX,getY
 - -----------------------------------------------------------------
 - Description: By inputing a Euclid2D parameter, getX and getY would return the x and y coordinates from Euclid2D, respectively.
 -}

getX :: Euclid2D -> Double
getX e = fst e

getY :: Euclid2D -> Double
getY e = snd e

{- -----------------------------------------------------------------
 - scalarMult
 - -----------------------------------------------------------------
 - Description: scalarMult takes a double (i.e any constant) and a Euclid2D parameter and multiplies the constant with the x coordinate and the y coordinate, respectively.
   This returns a result of type Euclid2D back.
 -}

scalarMult :: Double -> Euclid2D -> Euclid2D
scalarMult s e = (s * getX e, s * getY e)

{- -----------------------------------------------------------------
 - add
 - -----------------------------------------------------------------
 - Description: The add function takes two Euclid2D Parameters, and adds the x coordinates and the y coordinates from each of the Euclid2D parameter, returning a Euclid2D type as a result.
 -}

add :: Euclid2D -> Euclid2D -> Euclid2D
add e1 e2 = (getX e1 + getX e2 , getY e1 + getY e2)

{- -----------------------------------------------------------------
 - innerProduct
 - -----------------------------------------------------------------
 - Description: The innerProduct Function takes 2 Euclid2D parameters and adds the products between both the x coordinates and the y coordinates.
 -}

innerProduct :: Euclid2D -> Euclid2D -> Double
innerProduct e1 e2 = (getX e1 * getX e2 + getY e1 * getY e2)

{- -----------------------------------------------------------------
 - distance
 - -----------------------------------------------------------------
 - Description: The distance function gives the distance between two points by using the following formula : d(p,q) = sqrt ((x2 - x1)^2 + (y2 - y1)^2) 
 -}

distance :: Euclid2D -> Euclid2D -> Double
distance e1 e2 = sqrt ((getX e2 - getX e1)^2 + (getY e2 - getY e1)^2)

{- -----------------------------------------------------------------
 - maxDistance
 - -----------------------------------------------------------------
 - Description: maxDistance function takes a list of Euclid2D types, and returns a Euclid2D result (i.e essentially a geometrical point) that has the maximum distance from the origin (0,0).
 -}


maxDistance :: [Euclid2D] -> Euclid2D
maxDistance [] = (0,0)
maxDistance [e] = e
maxDistance (e0:e1:es) 
    | distance (0, 0) e0 >= distance (0, 0) e1 = maxDistance(e0:es)
    | otherwise = maxDistance (e1:es)





{-

### TEST CASES ###

Function: scalarMult
Test Case Number: #1 
Input: scalarMult 0 (1,2)
Expected Output: (0.0,0.0)
Actual Output: (0.0,0.0)

Function: scalarMult
Test Case Number: #2
Input: scalarMult 1.5 (3,4)
Expected Output: (4.5,6.0)
Actual Output: (4.5,6.0)

Function: scalarMult
Test Case Number: #3
Input: scalarMult 25.89 (4.5,11.24)
Expected Output: (116.505,291.0036)
Actual Output: (116.505,291.0036)



Function: add
Test Case Number: #1
Input: add (0,0) (1,4)
Expected Output: (1.0,4.0)
Actual Output: (1.0,4.0)

Function: add
Test Case Number: #2
Input: add (4,7) (12,19)
Expected Output: (16.0,26.0) 
Actual Output: (16.0,26.0)

Function: add
Test Case Number: #3
Input: add (-2.5,3.98) (4.5,-9.25)
Expected Output: (2.0,-5.27)
Actual Output: (2.0,-5.27)



Function: innerProduct
Test Case Number: #1
Input: innerProduct (0,0) (13,75)
Expected Output: 0.0
Actual Output: 0.0

Function: innerProduct
Test Case Number: #2 
Input: innerProduct (-4,7) (11,72)
Expected Output: 460.0
Actual Output: 460.0

Function: innerProduct
Test Case Number: #3 
Input: innerProduct (4.90,7.54) (13.47,71.3)
Expected Output: 603.605 
Actual Output: 603.605




Function: distance
Test Case Number: #1
Input: distance (0,0) (10,-10)
Expected Output: 14.142135623730951
Actual Output: 14.142135623730951

Function: distance
Test Case Number: #2 
Input: distance (18.98,13.87) (0,-43)
Expected Output: 59.95362624562421
Actual Output: 59.95362624562421

Function: distance
Test Case Number: #3 
Input: distance (91,76) (409,67)
Expected Output: 318.1273329973393
Actual Output: 318.1273329973393




Function: maxDistance
Test Case Number: #1
Input: maxDistance []
Expected Output: (0.0,0.0)
Actual Output: (0.0,0.0)

Function: maxDistance
Test Case Number: #2
Input: maxDistance [(1,2)]
Expected Output: (1.0,2.0)
Actual Output: (1.0,2.0)


Function: maxDistance
Test Case Number: #3
Input: maxDistance [(1,2),(-10,10),(10,10),(-10,-10)]
Expected Output: (-10.0,10.0) 
Actual Output: (-10.0,10.0)

Function: maxDistance
Test Case Number: #4
Input: maxDistance [(1,2),(-10,10),(10,10),(-10,-10),(100,76)]
Expected Output: (100.0,76.0)
Actual Output: (100.0,76.0)

-} 
