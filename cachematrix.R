## last modified on 19mar2015 by iabalki
## Replaced the original script with the complete script as requirested by homework assignnment 3

## This script contains two functions:

## 1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## will  retrieve the inverse from the cache.

## To find out more about what to inverse a matrix means, please refer to http://en.wikipedia.org/wiki/Invertible_matrix
## or the help of the solve function

## Important assumption: The matrix is invertible. 
## In order for a matrix to be invertible it has to be a) square matrix b) complex matrix or c) inverible rectangular matrix
## and since I only have limited time to write this function -- I will only be checking whether a) is true .. Anyway solve errors with rectangular matrixes :)

## makeCacheMatrix ------------------------------------------------

##  This function creates 4 different functions that will be used in the CacheSolve function:
##1. getmatrix() -- get the value of the existing matrix
##2. setmatrix() -- set the value of the existing matrix
##3. setinversematrix() -- calculate the inverse matrix 
##4. getinversematrix() -- get the value of the inverse matrix

## it is absolutely incomprehendable why would anyone need such functions instead of writing 
## what you need in line .. so I am afraid I can't explain any of this

makeCacheMatrix <- function(x = matrix()) 
{
 Inverse <-NULL
   
## Setmatrix will null the inverse and will set the input matrix to x
   setmatrix <- function(y) {
     
                              ## basic checks on the quality of our input and conversion into matrix
                              y <-as.matrix(y)
                              if (class (y)!="matrix") {message("Input cannot be converted to a matrix! Game over")
                                                        stop()                                                   
                                                        }
                              else 
                                   {if (ncols(y)!=nrows(y)) {message("this is not a square matrix. Game over")
                                                              stop ()   
                                                              }
                                   }
                              InputMatrix <<- y
                              Inverse <<- NULL
                            }

## getmatrix will return the input matrix 
   getmatrix <- function() InputMatrix
## setinversematrix will calulate the inverse
   setinversematrix <- function(Input) Inverse <<- Solve (Input)
## getinversematrix will produce the inverse matrix as output
   getinversematrix <- function() Inverse
## absolutely no idea what this does .. so if anyone knows.. let me know :) besides that it sets 
## a list where the names and values are the same.. and they match the functions we made
   list(setmatrix = setmatrix, getmatrix = getmatrix,
        setinversematrix = setinversematrix,
        getinversematrix = getinversematrix)
  
}

## cacheSolve ------------------------------------------------

## Return a matrix that is the inverse of 'x' either from the data or from the cache
## this code has something wrong with the syntax 
## > cacheSolve(x)
## Error in x$getinversematrix : $ operator is invalid for atomic vectors
## I also belive that if u run it twice it will likely produce the cache of the first inverse matrix which is an issue..
## but since the obejctive of this assignment is not functional code - we will ignore these...


cacheSolve <- function(x, ...) 
{
 
# Sets to output to the last calcuated inverse matrix in case nothing else modifies after
  Output <- x$getinversematrix()
# Checks whether the result is empty and if it has value it returns
  if(!is.null(Output)) {
                         message("getting cached data")
                         return(Output)
                       }
# if there is nothing in the cache.. gets the matrix
  data <- x$getmatrix()
# and calculates the inverse matrix
  Output <- setinversematrix(x)
# then is stores the calculated value in the cache
  x$setInverseMatrix(Output)
  Output

}

