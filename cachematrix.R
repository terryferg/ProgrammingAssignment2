##  This creates two functions: 1) makeCacheMatrix - This function creates a 
##  special "matrix" object that can cache its inverse. Matrix inversion is 
##  usually a costly computation and there may be some benefit to caching the
##  inverse of a matrix rather than compute it repeatedly. 2)cacheSolve - This 
##  function computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix above. If the inverse has already been calculated (and the 
##  matrix has not changed), then the cachesolve should retrieve the inverse 
##  from the cache.
##
##  These functions will assume that the matrix supplied is always invertible.

## Computing the inverse of a square matrix can be done with the solve function
##  in R. For example, if X is a square invertible matrix, then solve(X) returns
##  its inverse.

## creates a special "matrix" object that stores a matrix and its inverse
##  by using 4 functions and creates a list with each function as an element of 
## the list. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set.inverse.matrix <- function(solve) m <<- solve
        get.inverse.matrix <- function() m
        list(set = set, get = get, set.inverse.matrix=set.inverse.matrix,
             get.inverse.matrix = get.inverse.matrix)
}

## Return the inverse of a  matrix of 'x'. it needs the makeCacheMatrix()
##  function.

cacheSolve <- function(x, ...) {
       m <- x$get.inverse.matrix()
       if(!is.null(m)) {
               message("getting cached inverse matrix")
               return(m)
       }
        data <- x$get()
       m <- solve(data, ...)
       x$set.inverse.matrix(m)
       m
 }


