## Program that provides the inverse of an invertible matrix without running
## the solve calculation if it does not have to.

## Function that creates a list of functions to operate on the matrix 'x'. 
## The functions let the user set the matrix data in the cache, get the matrix data,
## set the inverse of the matrix in the cache, and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      setMatrix <- function(y) {
            x <<- y
            inv <<- NULL
      }
      getMatrix <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list (setMatrix = setMatrix, getMatrix = getMatrix, 
            setInverse = setInverse, getInverse = getInverse)
}


## Function that returns the inverse of x by looking in the cache first
## then calculates it if not in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      if(!is.null(inv)) {
            message("Getting cached inverse")
            return(inv)
      }
      matrixData <- x$getMatrix()
      inv <- solve(matrixData)
      x$setInverse(inv)
      inv
}
