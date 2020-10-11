## Matrix inversion is usually a costly computation and there may be some 
##    benefit to caching the inverse of a matrix rather than compute it 
##    repeatedly 

## makeCacheMatrix: This function creates a special "matrix" object that can 
#     cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverseM <- NULL
    set <- function(y) {
      x <<- y
      inverseM <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverseM <<- inverse
    getInverse <- function() inverseM
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned 
##   by makeCacheMatrix above. If the inverse has already been calculated 
##   (and the matrix has not changed), then cacheSolve should retrieve the 
##   inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  inverseM <- x$getInverse()
    if (!is.null(inverseM)) {
      message("getting cached matrix")
      return(inverseM)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inverseM)
    inverseM
}
