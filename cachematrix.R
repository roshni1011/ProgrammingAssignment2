## The following are a pair of functions that can creates an initial matrix and
## then caches the inverse of that matrix

## The 'makeCacheMatrix' function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      set.inverse <- function(inverse) inv <<- inverse
      get.inverse <- function() inv
      list(set = set, get = get,
           set.inverse = set.inverse,
           get.inverse = get.inverse)
}



## This second function 'cacheSolve' computes the inverse of the matrix
## returned by the 'makeCacheMatrix' function above.
##f the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      inv <- x$get.inverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$set.inverse(inv)
      inv
}