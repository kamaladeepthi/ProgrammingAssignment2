## The functions makeCacheMatrix and cacheSolve cache the inverse of a
## matrix.

## The function makeCacheMatrix creates a special object "matrix" that
## caches the inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y){ 
        x <<- y
        i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The function cacheSolve computes the inverse of a matrix using 
## the solve function.
## If the inverse is already calculated, then it retrieves the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) { 
            message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
