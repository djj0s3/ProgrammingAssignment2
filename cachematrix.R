## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The below functions create a special type of cached matrix that can be used
## in combination to cache the inverse matrix.

## makeCacheMatrix accepts a matrix and returns a list of functions:
## setinverse - sets the inverse of the matrix
## getinverse - returns the inverted matrix or NULL
## set - sets the matrix
## get - returns the matrix
makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  set <- function(y) {
    x <<- y
    inversed <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inversed <<- inverse
  getinverse <- function() inversed
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve accepts a makeCacheMatrix and returns the inverse. 
## if the inverse is cached instead of computing it'll return the cached result.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversed <- x$getinverse()
  if(!is.null(inversed)) {
    message("getting cached data")
    return(inversed)
  }
  data <- x$get()
  inversed <- solve(data, ...)
  x$setinverse(inversed)
  inversed
}
