## makeCacheMatrix and cacheSolve add 
## inverse caching to a matrix

## makeCacheMatrix takes a matrix and gives that
## matrix the ability to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
		set <- function(y) {
			x <<- y
			i <<- NULL
        }
		get <- function() x
		setinverse <- function(solve) i <<- solve
		getinverse <- function() i
        list(set = set, get = get,
			 setinverse = setinverse,
			 getinverse = getinverse)
}

## cacheSolve returns the inverse of a matrix
## created with makeCacheMatrix.
##
## If inverse of the matrix was previously solved,
## the cached inverse is returned.
##
## This can potentially processing time.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
