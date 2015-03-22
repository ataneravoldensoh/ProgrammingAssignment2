## This function is a pair of functions (makeCacheMatrix and cacheSolve) for calculating and caching the inverse of a matrix
## The function makeCacheMatrix create a special matrix objet that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) z <<- inverse
  getinverse <- function() z
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}
## The function cacheSolve computes the matrix created by the function adove
cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of "x"
  z <- x$getinverse()
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setinverse(z)
  z
}
