# An R function may be able to cache potentially time-consuming computations.
# With the following functions it will be able to store the inverse of matrix.
# This way, after the first calculation is done, it will return the value
# without recalculating it.

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# This function computes the inverse of the matrix returned by makeCacheMatrix.
# If the inverse has already been calculated, returns the value stored.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- i$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}