## Cached Inverse Matrix Calculation
#  Usage: cacheSolve(makeCacheMatrix(YOUR_MATRIX), ...)
#         further arguments will be passed to the solve() function.

## makeCacheMatrix creates a special "matrix", which is really a list containing functions to:
#  1. set the value of the matrix
#  2. get the value of the matrix
#  3. set the value of the inverse
#  4. get the value of the inverse

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


## cacheSolve calculates the inverse of the special "matrix" created with the above function.
#  However, it first checks to see if the inverse has already been calculated.
#  If so, it gets the inverse from the cache and skips the computation.
#  Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setinv(i)
  i
}
