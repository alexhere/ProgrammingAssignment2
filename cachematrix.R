## makeCacheMatrix creates a special "matrix", which is really a 
## list containing a function to
##     set the value of the matrix (set)
##     get the value of the matrix (get)
##     set the value of the inverse matrix (setInverse)
##     get the value of the inverse matrix (getInverse)

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix) im <<- inverseMatrix
  getInverse <- function() im
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## cacheSolve computes the inverse of the special "matrix"
## created with the makeCacheMatrix function.

## However, it first checks to see if the inverse matrix has
## already been computed. If so, it gets the previously computed 
## inverse matrix from the cache and skips the computation.
## Otherwise, it computes the inverse of the original matrix 
## and stores the computed inverse matrix in the cache via 
## the setInverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getInverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setInverse(im)
  im
}
