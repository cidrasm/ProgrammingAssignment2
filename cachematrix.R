## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
##   local variable inv will hold cached computed inverse
##   of matrix x

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve(x, ...)
## x   : result of makeCacheMatrix above
## ... : additional parameters
## Get cached inverse value or NULL if it hasn't been computed yet
## Otherwise compute inverse of matrix stored in x and store it
## internally in the cache variable in x
## return this value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (! is.null(inv)) {
    message('Returning cached inverse matrix')
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
