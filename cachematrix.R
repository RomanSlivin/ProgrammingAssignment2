## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix representation (list-based) 
## that can be passed to cacheSolve function 
## for caching-aware computation of the inverse matrix

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
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse of the matrix x which should be previously 
## created by a call to makeCacheMatrix function. If the calculation
## has already been performed before, returns the result from "cache".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached Inverse matrix...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
