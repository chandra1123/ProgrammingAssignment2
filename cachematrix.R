## Pair of functions that cache the inverse of a matrix.


## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  getInverse <- function() inv
  
  list(set = set, get = get, getInverse=getInverse, setInverse=setInverse)

}


## This function computes the inverse of special matrix object returned by 
## makeCacheMatrix. If the matrix object was not changed since inverse
## was calculated, it returns the cached value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  message("computing inverse")
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
