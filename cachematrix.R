## Calculating the inverse of matrices (especially when those matrices become large), can
## become prohibitively expensive; the purpose of the two functions in this file are to
## optimize the time needed to repeately calculate the inverse for the same matrix instances
## by caching the inverse values once they have been calculated.

## This function will create a "wrapper" that will allow the cacheSolve method to cache
## and retrieve the inverse of the wrapped matrix. The matrix to be wrapped can either be
## provided as an argument to this function or can be (re)assigned via the $set method; in
## the latter case, the previously cached inverse value (if previously calculated) will be
## reset to NULL.
##
## This function returns a list that encapsulates both the wrapped matrix, its cached
## inverse value (if previously computed), and the various getter/setter functions that 
## allow the cacheSolve method to operate properly. 
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL

  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(new.inverse) {
    inverse <<- new.inverse
  }
  
  getInverse <- function() {
    inverse
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This method will use the "wrapped" matrix instance returned by the makeCacheMatrix function
## and will return the inverse of that instance. In the event that the instance's inverse has
## already been calculated, then the cached inverse value will be returned; otherwise, the 
## inverse will be caculated and then cached to optimize subsequent invocations.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()

  data <- x$get()
  
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }

  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
