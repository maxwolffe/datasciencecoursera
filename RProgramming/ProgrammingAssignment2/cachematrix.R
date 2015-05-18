## Put comments here that give an overall description of what your
## functions do

## Creates an abstract data type for matrix that allows a user to:
## get - get the original matrix, 
## set - set the original matrix,
## setInverse - set the cached inverse of the matrix,
## getInverse - get the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Takes an abstract data type CacheMatrix (as created above)
## First checks to see if there exists a cached matrix, if so returns it. 
## Otherwise calcuates the inversed matrix and sets the cache. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
