## Functions that cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(M = matrix()) {
  M_inv <- NULL
  set <- function(S) {
    M <<- S
    M_inv <<- NULL
  }
  get <- function() M
  setinverse <- function(inverse) M_inv <<- inverse
  getinverse <- function() M_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from the
## cache.

cacheSolve <- function(M, ...) {
  M_inv <- M$getinverse()
  if(!is.null(M_inv)) {
    message("getting cached data")
    return(M_inv)
  }
  data <- M$get()
  M_inv <- solve(data, ...)
  M$setinverse(M_inv)
  M_inv
}
