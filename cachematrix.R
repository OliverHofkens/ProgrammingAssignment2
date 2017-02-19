## Contains functions that are able to compute the inverse of a matrix and cache
## the result.

## Creates a matrix with the ability to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse= getinverse)
}


## Computes the inverse of the matrix created with 'makeCacheMatrix', and stores
## the result in the cache. If it was already stored, it fetches it without 
## computing it again.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
          message("cache HIT")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        return(inv)
}
