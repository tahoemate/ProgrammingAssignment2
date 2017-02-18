## Pair of functions to cache the inverse of a matrix 
## and retrieve cached version.

## makeCacheMatrix() given a matrix argument x, create a special vector that can cache its inverse.
## This vector has 4 functions:
##
## get: retrieves matrix x
## set: stores matrix x
## getinverse: return inverse of matrix x
## setinverse: sets the inverse of matrix x

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL    # erase cache on matrix change
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve(): compute the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i  
}
