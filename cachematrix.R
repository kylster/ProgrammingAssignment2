## contains two functions.
## The first creates a vector that has a matrix and is able to cache the
## inverse of the matrix.
## The second returns the inverse of a matrix; either by calculating the invers
## or returning the cached value

## USAGE:
## Use makeCacheMatrix to initialise a matrix that can calculate and cache it's inverse
## Use cacheSolve to get the inverse of a matrix that is able to cache itself.  Takes as an argument
## the return value makeCacheMatrix
##
## Example use:
## # create an invertible matrix
## > inmat <- matrix(c(1,2,3,0,1,4,5,6,0), nrow=3,ncol=3)
## # creates a 3x3 invertible matrix:
## # [,1] [,2] [,3]
## # [1,]    1    0    5
## # [2,]    2    1    6
## # [3,]    3    4    0
##
## # creat a matrix that can cache it's inverse
## > mcm <- makeCacheMatrix(inmat)
## # get the inverse.  Will use the cache if populated, otherwise
## # the inverse is calculated and returned
## > cacheSolve(mcm)

## makeCacheMatrix creates a list containing a function to
# - set the value of the matrix
# - get the value of the matrix
# - set the value of the inverse of the matrix
# - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve
# returns the inverse of the matrix.  If a cached value
# exists, then it is returned.  Otherwise the value is calculated
# and then cached.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
