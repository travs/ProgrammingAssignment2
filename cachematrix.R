## makeCacheMatrix provides a list that has four functions. 
## Combined, these four functions store and retrieve a 
## matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve takes in a "cacheMatrix" object and sets the inverse
## if it is not already set. Otherwise, the inverse is returned.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinv(inv)
  inv
}
