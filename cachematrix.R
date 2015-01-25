## These functions allow caching of the inverse of a matrix such that it can be
## retrieved later, and this cached inverse can be accessed from within other functions

## makeCacheMatrix takes an input matrix and returns a list of functions 
## that can be used as a parameter to cacheSolve function to get inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve takes a list of fucntions returned by makeCacheMatrix as input
## and returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if ( !is.null(s) ) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
