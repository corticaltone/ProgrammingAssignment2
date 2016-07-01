## This pair of functions will store a cached version of a matrix, calculate it's inverse and store
## the inverse for future reference


## makeCacheMatrix will store a cached version of a matrix that can be acted on by cacheSolve
## outputs a list containing the matrix and functions for manipulating it

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolution <- function(solve) s <<- solve
  getsolution <- function() s
  list(set = set, get = get, setsolution = setsolution, getsolution = getsolution)
  
}


## cacheSolve will return the inverse of a cached matrix. If the inverse has previously been
## calculated, cacheSolve retrieves the stored inverse. If it has not, cacheSolve calculates
## the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolution()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolution(s)
  s
}
