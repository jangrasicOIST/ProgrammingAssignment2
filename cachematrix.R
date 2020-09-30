## Put comments here that give an overall description of what your
## functions do

## This function computes the inverse of a matrix and stores the result so 
## that it does not have to be recomputed each time.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get, 
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function accesses the cached result when you call the getsolve function.

cachesolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
