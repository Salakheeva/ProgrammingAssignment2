## Assignment: Caching the Inverse of a Matrix

# makeCacheMatrix creates a list of functions: 
#1. set/get the value of the matrix (m)
#2. set/get the value of the inversed matrix (i)
makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#Calculates the inversed matrix or takes it value from cache
#works with a list of functions, specified above
cacheSolve <- function(m, ...) {
  i <- m$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- m$get()
  i <- solve(data, ...)
  m$setInverse(i)
  i
}
