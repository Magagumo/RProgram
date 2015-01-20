## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix' stores a list of 4 functions associated with the stored 
## matrix, including how to set its value, get its value, sets its solve value 
## (i.e. inverse), and retrieve its solve value

makeCacheMatrix <- <- function(x = numeric()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## 'cacheSolve' takes a cached matrix and uses its saved functions to check
## the solved inverse (if it exists); otherwise it solves for the inverse;  
## inverse is then output either way

cachesolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
