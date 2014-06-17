## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix x as input and creates a vector containing the
## requiered functions to set the matrix, get the matrix, set the inverse, get 
## the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function evaluates whether the inverse has already been calculated
## and if so retrieves its value using the getinv() function of the object.
## If the inverse has not been calculated before it calculates it and then
## uses the function setinv() to cache it.

cacheSolve <- function(x) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
