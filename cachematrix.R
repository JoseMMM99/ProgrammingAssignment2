## Sorry if there are some typos in my comments
## English is not my native language

##This function creates a special "matrix" object that can cache its inverse.
##Basically I have renamed objects using "_inv" instead of "mean"
##and I replaced the "mean" function with "solve" function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above. If the inverse 
##has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

## Same strategy as above.
## The result of the previous function must be saved in an object 
## to use the cache information if the cacheSolve function is used again.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data (inverse matrix)")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
