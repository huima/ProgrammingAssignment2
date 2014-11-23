## This is programming exercise that demonstrates how
## computationally expensive function results can be
## cached and reused
##
## makeCacheMatrix creates cache object that then gets
## used in cacheSolve function, demonstrating how
## to use the cache object

## makeCacheMatrix returns a cache object 

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL ## clear the cache!
  }
  get <- function() x
  setinverse <- function(inversematrix) cache <<- inversematrix
  getinverse <- function() cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve uses cache object to either return cached result or 
## calculate it and then store the result in the cache

cacheSolve <- function(x, ...) {
  cachedata <- x$getinverse()
  if(!is.null(cachedata)) {
    message("getting cached data")
    return(cachedata)
  }
  data <- x$get()
  invmatrix <- solve(data, ...)
  x$setinverse(invmatrix)
  invmatrix
}
