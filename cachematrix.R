## The following code implements a pair of functions that cache the inverse of a matrix.
## The inverse of the matrix is cached so that the inverse calculation doesn't have to be performed
## more than once in the event that the data in the matrix hasn't changed.
## The first function creates the matrix object.
## Then the second function performs the inverse operation on the matrix the first time
## it's evaluated and thereafter, the result of the inverse operation is retrieved from cache.

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(setsolve) m <<- setsolve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by the makeCacheMatrix function above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cacheSolve function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
    ## Return a matrix that is the inverse of 'x'
}
