## The following code implements a pair of functions that cache the inverse of a matrix.
## The inverse of the matrix is cached so that the inverse calculation doesn't have to be performed
## more than once in the event that the data in the matrix hasn't changed.
## The first function, makeCacheMatrix, creates the matrix object.
## Then the second function, cacheSolve, performs the inverse operation on the matrix the first time
## it's evaluated and thereafter, the result of the inverse operation is retrieved from cache.

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## initialize the cached inverse of the matrix, setting it to NULL
    ## to indicate that the inverse of the matrix hasn't been computed yet
    m <- NULL
    set <- function(y) {
      ## set a new value of the matrix x to y
      x <<- y
      ## indicate that the new value of the matrix hasn't been computed yet
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(setsolve) m <<- setsolve
    getsolve <- function() m
    ## return a list of accessor functions (set, get) for the matrix and the cached version of the matrix
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by the makeCacheMatrix function above. If the inverse has 
## already been calculated and the matrix has not changed, then 
## the cacheSolve function retrieves the inverse from the cache.
## This function assumes the matrix is square and can be inverted.
## Therefore, the function does not handle errors in the event it is not possible to invert the matrix.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    ## check to see if inverse of the matrix has already been calculated
    if(!is.null(m)) {
      message("getting cached data") 
      ## if inverse has been calculated, retrieve it from cache
      return(m)
    }
    ## if inverse of matrix hasn't been calculated, calculate the inverse using the solve function
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
    ## Return a matrix that is the inverse of 'x'
}
