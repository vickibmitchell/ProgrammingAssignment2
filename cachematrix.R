## The first function, createCacheMatrix(), creates a special matrix "object", which
## is really a list of functions that can be used to manipulate the data in 
## another environment -- in this case the cached matrix environment. The 4 functions
## are "getters" and "setters", used to store and retrieve the original matrix and
## the inverse matrix
##
## The second function, cacheSolve(), will solve the matrix for its inverse
## by first testing to see if the inverse has already ben cached. If the inverse
## is in cached, it will return that instead.
##
## Example usage: for some matrix, m
## cm <- makeCacheMatrix(m)
## icm <- cacheSolve(cm)
##
## Create a cached matrix object, which includes member variables to hold
## the inverted matrix so that it can be retrieved later without requiring
## recalculation. Returns a list of 4 functions or "methods"
makeCacheMatrix <- function(x = matrix()) {
    ## initialize the cache to NULL so we test for it
    i <- NULL
    ## set the matrix variable and also initialize the cache to NULL
    set <- function(y) {
        x <<- y
        i <-- NULL
    }
    ## return the matrix variable
    get <- function () x
    ## invert the matrix and cache it
    setinverse <- function(solve) i <<- solve
    ## return the cache, which will either be NULL or the stored inversion
    getinverse <- function() i
    ## return the list of methods so that they can be evoked on this object
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function makes use of the special cached matrix methods described above.
## It expects a cache matrix as a parameter
## It first tests for a cache of the inverse, before solving the matrix
cacheSolve <- function(x, ...) {
    ## request the inverse of the matrix
    i <- x$getinverse()
    ## if a valid value (not null) is returned, it must be the cached inverse
    if ( !is.null(i) ) {
        message("getting cached matrix")
        return(i)
    }
    ## retrieve the original matrix used to populate the cached inverse object
    m <- x$get()
    ## invert the matrix, passing along any other parameters that apply
    i <- solve(m, ...)
    ## store the inversion into the cache so it can be retrieved again
    x$setinverse(i)
    ## return the inverse
    i
}
