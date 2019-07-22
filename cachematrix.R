## cachematrix.R contains 2 functions:
## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

## makeCacheMatrix: creates a special "matrix" object. It is passed a 
## standard matrix object and returns a list of four functions which manipulate 
## this standard matrix object. They allow to set or get the matrix object itself 
## or its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    # inverse not yet calculated right after creation
    i <- NULL
    
    # set the matrix value
    set <- function(y) {
        # set the matrix
        x <<- y
        # invalidate the inverse
        i <<- NULL
    }
    
    # get the matrix value
    get <- function() x
    
    # set the inverse
    setinv <- function(inv) i <<- inv
    
    # get the inverse
    getinv <- function() i
    
    # return the list of the four functions, named by their function names.
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve: calculates the inverse of a special "matrix" object as returned 
## by makeCacheMatrix. It first checks if the inverse has already been calculated.
## If yes, it returns the stored value. If not, it calculates the inverse and stores 
## the value in the special "matrix" object for future use.
cacheSolve <- function(x, ...) {
    
    # get the value of the inverse stored in the special "matrix" object
    inv <- x$getinv()
    
    # if it's not null, just return the cached value
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # if the value has not yet been cached, calculate it: get the matrix data
    m <- x$get()
    
    # calculate the inverse
    inv <- solve(m, ...)
    
    # store the value in the special "matrix" object's cache
    x$setinv(inv)
    
    # and return it
    inv
}
