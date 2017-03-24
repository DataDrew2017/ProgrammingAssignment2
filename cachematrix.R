## The pair of functions below cache the inverse of a square invertible matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        
        ## Set the value of the inputted matrix.
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        
        ## Get the value of the inputted matrix.
        get <- function() x
        
        ## Set the value of the inverted matrix.
        setinverse <- function(solve) invrs <<- solve
        
        ## Get the value of the inverted matrix.
        getinverse <- function() invrs
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse 
## has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.
cacheSolve <- function(x, ...) {
        invrs <- x$getinverse()
        
        ## Check to see if the inverse has already been calculated and, if so, print.
        if(!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
        }
        
        ## Otherwise, solve for and print inverted matrix.
        invrs <- solve(x$get(), ...)
        # Set value of inverse in the cache.
        x$setinverse(invrs)
        invrs
}