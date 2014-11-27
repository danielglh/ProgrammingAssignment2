## Matrix inversion is usually a pretty costly operation and the aim
## of these two functions is to create a special wrapper for matrix
## and cache the inverse of the matrix inside the wrapper.

## Wrap a matrix to make it possible to cache inverse of the matrix
## inside the wrapper.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ## Raw matrix setter
    set <- function(m) {
        x <<- m
        i <<- NULL
    }
    ## Raw matrix getter
    get <- function() x
    ## Cached inverse setter
    setInverse <- function(inverse) i <<- inverse
    ## Cached inverse getter
    getInverse <- function() i
    ## Return the wrapper
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## Calculate inverse of a wrapped matrix based on the caching logic

cacheSolve <- function(x, ...) {
    ## Try to get a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if (is.null(i)) { ## No cached data for the inverse of 'x'
        message("No cached data!")
        ## Fetch the raw matrix and calculate the inverse
        ma <- x$get()
        i <- solve(ma, ...)
        ## Cache the calculated inverse
        x$setInverse(i)
    }
    else { ## Cached data found for the inverse of 'x'
        message("Getting cached data!")
    }
    i
}
