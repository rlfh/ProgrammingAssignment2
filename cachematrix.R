## These function implement a caching mechanism for a matrix and its corresponding reverse

## This function implements the cache for the matrix, with functions for setting and getting the values stored. 

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



## This function attempts to read from the cache and if the value is not yet cached, solves the matrix
## and stores the value in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinv()
        if(!is.null(m)) {
                print("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}
