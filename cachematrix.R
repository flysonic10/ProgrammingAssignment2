## The functions in this file cache the inverse of a Matrix. If a cached matrix
## exists, the cache is returned, which prevents repeating a costly computation.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL

        # Set function sets the matrix in this CacheMatrix object
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        # Get function gets the matrix from this CacheMatrix object
        get <- function() x

        # SetInv function solves for the matrix inverse and sets it in this
        # CacheMatrix object
        setinv <- function(solve) m <<- solve

        # GetInv function gets the matrix inverse
        getinv <- function() m

        # This CacheMatrix object is a list of the above functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        # Retrieve the cached inverse
        m <- x$getinv()

        # If the inverse exists and was successfully retrieve, simply return it.
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }

        # Otherwise, get the object and solve the inverse.
        data <- x$get()
        m <- solve(data, ...)

        # Once solved, cache the inverse in the CacheMatrix object
        x$setinv(m)

        # Return the newly solved inverse
        m
}
