## "The functions store a matrix and cache the inverse of a matrix"

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
                }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the matrix by 
## makeCacheMatrix above. If the inverse has already been calculated,
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
