## makeCacheMatrix creates a type of cacheable matrix object,
## which can be used with the cacheSolve function to compute
## the inverse of the matrix

## Creates a cacheable matrix, with functions to set and retrieve the
## data and set and get the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL

    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i

    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Computes the inverse of the matrix using 'solve' and caches it,
## if the inverse has already been saved and the original matrix
## is unchanged, then just gets the cached data

cacheSolve <- function(x, ...) {
    inv <- x$getinv()

    ## Check if the cached value exists
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    ## Compute and save the inverse matrix
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)

    ## Return a matrix that is the inverse of 'x'
    inv
}
