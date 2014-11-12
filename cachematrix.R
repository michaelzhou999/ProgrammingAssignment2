library(fBasics)

## Take a matrix and return a list of functions
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(i) i <<- i
    getinv <- function() i
    list(set = set, get = get, getinv = getinv, setinv = setinv)
}


## Return the inverse of matrix, out of cache if
## it's already calculated.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- inv(data)
    x$setinv(m)
    m
}
