## These two functions work together to cache the inverse
## of a matrix

## makeCacheMatrix takes a matrix and and creates a special
## "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInv <- function(inv) i <<- inv
    getInv <- function() i
    list(set = set, get = get,
        setInv = setInv,
        getInv = getInv)
}


## cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already 
## been calculated (and the matrix has not changed), then 
## the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    i <- x$getInv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInv(i)
    i
}
