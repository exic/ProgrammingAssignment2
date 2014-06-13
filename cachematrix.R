## Create a matrix wrapping function ("object") that will cache the matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse <<- inverse
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Return a matrix that is the inverse of 'x'
## Requires x to be of "makeCacheMatrix" type

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        # inverse has been calculated already
        return(inverse)
    }
    data <- x$get()
    # actual solving
    inverse <- solve(data)
    # save inverse
    x$setinverse(inverse)
    inverse
}
