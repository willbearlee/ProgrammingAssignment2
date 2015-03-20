## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    # initialization
    inversion <- NULL

    # Set the value of the matrix
    set <- function(y) {
        x <<- y
        inversion <<- NULL   # since the matrix changed
    }
    # Get the value of the matrix
    get <- function() x
    # Set the inverse
    setInverse <- function(inverse) inversion <<- inverse
    # Get the inverse
    getInverse <- function() inversion

    # Finaly, return a list of all the above functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)    
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    # check if the inverse is already cached
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # not cached, so we get the matrix into data
    data <- x$get()
    # Cal inverse matrix
    inv <- solve(data, ...)
    # Set it back
    x$setInverse(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}
