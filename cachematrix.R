## Put comments here that give an overall description of what your
## functions do
    # These functions create an inversable "matrix" object, whose 
    # inverse is stored in the cache so it only needs to be 
    # calculated once per matrix.

## Write a short comment describing this function
    # Creates a list conatining four functions for setting and 
    # getting the "matrix", as well as its inverse, to the cache.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
    # Checks if there is a cached inverse for the "matrix". 
    # If there is one, retrieves it from the cache and ignores 
    # additional arguments. If there is no cached inverse, 
    # computes it using 'solve' and any additional matrices 
    # added through arguments.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...) 
    x$setinverse(i)
    i
}
