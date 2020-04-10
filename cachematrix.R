## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL             ## intialise local var. for inverse
    set <- function(y) {    ## setter for matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x                      ## getter for matrix
    setInverse <- function(inn) inv <<- inn  ## setter for inverse
    getInverse <- function() inv             ## getter for inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    localInverse <- x$getInverse()      ## get cached inverse
    if(!is.null(localInverse)) {        ## deliver cached inverse, if valid
        message("getting cached Inverse")
        return(localInverse)
    }
    data <- x$get()                     ## get data to calculate inverse
    localInverse <- solve(data, ...)    ## calculate inverse
    x$setInverse(localInverse)          ## set inverse for later use i.e. caching
    localInverse
}
