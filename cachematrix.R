## Implementation of matrix class with inverse caching 

## General interface to create "caching" matrix 
## defines supported actions on the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # if matrix is changed we have to reset cache
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(newinv) inv <<- newinv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Returns matrix inverse. Function uses caching mechanism for matrices that were
## previously inversed 
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    # return chached value if available
    if(!is.null(inv)) {
        message("using cache for matrix inverse")
        return(inv)
    }
    inv <- solve(x$get(), ...)
    x$setinv(inv)
    inv
}

## Examples:
#  m <- matrix(c(1,2,2,1), nrow=2, ncol=2)
#  x <- matrix(c(3,2,2,3), nrow=2, ncol=2)
#  cm <- makeCacheMatrix(m)
#  cacheSolve(cm) # cache miss
#  cacheSolve(cm) # cache hit
#  cm$set(x)
#  cacheSolve(cm) # cache miss