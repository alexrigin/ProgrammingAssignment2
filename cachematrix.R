## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## The first function creates a special 'Matrix' object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    invrsn <- NULL
    set <- function(y) {
        x <<- y
        invrsn <<- NULL
    }
    get <- function() x
    
    setInverse <- function(z) invrsn <<- z
    getInverse <- function() invrsn
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse  = getInverse)
}


## The second function computes the inverse of the special "matrix" created by 
## the first function (makeCacheMatrix). If the inverse has already been 
## calculated and the matrix has not changed, then it will get the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invrsn <- x$getInverse()
    
    if (!is.null(invrsn)) {
        message("getting cached data")
        return(invrsn)
    }
    
    mat <- x$get()
    invrsn <- solve(mat, ...)
    x$setInverse(invrsn)
    invrsn
}
