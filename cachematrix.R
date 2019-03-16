## The function aims to reduce the computation time of the inverse of matrix. To achieve this
## goal, this function is able to look up the cache to find the value of inverse instead of 
## recompute it.

## This function creates a special "matrix", which can sets and gets the value of the matrix 
## and inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function first checks if the matrix created by makeCacheMatrix has been calculated.
## If so, this function output the inverse which stored in the cache. Otherwise, it 
## calculates the inverse of the data and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
