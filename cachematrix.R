## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        #makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
        #1. Set value of m to NULL
        inv <- NULL
        #2. Cache matrix?
        set <- function(y){
                x <<- y
                # Clear cached value of m
                inv <<- NULL
        }
        get <-  function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
        
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        #cacheSolve: This function computes the inverse of the special "matrix" 
        #returned by makeCacheMatrix above. If the inverse has already been calculated
        #(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
        inv <- x$getInverse
        ## Check whether there is a matrix cached
        if(!is.null(inv)) {
                message("Getting cached data...")
                return(inv)
        }
        # Then calculate solve(x)
        data <- x$getInverse()
        inv <- solve(data)
        x$setInverse(inv)
        inv
        
}
