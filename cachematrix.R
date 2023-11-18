## The functions listed below create a "matrix" object that can cache its inverse 
## and compute the inverse of the "matrix". If the "matrix" has not changed and its inverse
## has already been computed, the cached inverse is retrieved without computing it again. 

## The first function creates a "matrix" object that caches its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
            x <<- y
            inv <<- NULL 
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The second function checks if the inverse of the "matrix" has been computed before and 
## retrieves it from the cache if the "matrix" has not changed since the caching. Otherwise, 
## the function computes the inverse and sets its value in the cache. 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(identical(x$set, x$get)) {
            if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
            }
        }
        data <- x$get
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
