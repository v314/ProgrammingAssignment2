## Two functions, able to set the value of a matrix, calculate its inverse, cache the result, and 
## retrieve the value from the cache (if already calculated)

## Creates a special "matrix" object: a list of functions able to set and get the value
## of the matrix and the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by the makeCacheMatrix function; if the 
## inverse has already been calculated, it retrieves the value from the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting data from cache")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
