## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creating a special matrix that consists of a numeric vector
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function()x
    setinverse <- function(inverse)inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## A function that caches the inverse of the special matrix
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
