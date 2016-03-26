## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL

    set <- function(y) { 
        x <<- y
        i <<- NULL
    }
    get        <- function()  { x }
    setInverse <- function(m) { i <<- m }
    getInverse <- function()  { i } 

    list(set        = set,
         get        = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data...")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setInverse(i)
    i
}
