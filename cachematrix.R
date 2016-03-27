## Create an object that stores a matrix and its inverse
## invert the matrix and store it in the object if it doesn't already exist.
## This works by setting variables in the environment created by 
## makeCacheMatrix; the parent environment of the functions it returns.

## makeCacheMatrix returns a list of functions for setting and getting 
## variables in the functions parent environment.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL

    set <- function(y) { 
        x <<- y
        i <<- NULL
    }
    get <- function() { 
        x 
    }
    setInverse <- function(m) { 
        i <<- m 
    }
    getInverse <- function() { 
        i 
    } 

    list(set        = set,
         get        = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve operates on a makeCacheMatrix list, returning the inverse if it 
## already set, otherwise getting the matrix, then setting the inverse and 
## returning it.
cacheSolve <- function(x) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data...")
        return(i)
    }
    m <- x$get()
    i <- solve(m) 
    x$setInverse(i)
    i
}
