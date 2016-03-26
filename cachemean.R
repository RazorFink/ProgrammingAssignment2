# An object that stores a vector and its mean value

# makeVector returns a list of functions
# the list is an environment; "m" and "x" are scoped within the list
makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, 
         get = get,
         setmean = setmean,
         getmean = getmean)
}

# cachemean expects a makeVector object
# it checks whether a mean value has been set within the scope of x
# if so, it is returned.  If not, the mean is calculated and set in x
cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
