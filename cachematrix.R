## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix as an argument returns a list
## of functions to access cached values
makeCacheMatrix <- function(x = matrix()) {
    # initialize inverse variable
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    # use 'solve' function to get the inverse of the matrix
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv, getinv = getinv) 
}


## This function looks if the inverse has been previously set
## if so, it accesses and returns the cached data, if not it
## calculates the inverse and stores it
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    # Checks if cached data exists, if so, returns that value
    if(!is.null(inv)) {
        message('getting cached data')
        return(inv)
    }
    # Otherwise calculate inverse, store cached value, return inv
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
