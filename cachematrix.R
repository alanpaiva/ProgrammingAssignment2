## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        if (det(x)==0) {        ##matrix provided isn't invertible, Error!
                message("Error: matrix provided should be invertible!")
        } else {                ##matrix provided is invertible then, create cache matrix
        z <- NULL
        set <- function(y) {
                x <<- y
                z <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) z <<- solve
        getsolve <- function() z
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        }
}
## Make cacheSolve function：calculates the inverse of the special matrix regressed by
## makeCacheMatrix. If the inverse has already been calculated and, the matrix hasn't changed. 
## The cacheSolve should restore the inverse from the cache.

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        z <- x$getsolve()
        if(!is.null(z)) {
                message("Acquiring cached data...")
                return(z)
        }
        data <- x$get()
        z <- solve(data, ...)
        x$setsolve(z)
        z
        ## Return a matrix that is the inverse of 'x'
}
