## Solution to Coursera rprog-007 assignment #2.
##
## Use makeCacheMatrix() to create a "matrix" (list) that stores a cached solve() of the matrix
## passed in and cacheSolve() to take the results of makeCacheMatrix() and return the solve().
## Assumes the matrix is always invertible.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## The 'matrix' is actually a list what has a setter, getter and a method to set and get the result
## of the solve() operation
makeCacheMatrix <- function(x = matrix()) 
{
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) 
{
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached solved matrix")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
