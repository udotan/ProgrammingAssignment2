## Put comments here that give an overall description of what your
## functions do

## Setting a null matrix to store the cached results of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) { 
        s <- NULL
        print(environment())
        evn <- environment()
        print(parent.env(evn))
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        getenv <- function() environment()
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve,
             getenv   = getenv)

}


## Pull inverse from chache if it is available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("R is now retrieveing cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
