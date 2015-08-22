## Put comments here that give an overall description of what your
## functions do

## This function creates list, which contains 4 functions to: set the matrix, get the matrix,
## set matrix inversion and get matrix inversion.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function checks, if matrix inversion was already computed. If yes, it returns that inverted matrix.
## If not, it computes matrix inversion, cache it and returns inverted matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
