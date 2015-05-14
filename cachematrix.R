## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates and returns a special matrix 
## object that includes a set of internal functions that allow the 
## inverse of a matrix to be cached for fast retrieval as long as the 
## input matrix remains constant.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setInv <- function(y) inv <<- y
    
    getInv <- function() inv
    
    list(set = set, get = get, 
         setInv = setInv, getInv = getInv)
}


## The cacheSolve function returns the inverse of a matrix object
## that is created by the makeCacheMatrix function.  If the inverse
## of the current matrix has already been computed, this function
## simply returns the cached version of the inverse matrix.  If not,
## this function executes the solve function to calculate the inverse
## of the matrix and then caches it.
cacheSolve <- function(x, ...) {
    y <- x$getInv()
    
    if(!is.null(y)) {
        message("getting cached inverse")
        return(inv)
    }
    
    data <- x$get()
    y <- solve(data, ...)
    x$setInv(y)
    
    y
}
