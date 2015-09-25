## Matrix inversion is usually a costly computation, so we implement a pair of functions that
## caches the inverse of the matrix to avoid having to compute it every time it is needed.

## makeCacheMatrix creates a matrix object that allows you to store a matrix upon creation and
## also call 4 additional functions: set allows you to set the matrix to a new value and resets
## the cached inverse to NULL, get returns the matrix, setInverse sets the inverse of the matrix
## (and should not be called directly), and getInverse returns the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    ## Set inverse to NULL upon creation
    inverse <- NULL
    
    ## set function overwrites x the original matrix with y and sets the cached inverse to NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    ## get function returns the matrix
    get <- function() x
    
    ## setInverse function sets the inverse of the matrix so it is cached (should not be called
    ## directly but is instead called by the cacheSolve function)
    setInverse <- function(inv) inverse <<- inv
    
    ## getInverse returns the cached inverse 
    getInverse <- function() inverse
    
    ## list function creates the list of functions which can be used
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

## cacheSolve checks to see if there is already a cached inverse matrix.  If there is, it 
## return the inverse, otherwise it calculates the inverse using the solve() function and 
## caches the result using the setInverse function from makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Gets the inverse matrix if cached
    inverse <- x$getInverse()
    
    ## If the inverse has been cached (is not NULL), return it
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    
    ## Otherwise, it is necessary to calculate the inverse so first get the matrix
    matrix <- x$get()
    
    ## Calculate the inverse using the solve() function
    inverse <- solve(matrix, ...)
    
    ## Cache the inverse using the setInverse function from makeCacheMatrix
    x$setInverse(inverse)
    
    ## Return the inverse of the matrix
    inverse
}
