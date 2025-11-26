## This file contains two functions that work together to store a matrix and 
## cache (save) its inverse so that R does not have to calculate it again 
## every time. 

## makeCacheMatrix(), creates a special object that holds 
## a matrix and can also store its inverse. It returns a list of four small 
## functions that let us:
##   1. Set a new matrix.
##   2. Get the current matrix.
##   3. Save (cache) the inverse of the matrix.
##   4. Retrieve the cached inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL          
    set <- function(y) {
        x <<- y
        inv <<- NULL         
    }
    
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv

    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}

## cacheSolve(), uses the special object created by 
## makeCacheMatrix(). This function checks whether the inverse has already 
## been calculated. 
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()    
    
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)           
    }
    
    mat <- x$get()            
    inv <- solve(mat, ...)    
    x$setInverse(inv)        
    inv                       
}