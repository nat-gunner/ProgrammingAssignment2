## This program will cache the inverse of a matrix to remove the need for 
## multiple computations of the same inverse

## makeCacheMatrix finds the inverse of a matrix and places it in the cache

makeCacheMatrix <- function(x = matrix()) {

    ##  Create a placeholder
  
    inv <- NULL
  
    ## Set the value of x and inv in the parent environment of the set function
    
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    
    ## retrieve the value of x
    
    get <- function() x
  
    ## setInv solves the input matrix and sets the value of inv in setInv's
    ## parent environment
    
    setInv <- function(solve) inv <<- solve(x)
    
    ## getInv returns the value of inv 
  
    getInv <- function() inv
    
    ## create a list of the functions for use in the cacheSolve function
  
    list(set = set, get = get, setInv = setInv, getInv = getInv)

}


## cacheSolve retrieves the cached inverse of the matrix calculated in
## makeCacheMatrix, if present. Otherwise, it finds the value of the input 
## matrix and solves it.

cacheSolve <- function(x, ...) {
  
    ## Retrieve the value of inv
  
    inv <- x$getInv()
    
    ## If inv exists in the cache, print a message and return inv
    
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
    ## If inv does not exist in the cache, calculate and return 
    ## the inverse matrix
    
    data <- x$get()
    
    inv <- solve(data, ...)
    
    x$setInv(inv)
    
    inv
  
}
