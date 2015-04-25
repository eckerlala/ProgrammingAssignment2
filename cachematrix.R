## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object and is able to cache its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  myDimensions <- dim(x)
  
  ## check if it is really a quadratic matrix
  if (length(myDimensions)!=2) {
    message("this object is not a matrix")
    return(NULL)
  }
  
  ## set-function in order to set the matrix object
  set <- function(y) {
    x <<- y   
    
    inverse <<- NULL
  }
  
  ##get function in order to get the matrix object
  get <- function() x
  
  ## set function in order to set the inverse matrix
  setInverse <- function(x_inv) inverse <<- x_inv
  
  ## get function in order to get the inverse matrix
  getInverse <- function() inverse
  
  ##returning a list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function calculates the inverse matrix if the inverse is not already cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## gets the cached inverse matrix
  inverse <- x$getInverse()
  
  if(!is.null(inverse) ) {
    message("getting cached inverse matrix")
    return(inverse)
  }
  ## if there is no cached inverse matrix it will be calculated now
  data <- x$get()
  inverse <- solve(data,diag(1,dim(data)[1],dim(data)[2]))
  x$setInverse(inverse)
  inverse
}
