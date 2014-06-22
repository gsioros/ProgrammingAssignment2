## The source file contains two functions for storing and retreiving a square invertable matrix and its inverse
## It uses solve() to caluclate the inverse of matrix
## The inverse matrix is stored so that saubsequent calls do not repeat the calulcations but retreive the stored result

## usage:
## CMtrx <-  makeCacheMatrix( mtrx)
## cacheSolve(CMtrx)

## makeCacheMatrix():
## returns an object that contains a matrix and its inverse 
## as well as methods for geting and seting them


makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL  ## creates the variable that will hold the inverted matrix
      set <- function(y) {    ## this is the set function
            x <<- y           ## the argument of the set function is going to be copied as the value of
            inv <<- NULL      ##  x (in the evniroment of the makeCacheMatrix called previously)
      }
      get <- function() x
      
      ## similar set function but for the inverted matrix stored in the
      ##  evniroment of the makeCacheMatrix called previously
      setInverse <- function(invert) inv <<- invert
      getInverse <- function() inv
      
      ## returns a list with the 4 methods for seting and getting data in the environment created with a call to makeCacheMatrix 
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)

}


## cacheSolve():
## calculates the inverse of the matrix stored in the special matrix object returned by makeCacheMatrix
## if the invese has already been calculated with a previous call to cacheSolve() then the stored invese matrix is returned

cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      if(!is.null(inv)) {
            message("getting cached matrix")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setInverse(inv)
      inv
        ## Return a matrix that is the inverse of 'x'
}
