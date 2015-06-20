##Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the 
## inverse of a matrix rather than compute it 
## repeatedly. 

## This file contains 2 functions: makeCacheMatrix and cacheSolve

# This function creates a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse matrix
## 4.get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, 
  setinverse=setinverse, getinverse=getinverse)
  ## Return the a list of four functions
}


## This function computes the inverse of the special
##"matrix" returned by makeCacheMatrix above. If the
## inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("GETTING CACHED DATA...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv      
  ## Return a matrix that is the inverse of 'x'
}
