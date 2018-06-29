## r20diw week 3 homework submission

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than computing it repeatedly
# These functions are part of the lessons on lexical scoping in R

## creates a special "matrix" object that can cache its inverse
# function is based on makeVector example

makeCacheMatrix <- function(x = matrix()) {
  
  s <- NULL #flag for whether inverse has been calculated
  
  set <- function(y) {
    x <<- y
    s <<- NULL #flag for calculated matrix inverse set to null
  }
  get <- function() x #function to return input arg
  
  setInverse <-function(solve) s <<-solve #function to find inverse using solve
  # when solve() is passed only 1st arg ( matrix object, a), it returns inverse
  # of a
  getInverse <-function() s #function to retrieve inverse result from memory
  
  # create a list containg functions for set, get, setInverse, getInverse for matrix object
  
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
  
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from the
## cache.
# function is based on cacheMean example

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  
  s <-x$getInverse()
  if(!is.null(s)){  #check flag to see if inverse has been calculated previously
    message("Inverse of matrix was calculated previously")
    message("getting cached data")
    return(s)
  }
  
  data<- x$get() #retrieve stored matrix
  s <- solve(data,...) # calcuate inverse
  x$setInverse(s)
  s
  
}




