## cachematrix.R
##
## Provides a pair of functions, as follows:
##
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache both a matrix
## and that matrix's inverse.
##
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the cached matrix has not changed),
## then cacheSolve simply retrieves the inverse from the cache.


## makeCacheMatrix function
##
## Returns an object that has cached a copy of its matrix argument (i.e., this.matrix);
## as such, this function is a constructor.
##
## The returned object is packaged as a list of four (4) named setter and getter methods:
##
## i. get returns the cached matrix;
## this may be the 1x1 empty matrix produced by matrix().
##
## ii. set changes the cached matrix to the value of the setter's argument;
## in doing so, the object's cached inverse matrix (see iii. and iv. below) is (re)set to NULL.
##
## iii. setInverse stores the (presumed) inverse of the cached matrix;
## no check is performed to ensure that setInverse's argument is in fact the inverse of the cached matrix.
##
## iv. getInverse returns the object's cached inverse;
## NULL is returned if there is no cached inverse.
##
## ALTERNATE DESIGN -- A (highly) preferred approach would be to encapsulate the funcationality of this
## file's cacheSolve function inside makeCacheMatrix.  Doing so would eliminate the setInverse method and
## likewise ensure that the result of a call to getInverse is indeed the inverse of get's return value.

makeCacheMatrix <- function(this.matrix = matrix()) {

  this.inverse <- NULL
  
  set <- function(m) {
    this.matrix <<- m
    this.inverse <<- NULL
  }
  get <- function() this.matrix
  
  setInverse <- function(i) this.inverse <<- i
  getInverse <- function() this.inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## cacheSolve function
##
## Returns a matrix that is the inverse of the argument cachedMatrix, the central assumption being that
## cachedMatrix is an object (of the type) produced by the function makeCacheMatrix.
##
## Simply returns the previously-cached inverse, if it is available.  Otherwise computes the inverse,
## caches it in cachedMatrix, and returns the computed inverse.
##
## ... supplies optional arguments to be passed along to the solve function in case it is called,
## which occurs IFF the requested inverse needs to be computed.

cacheSolve <- function(cachedMatrix, ...) {

  this.inverse <- cachedMatrix$getInverse()
  
  if ( !is.null(this.inverse) ) {
    message('returning the cached inverse')
    return(this.inverse)
  }
  
  message('computing, caching and returning the inverse')
  
  this.inverse <- solve(cachedMatrix$get(), ...)
  
  cachedMatrix$setInverse(this.inverse)
  
  this.inverse
  
}
