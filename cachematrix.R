# These functions satisfy Programmng Assignment 2 in the R Programming Course

# makeCacheMatrix() creates a list of functions that
# store two matrices in a shared environment. 
# One matrix is an input matrix and the other is 
# matrix is computed.The computed maatrix is the inverse of the input matrix. 
# Once computed the inverse matrix is stored (cached) in an environment shared
# by the functions in the list that makeCacheMatrix() returns.
# The list returned consists o functions to get and set the value of
# the input matrix and functioins to get and set the value of
# the inverse. If an inverse has not been computed then the function that gets
# the inverse retruns NULL.Otherwise it returns the previously computed inverse.
makeCacheMatrix <- function(inputMatrix = matrix()) {
  #Set the initial value of the cached inverse to NULL
  cachedInverse <- NULL
  setMatrix <- function(y) {
    # Stores a new value for input matrix in
    # in the surrounding environment
    inputMatrix <<- y
    # Since there is a new input matrix the 
    # inverse has not yet been cached. 
    # So set the cached inverse to NULL
    # in the surrounding environment
    cachedInverse <<- NULL
  }
  
  getMatrix <- function() {
    # Returns the input matrix
    # stored in a function closure
    inputMatrix
  } 
  
  setInverse <- function(i)  {
    # Set new value of cached inverse
    cachedInverse <<- i
  }
  
  getInverse <- function() {
    # Returns the value of the cached inverse
    # if it has been set. Otherwise it will
    # return NULL
    cachedInverse
  }
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


# cacheSolve() takes as input the result of invoking makeCacheSolve() on a square 
# matrix. It first attempts to get the inverse using the getInverse() function in
# the list of functions returend by makeCacheMatric(). If it fails to get a value,
# i.e the return value of getInverse() is NULL then it uses getMatrix() to retrieve
# the input matrix, inverts it, and cahces the result using the setInverse() function
# in the list returned by makeCacheMatrix(). If cacheSolve() is called again
# then it will retireve the previously computed matrix. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(is.null(inverse)) {
    # If the inverse has not been computed then
    # do it now. Print a message 
    # explaining that the inverse is being computed fresh.
    message("Computing and cachiing new inverse.")
    inverse <- solve(x$getMatrix())
    # Cache the newly computed inverse.
    x$setInverse(inverse)
  }
  else {
    # Print a message explaining that a
    # cached version of the inverse already
    # exists and is being retrieved and not computed.
    message("Returning cached inverse without computing it.")
  }
  # Which ever branch we took the variable 'inverse' must contain
  # the nverse of the input matrix since it is assumed that the
  # input matrix is invertible. So return it.
  inverse
}

