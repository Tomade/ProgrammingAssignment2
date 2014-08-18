## This file contains a simple implementation of an augmented matrix type. It 
## caches the value of its inverse the first time it is requested, so to return 
## it directly on each following request instead of recalculating it.  In code 
## using large matrix objects for which the inverses are often required,
## avoiding such recalculation yields significant performance increase, at the
## expense of double the memory storage.
## 
## Two functions are provided:
## 1) makeCacheMatrix(x <- matrix())
##    Takes a regular matrix in input and returns an augmented instance.
## 2) cacheSolve(x,...)
##    Takes an augmented matrix x previously obtained with "makeCacheMatrix()"
##    and returns its inverse as a regular matrix object.


# makeCacheMatrix(x <- matrix())
# Takes a regular matrix in input and returns an augmented instance.
# The instance really is a list of 4 closures, all holding the original
# matrix value as stored in the environment of this function.
# The function defines the four closures and returns them in a list.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


# cacheSolve(x,...)
# Takes an augmented matrix x previously obtained with "makeCacheMatrix()" and
# returns its inverse as a regular matrix object.
#
# This function assumes that the passed object is a list of four closures (set, 
# get, setinverse, getinverse) created with a call to "makeCacheMatrix()". It 
# calls "x$getinverse()" and checks if the return value is null, in which case it 
# first performs the actual inversion with solve() on the value returned by
# "x$get()" and stores the result into the augmented matrix with
# "x$setinverse()", before returning it to the caller.
#
# The function does no error checking and assumes the matrix to be invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
