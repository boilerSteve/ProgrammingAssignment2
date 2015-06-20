## We will write a function to cache the inverse of a matrix, and produce the 
## cached value rather than re-calculating.

## First, we're going to create a list of functions that store or return
## a matrix 'x', and return a cached inverse of the matrix, creating
## the cached inverse if it doesn't exist.  Syntax is a little funky here:
## 'x' is a parameter to the 'makeCacheMatrix' function, but it is actually
## treated like a stored value in a structure.  We could make it explicit by adding
## something like 'value <- x' to the outermost function, then using 'value' as
## the thing that the inner functions modify.
##
## The instructions suggest we don't want to re-solve the matrix if it hasn't
## changed.  
## " If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache. "
## I suspect they meant simply to make sure the 'set' function invalidates the inverse
## whenever it's called to modify 'x', but I figured it's possible (I didn't do any profiling to test)
## that for big matrices, checking for equality in the 'set' function is worthwhile, so 
## I did that.

makeCacheMatrix <- function(x = matrix()) {
# we start with no inverse
  inv <- NULL

# 'set' will store the value of the input matrix as the 
# formal parameter 'x' to the outer function.
# note that 'set' invalidates the inverse when it  
# modifies 'x'
# Note also that we first check for equality here; if we try to 
# set a new value that is the same as  the old one, we just leave
# everything alone.
  set <- function (y) {
    if (all (x == y)){
      return()
    }
    x <<- y
    inv <<- NULL
  }

# 'get' does nothing but retrieve the stored value of the 
# matrix itself
  get <- function () x

# 'setinv' will simply take a matrix 'inv' and store it into 
# 'inv' in the enclosing environment.  
# It doesn't check for validity, and is accessible from any 
# environment so we need to be careful not to call it except with the
# inverse of the matrix we want it associated with.
# As noted in discussions, this probably isn't a good design overall.
  setinv <-function (y) inv <<- y

# And we read back the inverse by simply returning 'inv' from the outer frame
  getinv <- function () inv

# 'makeCacheMatrix returns a list of these four functions, which also encapsulates
# the values 'x', and 'inv' (the original matrix and its inverse for this instance)
  return (list (set = set, get = get, setinv = setinv, getinv = getinv))
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
# We get the cached inverse, then if it is NULL, we 
# calculate the inverse from the data, and cache it.
# then we return either the cached or newly created value.
  inv <- x$getinv()
  if (is.null (inv)){
    inv <- solve (x$get ())
    x$setinv (inv)
  }
  return (inv)
}
