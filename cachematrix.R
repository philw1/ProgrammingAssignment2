#
# makeCacheMatrix - Creates a list of handler functions to  cache a matrix inverse
#                   Takes a square matrix as parameter
# cacheSolve - Returns a cached matrix inverse
#              Takes a list of handler functions from makeCacheMatrix
#

# Returns a list of functions to store and retrieve a cached matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  setmat <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  getmat <- function() x
  setmatinv <- function(inv) matinv <<- inv
  getmatinv <- function() matinv
  
  list(setmat=setmat, getmat=getmat, setmatinv=setmatinv, getmatinv=getmatinv)
}


# Returns the inverse of matrix from cache or recalculates 
cacheSolve <- function(x, ...) {
  matinv <- x$getmatinv()
  if (!is.null(matinv)) {
    message("getting cached inverse")
    return(matinv)
  }
  temp <- x$getmat()
  matinv <- solve(temp)
  x$setmatinv(matinv)
  matinv
}
