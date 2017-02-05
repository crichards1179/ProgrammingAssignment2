## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
      x <<- y
      i <<- NULL
  }
  get <- function() x
  setInv <- function(inv) inv <<- inv
  getInv <- function() i
  list(set = set,
    get = get,
    setInv = setInv,
    getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned 
##      by makeCacheMatrix above. If the inverse has already been calculated 
##      (and the matrix has not changed), then cacheSolve should retrieve the 
##      inverse from the cache.

cacheSolve <- function(x, ...) {
        
        inv <- x$getInv()
        # If inverse of matrix has already been calculated, return it.
        if (!is.null(inv)) {
                return(inv)
        }
        # Otherwise, get the matrix, solve it for it's inverse and set it in
        #   the matrix object.
        matx <- x$get()
        inv <- solve(matx, ...)
        x$setInv(inv)
        
        ## Return the matrix object with the inverse of 'x'
        inv
}
